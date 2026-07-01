#
# Natural Gas Bill Analysis - Shiny App
# Reads billing data directly from a CSV hosted on GitHub
#

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(pdftools)
library(stringr)

# ---- 1. SET YOUR GITHUB RAW CSV URL HERE ----
csv_url <- "https://raw.githubusercontent.com/atticus1111/Data-app-for-BHP/refs/heads/main/Book1.csv"

# ---- 2. Load + reshape the data ----
load_gas_data <- function(url) {
  raw <- read_csv(url, show_col_types = FALSE)

  # --- Diagnostics: print what was actually loaded ---
  cat("\n--- DEBUG: column names found ---\n")
  print(names(raw))
  cat("--- DEBUG: first 3 rows ---\n")
  print(head(raw, 3))

  # Drop fully empty / unnamed trailing columns (your file has a few blank ones)
  raw <- raw %>% select(where(~ !all(is.na(.))))
  raw <- raw[, !grepl("^\\.\\.\\.|^$|^NA$", names(raw))]

  # Find the description column robustly (case/whitespace-insensitive)
  desc_col <- names(raw)[trimws(tolower(names(raw))) == "description"]
  if (length(desc_col) == 0) {
    stop(
      "Could not find a 'Description' column. Columns found were: ",
      paste(names(raw), collapse = ", ")
    )
  }
  desc_col <- desc_col[1]
  names(raw)[names(raw) == desc_col] <- "Description"

  # Identify date columns (everything except "Description")
  date_cols <- setdiff(names(raw), "Description")

  # Pivot to long format: one row per Description per billing date
  long_data <- raw %>%
    pivot_longer(
      cols = all_of(date_cols),
      names_to = "bill_date",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      bill_date = mdy(trimws(bill_date)),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    filter(!is.na(bill_date), !is.na(value))

  if (nrow(long_data) == 0) {
    stop("Data loaded but ended up empty after cleaning. Check the DEBUG output above for clues.")
  }

  long_data
}

gas_data <- load_gas_data(csv_url)

# Keep a separate, growable copy in memory that the PDF upload feature
# appends to. This does NOT touch your GitHub file -- it only lives in
# the running app session. Download the updated CSV using the button
# in the "Upload PDF bill" tab to save your changes permanently.
gas_data_full <- gas_data

# ---- 2b. PDF bill parser ----
# Charge labels we look for in the PDF, mapped to the same
# "Description" names used in your CSV. Add more lines here if your
# bills include charges not yet covered. Patterns are matched against
# words on the page joined back together line by line.
charge_patterns <- c(
  "Service & Facility"     = "^Service\\s*&\\s*Facility$",
  "Usage Charge"           = "^Usage Charge$",
  "Capacity Charge"        = "^Capacity Charge$",
  "Natural Gas Q1"         = "^Natural Gas Q1$",
  "Natural Gas Q2"         = "^Natural Gas Q2$",
  "Natural Gas Q3"         = "^Natural Gas Q3$",
  "Natural Gas Q4"         = "^Natural Gas Q4$",
  "Demand Side Mgmt"       = "^Demand Side Mgmt$",
  "Interstate Pipeline"    = "^Interstate Pipeline$",
  "GRSA"                   = "^GRSA$",
  "Energy Assistance Chg"  = "^Energy Assistance",
  "Subtotal"               = "^Subtotal$",
  "Franchise Fee"          = "^Franchise Fee$",
  "Climate Tax"            = "^Climate Tax$",
  "Sales Tax"              = "^Sales Tax$",
  "Total"                  = "^Total$",
  "Premises Total"         = "^Premises Total$"
)

# Groups words from pdf_data() into visual lines by rounding their
# y-position (words on the same printed line share ~the same y).
group_words_into_lines <- function(page_data, y_tolerance = 3) {
  page_data <- page_data %>% arrange(y, x)
  page_data$line_id <- cumsum(c(1, diff(page_data$y) > y_tolerance))
  page_data %>%
    group_by(line_id) %>%
    summarise(
      text = paste(text, collapse = " "),
      x_min = min(x),
      y = min(y),
      .groups = "drop"
    ) %>%
    arrange(y, x_min)
}

# Extracts the billing/read end date from text like
# "Read Dates: 03/04/25 - 04/02/25" -> uses the second (end) date.
extract_bill_date <- function(all_text) {
  dates <- str_extract_all(all_text, "\\d{1,2}/\\d{1,2}/\\d{2,4}")[[1]]
  if (length(dates) == 0) return(NA)
  parsed <- suppressWarnings(mdy(dates))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) return(NA)
  max(parsed, na.rm = TRUE)
}

# Main entry point: takes a path to an uploaded PDF, returns a tidy
# data frame in the same long format as the CSV (Description, bill_date, value).
# Uses word coordinates so labels and charges in the same row are paired
# correctly even when columns (like USAGE UNITS, RATE, CHARGE) sit between them.
parse_gas_pdf <- function(pdf_path) {
  pages <- pdf_data(pdf_path)

  bill_date <- extract_bill_date(paste(pdf_text(pdf_path), collapse = " "))

  all_lines <- bind_rows(lapply(pages, group_words_into_lines))

  results <- lapply(names(charge_patterns), function(desc) {
    pattern <- charge_patterns[[desc]]

    # Find lines whose leading text (before the numeric columns) matches the label.
    # Allows digits in the label itself (e.g. "Natural Gas Q1") by stopping at the
    # first number that's followed by a space+digit or space+$ (start of data columns).
    label_part <- all_lines %>%
      mutate(label_only = str_trim(str_extract(text, "^[A-Za-z0-9 &.,]+?(?=\\s+[\\d$])"))) %>%
      filter(str_detect(label_only, regex(pattern, ignore_case = TRUE)))

    if (nrow(label_part) == 0) return(data.frame(Description = desc, bill_date = bill_date, value = NA_real_))

    row_text <- label_part$text[1]

    # The CHARGE column is the right-most dollar amount on that same row
    amounts <- str_extract_all(row_text, "-?\\d[\\d,]*\\.\\d{2}")[[1]]
    amt <- if (length(amounts) == 0) NA_real_ else as.numeric(gsub(",", "", amounts[length(amounts)]))

    data.frame(Description = desc, bill_date = bill_date, value = amt)
  })

  out <- bind_rows(results) %>% filter(!is.na(value))

  # Sanity check: Total should roughly equal Subtotal + Franchise Fee +
  # Climate Tax + Sales Tax. If it doesn't, the "Total" row was likely
  # mis-grouped (e.g. merged with a neighboring "Premises Total" row) --
  # in that case prefer "Premises Total" if we found one, since it tends
  # to sit on its own line with more vertical space around it.
  get_val <- function(d) {
    v <- out$value[out$Description == d]
    if (length(v) == 0) NA_real_ else v[1]
  }
  expected_total <- sum(get_val("Subtotal"), get_val("Franchise Fee"),
                         get_val("Climate Tax"), get_val("Sales Tax"), na.rm = TRUE)
  actual_total <- get_val("Total")
  premises_total <- get_val("Premises Total")

  if (!is.na(expected_total) && !is.na(actual_total) &&
      abs(actual_total - expected_total) > 1 && !is.na(premises_total)) {
    out$value[out$Description == "Total"] <- premises_total
  }

  # Drop the helper row -- it's the same figure as Total, no need to keep both
  out <- out %>% filter(Description != "Premises Total")

  out
}

# Charge categories (rows) available for selection
charge_types <- gas_data %>%
  filter(!Description %in% c("Usage Units", "Capacity Units")) %>%
  distinct(Description) %>%
  pull(Description)

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(version = 5),

  titlePanel("Natural Gas Bill Analysis"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "charges",
        "Select charge type(s):",
        choices = charge_types,
        selected = c("Total", "Usage Charge", "Capacity Charge"),
        multiple = TRUE
      ),
      dateRangeInput(
        "date_range",
        "Date range:",
        start = min(gas_data$bill_date, na.rm = TRUE),
        end = max(gas_data$bill_date, na.rm = TRUE)
      ),
      fileInput(
        "pdf_upload",
        "Upload a bill PDF:",
        accept = ".pdf"
      ),
      helpText("Extracted charges can be previewed and added to your data in the 'Upload PDF bill' tab.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Trends",
          plotOutput("trendPlot", height = "400px")
        ),
        tabPanel(
          "Summary",
          tableOutput("summaryTable")
        ),
        tabPanel(
          "Raw data",
          tableOutput("rawTable")
        ),
        tabPanel(
          "Upload PDF bill",
          br(),
          h4("Extracted from PDF"),
          tableOutput("pdfPreview"),
          br(),
          actionButton("add_pdf_data", "Add this bill to my data"),
          downloadButton("download_data", "Download updated CSV"),
          br(), br(),
          textOutput("pdf_status")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Reactive store for the full dataset (CSV data + any appended PDF data)
  data_store <- reactiveVal(gas_data_full)

  # Parse the uploaded PDF whenever a new file comes in
  pdf_extracted <- reactive({
    req(input$pdf_upload)
    parse_gas_pdf(input$pdf_upload$datapath)
  })

  output$pdfPreview <- renderTable({
    df <- pdf_extracted()
    validate(need(nrow(df) > 0, "No charges could be read from this PDF. Try a different file, or check pdf_status for details."))
    df %>% mutate(bill_date = format(bill_date, "%m/%d/%Y"))
  })

  output$pdf_status <- renderText({
    if (is.null(input$pdf_upload)) return("")
    df <- pdf_extracted()
    if (nrow(df) == 0) {
      "Nothing was extracted. This PDF's layout may differ from what the parser expects -- let me know and I can adjust the patterns."
    } else {
      paste0("Found ", nrow(df), " charge line(s) for bill date ", format(unique(df$bill_date), "%m/%d/%Y"), ".")
    }
  })

  # Append extracted data to the in-memory dataset
  observeEvent(input$add_pdf_data, {
    new_rows <- pdf_extracted()
    if (nrow(new_rows) == 0) {
      showNotification("Nothing to add -- no charges were extracted from this PDF.", type = "warning")
      return()
    }
    updated <- bind_rows(data_store(), new_rows) %>%
      distinct(Description, bill_date, .keep_all = TRUE) %>%
      arrange(bill_date, Description)
    data_store(updated)

    # Refresh the charge-type choices in case the PDF introduced a new label
    updateSelectInput(
      session, "charges",
      choices = unique(updated$Description[!updated$Description %in% c("Usage Units", "Capacity Units")]),
      selected = input$charges
    )

    showNotification("Bill added to your data. Use 'Download updated CSV' to save it.", type = "message")
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("gas_data_updated_", Sys.Date(), ".csv"),
    content = function(file) {
      out <- data_store() %>%
        pivot_wider(names_from = bill_date, values_from = value)
      write_csv(out, file)
    }
  )

  filtered_data <- reactive({
    data_store() %>%
      filter(
        Description %in% input$charges,
        bill_date >= input$date_range[1],
        bill_date <= input$date_range[2]
      )
  })

  output$trendPlot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data for the selected charges/date range."))

    ggplot(df, aes(x = bill_date, y = value, color = Description)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        x = "Billing date",
        y = "Amount ($)",
        color = "Charge type",
        title = "Charges over time"
      ) +
      theme_minimal(base_size = 14)
  })

  output$summaryTable <- renderTable({
    df <- filtered_data()
    df %>%
      group_by(Description) %>%
      summarise(
        Min = round(min(value, na.rm = TRUE), 2),
        Max = round(max(value, na.rm = TRUE), 2),
        Average = round(mean(value, na.rm = TRUE), 2),
        Total = round(sum(value, na.rm = TRUE), 2),
        .groups = "drop"
      )
  })

  output$rawTable <- renderTable({
    filtered_data() %>%
      arrange(bill_date, Description) %>%
      mutate(bill_date = format(bill_date, "%m/%d/%Y"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
