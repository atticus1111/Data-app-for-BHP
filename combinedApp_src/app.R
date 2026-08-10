#
# Utility Bill Analysis + Electrification Planner
# Tracks natural gas AND electricity bills, models electrification impact
#
# CHANGES IN THIS VERSION:
#   1. No more hardcoded GitHub CSV URLs. Each user pastes their own
#      link(s) into the sidebar after launching the app, so multiple
#      properties/users can share this same app without seeing each
#      other's data. Links are optional -- you can skip straight to
#      uploading PDFs instead.
#   2. PDF upload now accepts multiple files at once.
#   3. New "Download Excel (all data)" button exports everything
#      collected in the session (from CSV links and/or PDFs) as a
#      single .xlsx workbook with gas/electric, wide/long sheets.
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
library(scales)
library(writexl)

# ============================================================
# 1. DATA LOADING
# ============================================================
empty_bill_df <- function() {
  data.frame(Description = character(), bill_date = as.Date(character()), value = numeric())
}

# Combine existing + new bill rows, keyed by Description + billing month.
# If new data has a row for a month/description that already exists (e.g. a
# second PDF for the same period with a slightly different exact date), the
# newer value replaces the old one rather than creating a duplicate.
merge_bill_data <- function(old, new) {
  bind_rows(old, new) %>%
    group_by(Description, bill_date) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    arrange(bill_date)
}

# url is optional -- if blank/NULL, just returns an empty data frame
load_bill_data <- function(url, label = "data") {
  if (is.null(url) || !nzchar(trimws(url))) return(empty_bill_df())
  tryCatch({
    raw <- read_csv(url, show_col_types = FALSE)
    raw <- raw %>% select(where(~ !all(is.na(.))))
    raw <- raw[, !grepl("^\\.\\.\\.+|^$|^NA$", names(raw))]

    desc_col <- names(raw)[trimws(tolower(names(raw))) == "description"]
    if (length(desc_col) == 0) stop("No 'Description' column found in ", label)
    names(raw)[names(raw) == desc_col[1]] <- "Description"

    date_cols <- setdiff(names(raw), "Description")
    raw %>%
      pivot_longer(cols = all_of(date_cols), names_to = "bill_date", values_to = "value") %>%
      filter(!is.na(value)) %>%
      mutate(
        bill_date = mdy(trimws(bill_date)),
        value     = suppressWarnings(as.numeric(value))
      ) %>%
      filter(!is.na(bill_date), !is.na(value)) # %>%
      # Bills are grouped by billing month -- different exact dates within
      # the same month (e.g. 3/14 vs 3/17) represent the same billing period
     # mutate(bill_date = floor_date(bill_date, "month"))
  }, error = function(e) {
    message("Could not load ", label, ": ", e$message)
    empty_bill_df()
  })
}

# ============================================================
# 2. PDF PARSERS
# ============================================================
group_words_into_lines <- function(page_data, y_tolerance = 3) {
  page_data <- page_data %>% arrange(y, x)
  page_data$line_id <- cumsum(c(1, diff(page_data$y) > y_tolerance))
  page_data %>%
    group_by(line_id) %>%
    summarise(text = paste(text, collapse = " "), x_min = min(x), y = min(y), .groups = "drop") %>%
    arrange(y, x_min)
}

extract_bill_date <- function(all_text) {
  # Prefer the labeled "Statement Date" -- this is the bill's actual
  # billing-period date. Falling back to "the latest date anywhere in the
  # text" is unreliable because the Due Date (usually ~3 weeks later, and
  # sometimes in the following calendar month) is almost always later and
  # would otherwise get picked instead, misfiling the bill under the wrong month.
  # NOTE: uses [\s\S] (any character) rather than \D in the gap, because on
  # some layouts the Statement Number (a run of digits) sits between the
  # "Statement Date" label and the actual date, e.g.:
  #   STATEMENT NUMBER  STATEMENT DATE  AMOUNT DUE
  #   922945250         04/11/2025      $56,505.54
  # \D can't skip over "922945250", which silently broke this match before.
  m <- str_match(all_text, regex("STATEMENT DATE[\\s\\S]{0,40}?(\\d{1,2}/\\d{1,2}/\\d{2,4})", ignore_case = TRUE))
  if (!is.na(m[1, 2])) {
    parsed <- suppressWarnings(mdy(m[1, 2]))
    if (!is.na(parsed)) return(parsed)
  }

  # Fallback: no "Statement Date" label found -- use the earliest date in
  # the document instead of the latest, since the due date (later) is far
  # more likely to appear than an earlier, unlabeled statement date.
  dates  <- str_extract_all(all_text, "\\d{1,2}/\\d{1,2}/\\d{2,4}")[[1]]
  parsed <- suppressWarnings(mdy(dates))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) return(NA_real_)
  min(parsed, na.rm = TRUE)
}

# Pulls the statement's overall "Amount Due" (the cover-page total, e.g.
# "$20,367.37"). NOTE: this can include a carried-over past-due balance
# and late fees on top of the actual utility charges for the period --
# use extract_current_charges() below when you specifically want just
# this period's charges.
extract_amount_due <- function(all_text) {
  m <- str_match(all_text, regex("AMOUNT DUE[\\s\\S]{0,100}?\\$?\\s*(-?[\\d,]+\\.\\d{2})", ignore_case = TRUE))
  if (is.na(m[1, 2])) return(NA_real_)
  as.numeric(gsub(",", "", m[1, 2]))
}

# Pulls "Current Charges" -- this period's actual utility charges, with
# any Balance Forward (past-due amount carried over from prior bills) and
# late fees excluded. This is what should be used to back into the
# electricity total, since Amount Due can be inflated by unpaid balances.
extract_current_charges <- function(all_text) {
  m <- str_match(all_text, regex("CURRENT CHARGES[\\s\\S]{0,60}?\\$?\\s*(-?[\\d,]+\\.\\d{2})", ignore_case = TRUE))
  if (is.na(m[1, 2])) return(NA_real_)
  as.numeric(gsub(",", "", m[1, 2]))
}

extract_charges <- function(all_lines, patterns) {
  lapply(names(patterns), function(desc) {
    pat <- patterns[[desc]]
    matched <- all_lines %>%
      mutate(label_only = str_trim(str_extract(text, "^[A-Za-z0-9 &.,]+?(?=\\s+[\\d$])"))) %>%
      filter(str_detect(label_only, regex(pat, ignore_case = TRUE)))
    if (nrow(matched) == 0) return(data.frame(Description = desc, value = NA_real_))
    # Statements that cover multiple premises repeat the same charge
    # categories once per premises section (e.g. "Distribution Demand"
    # appears on each premises' electricity page). Sum every matching
    # line rather than only using the first one, so multi-premises bills
    # get the correct combined total for each charge.
    vals <- vapply(matched$text, function(t) {
      amounts <- str_extract_all(t, "-?\\d[\\d,]*\\.\\d{2}")[[1]]
      if (length(amounts) == 0) return(NA_real_)
      as.numeric(gsub(",", "", amounts[length(amounts)]))
    }, numeric(1), USE.NAMES = FALSE)
    vals <- vals[!is.na(vals)]
    amt <- if (length(vals) == 0) NA_real_ else sum(vals)
    data.frame(Description = desc, value = amt)
  }) %>% bind_rows() %>% filter(!is.na(value))
}

# --- Gas charge patterns ---
gas_patterns <- c(
  "Service & Facility"    = "^Service\\s*&\\s*Facility$",
  "Usage Charge"          = "^Usage Charge$",
  "Capacity Charge"       = "^Capacity Charge$",
  "Natural Gas Q1"        = "^Natural Gas Q1$",
  "Natural Gas Q2"        = "^Natural Gas Q2$",
  "Natural Gas Q3"        = "^Natural Gas Q3$",
  "Natural Gas Q4"        = "^Natural Gas Q4$",
  "Demand Side Mgmt"      = "^Demand Side Mgmt$",
  "Interstate Pipeline"   = "^Interstate Pipeline$",
  "GRSA"                  = "^GRSA$",
  "Energy Assistance Chg" = "^Energy Assistance",
  "Subtotal"              = "^Subtotal$",
  "Franchise Fee"         = "^Franchise Fee$",
  "Climate Tax"           = "^Climate Tax$",
  "Sales Tax"             = "^Sales Tax$",
  "Total"                 = "^Total$",
  "Premises Total"        = "^Premises Total$"
)

# --- Electricity charge patterns ---
elec_patterns <- c(
  "Elec Service & Facility"    = "^Service\\s*&\\s*Facility$",
  "Secondary General"          = "^Secondary General$",
  "ECA Q1"                     = "^ECA Q1$",
  "ECA Q2"                     = "^ECA Q2$",
  "ECA Q3"                     = "^ECA Q3$",
  "ECA Q4"                     = "^ECA Q4$",
  "Distribution Demand"        = "^Distribution Demand$",
  "Gen & Transm Demand"        = "^Gen\\s*&\\s*Transm Demand$",
  "Trans Cost Adj"             = "^Trans Cost Adj$",
  "Purch Cap Cost Adj"         = "^Purch Cap Cost Adj$",
  "Trans Elec Plan"            = "^Trans Elec Plan$",
  "Elec Demand Side Mgmt"      = "^Demand Side Mgmt$",
  "Renew. Energy Std Adj"      = "^Renew",
  "Colo Energy Plan Adj"       = "^Colo Energy Plan",
  "Clean Energy Plan Rev"      = "^Clean Energy Plan",
  "Elec Energy Assistance Chg" = "^Energy Assistance",
  "Elec Subtotal"              = "^Subtotal$",
  "Elec Franchise Fee"         = "^Franchise Fee$",
  "Elec Climate Tax"           = "^Climate Tax$",
  "Elec Sales Tax"             = "^Sales Tax$",
  "Elec Total"                 = "^Total$",
  "Elec Premises Total"        = "^Premises Total$"
)

# Demand-related electric charge names (used for strain modeling)
demand_charge_names <- c(
  "Distribution Demand", "Gen & Transm Demand", "Trans Cost Adj",
  "Purch Cap Cost Adj", "Trans Elec Plan", "Elec Demand Side Mgmt"
)

# Sanity-checks and resolves the Total vs Premises Total ambiguity for one fuel
resolve_total <- function(out, fuel_type) {
  get_val <- function(d) { v <- out$value[out$Description == d]; if(length(v)==0) NA_real_ else v[1] }
  if (fuel_type == "gas") {
    tk="Total"; pk="Premises Total"; sk="Subtotal"; fk="Franchise Fee"; ck="Climate Tax"; stk="Sales Tax"
  } else {
    tk="Elec Total"; pk="Elec Premises Total"; sk="Elec Subtotal"
    fk="Elec Franchise Fee"; ck="Elec Climate Tax"; stk="Elec Sales Tax"
  }
  expected <- sum(get_val(sk), get_val(fk), get_val(ck), get_val(stk), na.rm=TRUE)
  actual   <- get_val(tk)
  premises <- get_val(pk)
  if (!is.na(expected) && !is.na(actual) && abs(actual - expected) > 1 && !is.na(premises))
    out$value[out$Description == tk] <- premises
  out %>% filter(Description != pk)
}

# Main combined parser -- detects which pages belong to gas vs electricity
# by scanning for section headers, then applies the right pattern set.
# Works whether sections are on the same page or different pages.
parse_combined_pdf <- function(pdf_path) {
  pages     <- pdf_data(pdf_path)
  all_text  <- paste(pdf_text(pdf_path), collapse = " ")
  bill_date <- extract_bill_date(all_text)
  # Group by billing month -- a bill dated 3/14 and one dated 3/17 are
  # treated as the same billing period
  if (!is.na(bill_date)) bill_date <- floor_date(bill_date, "month")
  # Prefer "Current Charges" -- this excludes any carried-over past-due
  # balance and late fees, which "Amount Due" does NOT exclude and which
  # would otherwise inflate the computed electricity total on overdue bills.
  current_charges <- extract_current_charges(all_text)
  amount_due <- extract_amount_due(all_text)
  period_total <- if (!is.na(current_charges)) current_charges else amount_due

  gas_lines  <- list()
  elec_lines <- list()

  for (i in seq_along(pages)) {
    page_lines <- group_words_into_lines(pages[[i]])
    combined   <- paste(page_lines$text, collapse = " ")

    has_gas  <- str_detect(combined, regex("NATURAL GAS CHARGES",  ignore_case = TRUE))
    has_elec <- str_detect(combined, regex("ELECTRICITY CHARGES",  ignore_case = TRUE))

    if (has_gas && has_elec) {
      # Both sections on the same page -- split by y-position of each header
      gas_header_y  <- page_lines$y[str_detect(page_lines$text,
                                               regex("NATURAL GAS CHARGES",  ignore_case = TRUE))][1]
      elec_header_y <- page_lines$y[str_detect(page_lines$text,
                                               regex("ELECTRICITY CHARGES",  ignore_case = TRUE))][1]

      if (!is.na(gas_header_y) && !is.na(elec_header_y)) {
        if (gas_header_y < elec_header_y) {
          gas_lines[[length(gas_lines)+1]]  <- page_lines %>% filter(y >= gas_header_y,  y < elec_header_y)
          elec_lines[[length(elec_lines)+1]] <- page_lines %>% filter(y >= elec_header_y)
        } else {
          elec_lines[[length(elec_lines)+1]] <- page_lines %>% filter(y >= elec_header_y, y < gas_header_y)
          gas_lines[[length(gas_lines)+1]]   <- page_lines %>% filter(y >= gas_header_y)
        }
      }
    } else if (has_gas) {
      gas_lines[[length(gas_lines)+1]]  <- page_lines
    } else if (has_elec) {
      elec_lines[[length(elec_lines)+1]] <- page_lines
    }
    # Pages with neither header (cover page, payment stub, etc.) are skipped
  }

  results <- list()

  # Extract gas charges
  if (length(gas_lines) > 0) {
    gas_all <- bind_rows(gas_lines)
    gas_out  <- extract_charges(gas_all, gas_patterns) %>%
      mutate(bill_date = bill_date, fuel = "gas")
    gas_out  <- resolve_total(gas_out, "gas")
    results[["gas"]] <- gas_out
  }

  # Extract electricity charges
  if (length(elec_lines) > 0) {
    elec_all <- bind_rows(elec_lines)
    elec_out  <- extract_charges(elec_all, elec_patterns) %>%
      mutate(bill_date = bill_date, fuel = "electricity")
    elec_out  <- resolve_total(elec_out, "electricity")
    results[["electricity"]] <- elec_out
  }

  # Electricity Total is unreliable to parse directly off the statement, so
  # back into it from this period's Current Charges (falling back to
  # Amount Due if Current Charges wasn't found) minus the Gas Total --
  # this overrides whatever "Elec Total" line item was extracted.
  if (!is.na(period_total) && "electricity" %in% names(results)) {
    gas_total_val <- 0
    if ("gas" %in% names(results)) {
      gv <- results[["gas"]]$value[results[["gas"]]$Description == "Total"]
      if (length(gv) > 0 && !is.na(gv[1])) gas_total_val <- gv[1]
    }
    computed_elec_total <- period_total - gas_total_val

    elec_df <- results[["electricity"]]
    if ("Elec Total" %in% elec_df$Description) {
      elec_df$value[elec_df$Description == "Elec Total"] <- computed_elec_total
    } else {
      elec_df <- bind_rows(
        elec_df,
        data.frame(Description = "Elec Total", value = computed_elec_total,
                   bill_date = bill_date, fuel = "electricity")
      )
    }
    results[["electricity"]] <- elec_df
  }

  bind_rows(results)
}

# Parse one or more PDFs (as given by a Shiny fileInput data frame) and
# tag each extracted row with which source file it came from.
parse_multiple_pdfs <- function(files_df) {
  if (is.null(files_df) || nrow(files_df) == 0) return(data.frame())
  out <- lapply(seq_len(nrow(files_df)), function(i) {
    df <- tryCatch(parse_combined_pdf(files_df$datapath[i]),
                    error = function(e) {
                      message("Could not parse ", files_df$name[i], ": ", e$message)
                      data.frame()
                    })
    if (nrow(df) > 0) df$source_file <- files_df$name[i]
    df
  })
  bind_rows(out)
}

# ============================================================
# 3. ELECTRIFICATION STRAIN MODEL
# ============================================================
# Estimates the added monthly electric cost from converting gas heating
# to a heat pump, given:
#   - baseline_gas_dth  : average non-heating-season gas usage (Dth/month)
#   - heating_gas_dth   : extra gas usage in heating months (Dth/month)
#   - cop               : heat pump coefficient of performance (2-4 typical)
#   - current_peak_kw   : current electric peak demand (kW)
#   - demand_rate       : $/kW/month total demand charge rate
#   - kwh_rate          : blended $/kWh energy rate
#   - peak_adder_pct    : how much of heat pump load adds to peak demand (0-1)
electrification_model <- function(
    heating_gas_dth, cop, current_peak_kw,
    demand_rate, kwh_rate, peak_adder_pct = 0.5
) {
  # 1 Dth = 293.07 kWh (energy equivalent)
  heat_kwh_needed   <- heating_gas_dth * 293.07
  # Heat pump delivers same heat using less electricity due to COP
  heat_kwh_electric <- heat_kwh_needed / cop
  # New energy cost from heating load
  added_energy_cost <- heat_kwh_electric * kwh_rate
  # Peak demand increase: a fraction of heating load hits the monthly peak
  # (heat pumps run continuously, not all at the same moment)
  added_peak_kw     <- (heat_kwh_electric / 720) * peak_adder_pct * 10
  added_demand_cost <- added_peak_kw * demand_rate

  list(
    heat_kwh_electric = round(heat_kwh_electric, 1),
    added_energy_cost = round(added_energy_cost, 2),
    added_peak_kw     = round(added_peak_kw, 1),
    added_demand_cost = round(added_demand_cost, 2),
    total_added_cost  = round(added_energy_cost + added_demand_cost, 2)
  )
}

# ============================================================
# 4. UI
# ============================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  titlePanel("Utility Bill Analysis & Electrification Planner"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h5("Data source (optional)"),
      helpText("Paste your own gas/electric CSV links below, or skip this ",
               "and just upload PDFs. Links you paste here are only used ",
               "in your own browser session -- nobody else using this app ",
               "sees them or your data."),
      textInput("gas_csv_url",  "Gas CSV URL",         value = "https://raw.githubusercontent.com/atticus1111/Data-app-for-BHP/refs/heads/main/phpdata.csv"),
      textInput("elec_csv_url", "Electricity CSV URL", value = "https://raw.githubusercontent.com/atticus1111/Data-app-for-BHP/main/Electricity_by_date.csv"),
      actionButton("load_csv_btn", "Load CSV link(s)", class = "btn-secondary"),
      hr(),
      h5("Filters"),
      selectInput("fuel_filter", "Fuel type:",
                  choices = c("Gas", "Electricity", "Both"), selected = "Both"),
      dateRangeInput("date_range", "Date range:",
                     start = Sys.Date() - 365,
                     end   = Sys.Date()),
      hr(),
      h5("Upload Bill PDF(s)"),
      fileInput("pdf_upload", NULL, accept = ".pdf", multiple = TRUE,
                buttonLabel = "Choose PDF(s)"),
      helpText("You can select multiple PDFs at once. Each one is expected ",
               "to contain both gas and electricity charges -- both fuels ",
               "will be extracted automatically from every file. Bills are ",
               "grouped by billing month, so two bills dated a few days ",
               "apart in the same month are treated as one period (the ",
               "most recently added one wins if values differ).")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        # ---- Combined Overview ----
        tabPanel("Overview",
                 br(),
                 fluidRow(
                   column(6, plotOutput("gas_total_plot",  height = "300px")),
                   column(6, plotOutput("elec_total_plot", height = "300px"))
                 ),
                 br(),
                 plotOutput("combined_cost_plot", height = "300px")
        ),

        # ---- Gas Detail ----
        tabPanel("Gas Detail",
                 br(),
                 selectInput("gas_charges", "Select charges:",
                             choices  = NULL,
                             multiple = TRUE),
                 plotOutput("gasTrendPlot", height = "350px"),
                 br(),
                 tableOutput("gasSummaryTable")
        ),

        # ---- Electricity Detail ----
        tabPanel("Electricity Detail",
                 br(),
                 selectInput("elec_charges", "Select charges:",
                             choices  = NULL,
                             multiple = TRUE),
                 plotOutput("elecTrendPlot", height = "350px"),
                 br(),
                 tableOutput("elecSummaryTable")
        ),

        # ---- Electrification Planner ----
        tabPanel("Electrification Planner",
                 br(),
                 fluidRow(
                   column(4,
                          h5("Gas heating assumptions"),
                          sliderInput("baseline_gas", "Non-heating baseline gas use (Dth/mo):",
                                      min=0, max=200, value=50, step=5),
                          helpText("Lowest-usage summer month ≈ baseline (water heater, cooking). Everything above this in winter is heating."),
                          sliderInput("cop", "Heat pump efficiency (COP):",
                                      min=1.5, max=5, value=2.5, step=0.1),
                          helpText("Typical range: 2.0 (cold climate, old unit) to 4.0 (mild climate, new unit).")
                   ),
                   column(4,
                          h5("Electric demand assumptions"),
                          sliderInput("demand_rate", "Total demand charge rate ($/kW/mo):",
                                      min=10, max=60, value=24, step=0.5),
                          helpText("Sum of all per-kW line items on your electric bill."),
                          sliderInput("kwh_rate", "Blended energy rate ($/kWh):",
                                      min=0.01, max=0.30, value=0.03, step=0.005),
                          helpText("Total energy charges ÷ total kWh from your electric bills.")
                   ),
                   column(4,
                          h5("Results"),
                          br(),
                          tableOutput("strain_table"),
                          br(),
                          helpText("These are estimates. Actual savings depend on equipment, climate, and rate changes."),
                          hr(),
                          h6("Gas fixed charges eliminated by full electrification:"),
                          textOutput("gas_fixed_savings")
                   )
                 ),
                 br(),
                 plotOutput("electrification_plot", height = "350px")
        ),

        # ---- Upload PDF ----
        tabPanel("Upload PDF",
                 br(),
                 h4("Extracted charges"),
                 tableOutput("pdfPreview"),
                 textOutput("pdf_status"),
                 br(),
                 actionButton("add_pdf_data", "Add to my data", class = "btn-primary"),
                 hr(),
                 h5("Download compiled data"),
                 helpText("Downloads everything currently in your session (from CSV links and/or PDFs you've added above)."),
                 downloadButton("download_xlsx", "Download Excel (all data, .xlsx)", class = "btn-success"),
                 br(), br(),
                 downloadButton("download_gas",  "Download gas CSV"),
                 downloadButton("download_elec", "Download electricity CSV"),
                 hr(),
                 h5("Reset"),
                 helpText("If old bills were added before a bug fix, they can be sitting under the wrong ",
                          "month or hold a wrong value. Clearing your data and re-uploading all PDFs from ",
                          "scratch guarantees a clean re-extraction with the current (fixed) parsing logic."),
                 actionButton("clear_data_btn", "Clear all data", class = "btn-danger")
        )
      )
    )
  )
)

# ============================================================
# 5. SERVER
# ============================================================
server <- function(input, output, session) {

  gas_store  <- reactiveVal(empty_bill_df())
  elec_store <- reactiveVal(empty_bill_df())

  # -- Load optional CSV links, entered at runtime --
  observeEvent(input$gas_csv_url, {
   gas_new  <- load_bill_data(input$gas_csv_url,  "gas CSV")
    elec_new <- load_bill_data(input$elec_csv_url, "electricity CSV")

    n_added <- 0
    if (nrow(gas_new) > 0) {
      gas_store(merge_bill_data(gas_store(), gas_new))
      n_added <- n_added + nrow(gas_new)
    }
    if (nrow(elec_new) > 0) {
      elec_store(merge_bill_data(elec_store(), elec_new))
      n_added <- n_added + nrow(elec_new)
    }

    if (n_added == 0) {
      showNotification("No data loaded -- check the URL(s) and that the CSV has a 'Description' column.", type = "warning")
    } else {
      showNotification(paste0("Loaded ", n_added, " rows from CSV link(s)."), type = "message")
    }
  })

  # -- Keep charge-selector choices and date range in sync with whatever
  #    data is currently in the store (from CSV links and/or PDFs) --
  observe({
    gs <- gas_store()
    choices <- unique(gs$Description)
    default_sel <- intersect(c("Total", "Usage Charge", "Capacity Charge"), choices)
    current_sel <- intersect(isolate(input$gas_charges), choices)
    sel <- if (length(current_sel) > 0) current_sel else default_sel
    updateSelectInput(session, "gas_charges", choices = choices, selected = sel)
  })

  observe({
    es <- elec_store()
    choices <- unique(es$Description)
    default_sel <- intersect(c("Elec Total", "Distribution Demand", "Gen & Transm Demand"), choices)
    current_sel <- intersect(isolate(input$elec_charges), choices)
    sel <- if (length(current_sel) > 0) current_sel else default_sel
    updateSelectInput(session, "elec_charges", choices = choices, selected = sel)
  })

  observe({
    all_dates <- c(gas_store()$bill_date, elec_store()$bill_date)
    if (length(all_dates) > 0) {
      updateDateRangeInput(session, "date_range",
                            start = min(all_dates, na.rm = TRUE),
                            end   = max(all_dates, na.rm = TRUE))
    }
  })

  # -- PDF parsing: one upload can include multiple files, each yielding
  #    both gas and electricity rows --
  pdf_extracted <- reactive({
    req(input$pdf_upload)
    parse_multiple_pdfs(input$pdf_upload)
  })

  output$pdfPreview <- renderTable({
    df <- pdf_extracted()
    validate(need(nrow(df) > 0,
                  "No charges extracted. The PDF(s) may not contain 'NATURAL GAS CHARGES' or 'ELECTRICITY CHARGES' headers."))
    df %>%
      mutate(bill_date = format(bill_date, "%b %Y")) %>%
      rename(`Billing Month` = bill_date) %>%
      select(source_file, fuel, Description, `Billing Month`, value) %>%
      arrange(source_file, fuel, Description)
  })

  output$pdf_status <- renderText({
    req(input$pdf_upload)
    df <- pdf_extracted()
    n_files <- length(unique(input$pdf_upload$name))
    if (nrow(df) == 0) return(paste0("Nothing extracted from ", n_files, " file(s)."))
    gas_n  <- sum(df$fuel == "gas")
    elec_n <- sum(df$fuel == "electricity")
    n_dates <- length(unique(df$bill_date))
    paste0(
      "Parsed ", n_files, " file(s): found ", gas_n, " gas charge(s) and ",
      elec_n, " electricity charge(s) across ", n_dates, " bill date(s)."
    )
  })

  observeEvent(input$add_pdf_data, {
    df <- pdf_extracted()
    if (nrow(df) == 0) { showNotification("Nothing to add.", type = "warning"); return() }

    gas_rows  <- df %>% filter(fuel == "gas")         %>% select(Description, bill_date, value)
    elec_rows <- df %>% filter(fuel == "electricity") %>% select(Description, bill_date, value)

    if (nrow(gas_rows) > 0) {
      gas_store(merge_bill_data(gas_store(), gas_rows))
    }
    if (nrow(elec_rows) > 0) {
      elec_store(merge_bill_data(elec_store(), elec_rows))
    }
    showNotification(
      paste0("Added ", nrow(gas_rows), " gas and ", nrow(elec_rows),
             " electric charge(s) from ", length(unique(df$source_file)),
             " file(s), grouped by billing month."),
      type = "message"
    )
  })

  # -- Reset: clear all stored data (with a confirmation step, since this
  #    can't be undone) --
  observeEvent(input$clear_data_btn, {
    showModal(modalDialog(
      title = "Clear all data?",
      "This removes every gas and electricity row currently stored in this session (from CSV links and/or uploaded PDFs). This can't be undone. Re-upload your PDFs afterward to rebuild the data with the current parsing logic.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_data", "Clear all data", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_clear_data, {
    gas_store(empty_bill_df())
    elec_store(empty_bill_df())
    removeModal()
    showNotification("All data cleared. Upload your PDFs again to rebuild it.", type = "message")
  })

  # -- Downloads --
  output$download_gas <- downloadHandler(
    filename = function() paste0("gas_data_", Sys.Date(), ".csv"),
    content  = function(f) write_csv(
      gas_store() %>% mutate(bill_date = format(bill_date, "%Y-%m")) %>%
        pivot_wider(names_from = bill_date, values_from = value), f)
  )
  output$download_elec <- downloadHandler(
    filename = function() paste0("elec_data_", Sys.Date(), ".csv"),
    content  = function(f) write_csv(
      elec_store() %>% mutate(bill_date = format(bill_date, "%Y-%m")) %>%
        pivot_wider(names_from = bill_date, values_from = value), f)
  )

  output$download_xlsx <- downloadHandler(
    filename = function() paste0("utility_data_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      gs <- gas_store()
      es <- elec_store()

      gas_long  <- gs %>% arrange(bill_date, Description)
      elec_long <- es %>% arrange(bill_date, Description)

      # Use "YYYY-MM" column names for the wide sheets -- one column per
      # billing month, so pivot_wider doesn't choke on Date-typed names
      gas_wide <- gs %>%
        mutate(bill_date = format(bill_date, "%Y-%m")) %>%
        pivot_wider(names_from = bill_date, values_from = value)
      elec_wide <- es %>%
        mutate(bill_date = format(bill_date, "%Y-%m")) %>%
        pivot_wider(names_from = bill_date, values_from = value)

      sheets <- list(
        "Gas (long)"         = gas_long,
        "Electricity (long)" = elec_long,
        "Gas (wide)"         = gas_wide,
        "Electricity (wide)" = elec_wide
      )
      # Drop any empty sheets so the workbook doesn't include blank tabs
      sheets <- sheets[sapply(sheets, nrow) > 0]
      if (length(sheets) == 0) sheets <- list("No data" = data.frame(Note = "No data loaded yet."))

      write_xlsx(sheets, path = file)
    }
  )

  # -- Overview plots --
  output$gas_total_plot <- renderPlot({
    df <- gas_store() %>% filter(Description == "Total")
    validate(need(nrow(df)>0, "No gas Total data yet."))
    ggplot(df, aes(bill_date, value)) +
      geom_col(fill="#e07b39") +
      scale_y_continuous(labels=dollar) +
      labs(title="Monthly Gas Total", x=NULL, y="$") +
      theme_minimal(base_size=13)
  })

  output$elec_total_plot <- renderPlot({
    df <- elec_store() 
    #%>% filter(Description == "Elec Total") 
    validate(need(nrow(df)>0, "No electricity Total data yet."))
    ggplot(df, aes(bill_date, value)) +
      geom_col(width = 20,fill="#3a7ebf") +
      scale_y_continuous(labels=dollar) +
      labs(title="Monthly Electricity Total", x=NULL, y="$") +
      theme_minimal(base_size=13)
  })

  
  output$combined_cost_plot <- renderPlot({
    g <- gas_store()  %>% filter(Description == "Total") %>% mutate(fuel="Gas")
    e <- elec_store()  %>% mutate(fuel="Electricity")
      #filter(Description == "Elec Total") 
    df <- bind_rows(g, e)
    validate(need(nrow(df)>0, "No data yet."))
    ggplot(df, aes(bill_date, value, fill=fuel)) +
      geom_col(width=15, position="stack") +
      scale_fill_manual(values=c("Gas"="#e07b39","Electricity"="#3a7ebf")) +
      scale_y_continuous(labels=dollar) +
      labs(title="Combined Monthly Utility Cost", x=NULL, y="$", fill=NULL) +
      theme_minimal(base_size=13)
  })

  # -- Gas detail --
  output$gasTrendPlot <- renderPlot({
    req(input$gas_charges)
    df <- gas_store() %>%
      filter(Description %in% input$gas_charges,
             bill_date >= input$date_range[1], bill_date <= input$date_range[2])
    validate(need(nrow(df)>0, "No data for selection."))
    ggplot(df, aes(bill_date, value, color=Description)) +
      geom_line(linewidth=1) + geom_point(size=2) +
      scale_y_continuous(labels=dollar) +
      labs(x=NULL, y="$", color=NULL, title="Gas charges over time") +
      theme_minimal(base_size=13)
  })

  output$gasSummaryTable <- renderTable({
    req(input$gas_charges)
    gas_store() %>% filter(Description %in% input$gas_charges) %>%
      group_by(Description) %>%
      summarise(Min=dollar(min(value)), Max=dollar(max(value)),
                Average=dollar(mean(value)), `Annual Total`=dollar(sum(value)), .groups="drop")
  })

  # -- Electricity detail --
  output$elecTrendPlot <- renderPlot({
    req(input$elec_charges)
    df <- elec_store() %>%
      filter(Description %in% input$elec_charges,
             bill_date >= input$date_range[1], bill_date <= input$date_range[2])
    validate(need(nrow(df)>0, "No data for selection."))
    ggplot(df, aes(bill_date, value, color=Description)) +
      geom_line(linewidth=1) + geom_point(size=2) +
      scale_y_continuous(labels=dollar) +
      labs(x=NULL, y="$", color=NULL, title="Electricity charges over time") +
      theme_minimal(base_size=13)
  })

  output$elecSummaryTable <- renderTable({
    req(input$elec_charges)
    elec_store() %>% filter(Description %in% input$elec_charges) %>%
      group_by(Description) %>%
      summarise(Min=dollar(min(value)), Max=dollar(max(value)),
                Average=dollar(mean(value)), `Annual Total`=dollar(sum(value)), .groups="drop")
  })

  # -- Electrification planner --
  # Estimate heating gas = average monthly gas total minus baseline
  heating_gas_estimate <- reactive({
    totals <- gas_store() %>% filter(Description == "Total")
    if (nrow(totals) == 0) return(100)  # fallback default
    usage <- gas_store() %>% filter(Description == "Usage Charge")
    if (nrow(usage) > 0) {
      avg_usage_cost <- mean(usage$value, na.rm=TRUE)
      # $0.50/Dth approx blended rate
      estimated_dth <- avg_usage_cost / 0.50
      max(0, estimated_dth - input$baseline_gas)
    } else {
      100
    }
  })

  strain_results <- reactive({
    electrification_model(
      heating_gas_dth   = heating_gas_estimate(),
      cop               = input$cop,
      current_peak_kw   = 188,
      demand_rate       = input$demand_rate,
      kwh_rate          = input$kwh_rate,
      peak_adder_pct    = 0.5
    )
  })

  output$strain_table <- renderTable({
    r <- strain_results()
    data.frame(
      Metric = c(
        "Heating load converted to kWh/mo",
        "Added electric energy cost/mo",
        "Estimated added peak demand (kW)",
        "Added demand charge/mo",
        "Total added electric cost/mo"
      ),
      Value = c(
        paste0(r$heat_kwh_electric, " kWh"),
        dollar(r$added_energy_cost),
        paste0(r$added_peak_kw, " kW"),
        dollar(r$added_demand_cost),
        dollar(r$total_added_cost)
      )
    )
  })

  output$gas_fixed_savings <- renderText({
    svc <- gas_store() %>% filter(Description == "Service & Facility")
    cap <- gas_store() %>% filter(Description == "Capacity Charge")
    svc_avg <- if(nrow(svc)>0) mean(svc$value, na.rm=TRUE) else 225
    cap_avg <- if(nrow(cap)>0) mean(cap$value, na.rm=TRUE) else 1305
    paste0("~", dollar(svc_avg + cap_avg), "/month (",
           dollar((svc_avg + cap_avg)*12), "/year) in Service & Capacity charges eliminated")
  })

  output$electrification_plot <- renderPlot({
    r <- strain_results()
    svc <- gas_store() %>% filter(Description == "Service & Facility")
    cap <- gas_store() %>% filter(Description == "Capacity Charge")
    gas_usage <- gas_store() %>% filter(Description == "Usage Charge")
    gas_tax   <- gas_store() %>% filter(Description %in% c("Franchise Fee","Climate Tax","Sales Tax"))

    svc_avg      <- if(nrow(svc)>0)      mean(svc$value,      na.rm=TRUE) else 225
    cap_avg      <- if(nrow(cap)>0)      mean(cap$value,      na.rm=TRUE) else 1305
    gas_usage_avg<- if(nrow(gas_usage)>0) mean(gas_usage$value,na.rm=TRUE) else 450
    gas_tax_avg  <- if(nrow(gas_tax)>0)  sum(tapply(gas_tax$value, gas_tax$Description, mean, na.rm=TRUE)) else 150

    post_elec_added <- r$total_added_cost

    savings_net <- (svc_avg + cap_avg + gas_usage_avg + gas_tax_avg) - post_elec_added
    title_str <- paste0("Estimated monthly impact of full gas electrification: ",
                        ifelse(savings_net>0, "save ", "cost added "),
                        dollar(abs(savings_net)), "/mo")

    bar_df <- data.frame(
      Category = c("Gas fixed\n(eliminated)", "Gas usage+tax\n(eliminated)", "New electric\nheating cost"),
      Value    = c(svc_avg + cap_avg, gas_usage_avg + gas_tax_avg, post_elec_added),
      Color    = c("Savings","Savings","New cost")
    )

    ggplot(bar_df, aes(Category, Value, fill=Color)) +
      geom_col(width=0.6) +
      scale_fill_manual(values=c("Savings"="#4caf50","New cost"="#3a7ebf")) +
      scale_y_continuous(labels=dollar) +
      labs(title=title_str, x=NULL, y="$/month", fill=NULL,
           caption="Based on monthly averages. Actual results will vary by season, equipment, and rate changes.") +
      theme_minimal(base_size=13)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
