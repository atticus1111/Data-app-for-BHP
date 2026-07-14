#
# Utility Bill Analysis + Electrification Planner
# Tracks natural gas AND electricity bills, models electrification impact
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

# ============================================================
# 1. CONFIGURATION — paste your GitHub raw CSV URLs here
# ============================================================
gas_csv_url  <- "https://raw.githubusercontent.com/atticus1111/Data-app-for-BHP/refs/heads/main/phpdata.csv"
elec_csv_url <- "https://raw.githubusercontent.com/atticus1111/Data-app-for-BHP/refs/heads/main/phpdata.csv"  # can be same file or separate

# ============================================================
# 2. DATA LOADING
# ============================================================
load_bill_data <- function(url, label = "data") {
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
      filter(!is.na(bill_date), !is.na(value))
  }, error = function(e) {
    message("Could not load ", label, ": ", e$message)
    data.frame(Description = character(), bill_date = as.Date(character()), value = numeric())
  })
}

gas_data  <- load_bill_data(gas_csv_url,  "gas CSV")
elec_data <- load_bill_data(elec_csv_url, "electricity CSV")

# ============================================================
# 3. PDF PARSERS
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
  dates  <- str_extract_all(all_text, "\\d{1,2}/\\d{1,2}/\\d{2,4}")[[1]]
  parsed <- suppressWarnings(mdy(dates))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) return(NA_real_)
  max(parsed, na.rm = TRUE)
}

extract_charges <- function(all_lines, patterns) {
  lapply(names(patterns), function(desc) {
    pat <- patterns[[desc]]
    matched <- all_lines %>%
      mutate(label_only = str_trim(str_extract(text, "^[A-Za-z0-9 &.,]+?(?=\\s+[\\d$])"))) %>%
      filter(str_detect(label_only, regex(pat, ignore_case = TRUE)))
    if (nrow(matched) == 0) return(data.frame(Description = desc, value = NA_real_))
    amounts <- str_extract_all(matched$text[1], "-?\\d[\\d,]*\\.\\d{2}")[[1]]
    amt <- if (length(amounts) == 0) NA_real_ else as.numeric(gsub(",", "", amounts[length(amounts)]))
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

# Main combined parser — detects which pages belong to gas vs electricity
# by scanning for section headers, then applies the right pattern set.
# Works whether sections are on the same page or different pages.
parse_combined_pdf <- function(pdf_path) {
  pages     <- pdf_data(pdf_path)
  all_text  <- paste(pdf_text(pdf_path), collapse = " ")
  bill_date <- extract_bill_date(all_text)
  
  gas_lines  <- list()
  elec_lines <- list()
  
  for (i in seq_along(pages)) {
    page_lines <- group_words_into_lines(pages[[i]])
    combined   <- paste(page_lines$text, collapse = " ")
    
    has_gas  <- str_detect(combined, regex("NATURAL GAS CHARGES",  ignore_case = TRUE))
    has_elec <- str_detect(combined, regex("ELECTRICITY CHARGES",  ignore_case = TRUE))
    
    if (has_gas && has_elec) {
      # Both sections on the same page — split by y-position of each header
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
  
  bind_rows(results)
}

# ============================================================
# 4. ELECTRIFICATION STRAIN MODEL
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
# 5. UI
# ============================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Utility Bill Analysis & Electrification Planner"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h5("Filters"),
      selectInput("fuel_filter", "Fuel type:",
                  choices = c("Gas", "Electricity", "Both"), selected = "Both"),
      dateRangeInput("date_range", "Date range:",
                     start = min(c(gas_data$bill_date, elec_data$bill_date), na.rm = TRUE),
                     end   = max(c(gas_data$bill_date, elec_data$bill_date), na.rm = TRUE)),
      hr(),
      h5("Upload a Bill PDF"),
      fileInput("pdf_upload", NULL, accept = ".pdf", buttonLabel = "Choose PDF"),
      helpText("Each PDF contains both gas and electricity charges — both will be extracted automatically.")
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
                             choices  = unique(gas_data$Description),
                             selected = c("Total", "Usage Charge", "Capacity Charge"),
                             multiple = TRUE),
                 plotOutput("gasTrendPlot", height = "350px"),
                 br(),
                 tableOutput("gasSummaryTable")
        ),
        
        # ---- Electricity Detail ----
        tabPanel("Electricity Detail",
                 br(),
                 selectInput("elec_charges", "Select charges:",
                             choices  = unique(elec_data$Description),
                             selected = c("Elec Total", "Distribution Demand", "Gen & Transm Demand"),
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
                          helpText("Sum of all per-kW line items on your electric bill. Your current bills show ~$23.85/kW."),
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
                 downloadButton("download_gas",  "Download gas CSV"),
                 downloadButton("download_elec", "Download electricity CSV")
        )
      )
    )
  )
)

# ============================================================
# 6. SERVER
# ============================================================
server <- function(input, output, session) {
  
  gas_store  <- reactiveVal(gas_data)
  elec_store <- reactiveVal(elec_data)
  
  # -- PDF parsing: one upload extracts both gas and electricity --
  pdf_extracted <- reactive({
    req(input$pdf_upload)
    parse_combined_pdf(input$pdf_upload$datapath)
  })
  
  output$pdfPreview <- renderTable({
    df <- pdf_extracted()
    validate(need(nrow(df) > 0,
                  "No charges extracted. The PDF may not contain 'NATURAL GAS CHARGES' or 'ELECTRICITY CHARGES' headers."))
    df %>%
      mutate(bill_date = format(bill_date, "%m/%d/%Y")) %>%
      select(fuel, Description, bill_date, value) %>%
      arrange(fuel, Description)
  })
  
  output$pdf_status <- renderText({
    req(input$pdf_upload)
    df <- pdf_extracted()
    if (nrow(df) == 0) return("Nothing extracted.")
    gas_n  <- sum(df$fuel == "gas")
    elec_n <- sum(df$fuel == "electricity")
    paste0(
      "Found ", gas_n,  " gas charge(s) and ",
      elec_n, " electricity charge(s) for bill date ",
      format(df$bill_date[1], "%m/%d/%Y"), "."
    )
  })
  
  observeEvent(input$add_pdf_data, {
    df <- pdf_extracted()
    if (nrow(df) == 0) { showNotification("Nothing to add.", type="warning"); return() }
    
    gas_rows  <- df %>% filter(fuel == "gas")  %>% select(Description, bill_date, value)
    elec_rows <- df %>% filter(fuel == "electricity") %>% select(Description, bill_date, value)
    
    if (nrow(gas_rows) > 0) {
      updated_gas <- bind_rows(gas_store(), gas_rows) %>%
        distinct(Description, bill_date, .keep_all=TRUE) %>% arrange(bill_date)
      gas_store(updated_gas)
      updateSelectInput(session, "gas_charges", choices=unique(updated_gas$Description))
    }
    if (nrow(elec_rows) > 0) {
      updated_elec <- bind_rows(elec_store(), elec_rows) %>%
        distinct(Description, bill_date, .keep_all=TRUE) %>% arrange(bill_date)
      elec_store(updated_elec)
      updateSelectInput(session, "elec_charges", choices=unique(updated_elec$Description))
    }
    showNotification(
      paste0("Added ", nrow(gas_rows), " gas and ", nrow(elec_rows), " electric charge(s). Download CSV to save permanently."),
      type="message"
    )
  })
  
  output$download_gas <- downloadHandler(
    filename = function() paste0("gas_data_", Sys.Date(), ".csv"),
    content  = function(f) write_csv(gas_store()  %>% pivot_wider(names_from=bill_date, values_from=value), f)
  )
  output$download_elec <- downloadHandler(
    filename = function() paste0("elec_data_", Sys.Date(), ".csv"),
    content  = function(f) write_csv(elec_store() %>% pivot_wider(names_from=bill_date, values_from=value), f)
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
    df <- elec_store() %>% filter(Description == "Elec Total")
    validate(need(nrow(df)>0, "No electricity Total data yet."))
    ggplot(df, aes(bill_date, value)) +
      geom_col(fill="#3a7ebf") +
      scale_y_continuous(labels=dollar) +
      labs(title="Monthly Electricity Total", x=NULL, y="$") +
      theme_minimal(base_size=13)
  })
  
  output$combined_cost_plot <- renderPlot({
    g <- gas_store()  %>% filter(Description == "Total") %>% mutate(fuel="Gas")
    e <- elec_store() %>% filter(Description == "Elec Total") %>% mutate(fuel="Electricity")
    df <- bind_rows(g, e)
    validate(need(nrow(df)>0, "No data yet."))
    ggplot(df, aes(bill_date, value, fill=fuel)) +
      geom_col(position="stack") +
      scale_fill_manual(values=c("Gas"="#e07b39","Electricity"="#3a7ebf")) +
      scale_y_continuous(labels=dollar) +
      labs(title="Combined Monthly Utility Cost", x=NULL, y="$", fill=NULL) +
      theme_minimal(base_size=13)
  })
  
  # -- Gas detail --
  output$gasTrendPlot <- renderPlot({
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
    gas_store() %>% filter(Description %in% input$gas_charges) %>%
      group_by(Description) %>%
      summarise(Min=dollar(min(value)), Max=dollar(max(value)),
                Average=dollar(mean(value)), `Annual Total`=dollar(sum(value)), .groups="drop")
  })
  
  # -- Electricity detail --
  output$elecTrendPlot <- renderPlot({
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
    avg_total_dth <- mean(totals$value, na.rm=TRUE)
    # Rough: assume Usage Charge / blended gas rate gives Dth proxy
    # Use Dth from data if available, else derive from cost ratio
    usage <- gas_store() %>% filter(Description == "Usage Charge")
    if (nrow(usage) > 0) {
      avg_usage_cost <- mean(usage$value, na.rm=TRUE)
      # $0.50/Dth approx blended rate from your bills
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
    
    current_gas_total  <- svc_avg + cap_avg + gas_usage_avg + gas_tax_avg
    post_elec_gas_cost <- 0  # gas service eliminated
    post_elec_added    <- r$total_added_cost
    
    df <- data.frame(
      Scenario = c("Current: Gas fixed","Current: Gas usage+tax",
                   "Post-electrification: Gas saved","Post-electrification: Added electric"),
      Amount   = c(svc_avg + cap_avg, gas_usage_avg + gas_tax_avg,
                   -(svc_avg + cap_avg + gas_usage_avg + gas_tax_avg), post_elec_added),
      Type     = c("Gas fixed cost","Gas variable cost","Savings","New electric cost")
    )
    
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
