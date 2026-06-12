library(shiny)
library(bslib)
library(reticulate)

use_python(
  "/usr/bin/python3",
  required = TRUE
)

source_python("~/Documents/Computing/Future City/Data-app-for-BHP/file_convert_.py")

ui <- page_fluid(
  
  title = "BHP Dashboard",
  
  layout_sidebar(
    
    sidebar = sidebar(
      
      "controls",
      
      position = "left",
      
      card(
        card_header("Individual File Selector"),
        
        fileInput(
          "file_in_",
          label = "Manual Input File:",
          accept = c(".pdf")
        )
      ),
      
      card(
        card_header("Table Selector"),
        
        sliderInput(
          "table_num_",
          label = "Table Number",
          min = 1,
          max = 20,
          value = 1
        )
      )
      
    ),
    
    card(
      
      card_header("Analytics"),
      
      textOutput("selection_"),
      
      tableOutput("data_1")
      
    )
    
  )
  
)

server <- function(input, output) {
  
  pdf_data <- reactive({
    
    req(input$file_in_)
    
    extract_tables_to_json(
      input$file_in_$datapath
    )
    
  })
  
  output$selection_ <- renderText({
    
    req(pdf_data())
    
    paste(
      "Tables found:",
      pdf_data()$total_tables
    )
    
  })
  
  output$data_1 <- renderTable({
    
    req(pdf_data())
    
    idx <- input$table_num_
    
    req(
      idx <= length(pdf_data()$tables)
    )
    
    pdf_data()$tables[[idx]]$data
    
  })
  
}

shinyApp(ui, server)