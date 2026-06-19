library(shiny)
library(bslib)
library(reticulate)
library(DT)
library(shinydashboard)
library(ggplot2)

# state: changed ui to dashboard style to look better, hoping that each tab could be a
# differnt property in the end. I took G's data from excell and converted to a csv
# not quite sure what to do with the data yet beause this requires new methods of graphing and analysis 
# than I am used too. The issue i see right now is that I want to select the data based on its row
# but the rownames aren't really loading in how I want. 
# will see about this later, maybe data needs to be manipulated to better deal with its format. 



use_python(
  "/usr/bin/python3",
  required = TRUE
)

file_1<-read.csv("~/Documents/Computing/Future City/Data-app-for-BHP/Book1.csv")


source_python("~/Documents/Computing/Future City/Data-app-for-BHP/file_convert_.py")

header <- dashboardHeader(
  title = "BHP Dashboard"
)

sidebar <- dashboardSidebar(
  sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                       badgeLabel = "new", badgeColor = "green")
  ))



  tab1<-tabItem(
    tabName = "dashboard",
    h2("Dashboard tab content")
  )
  
  tab2 <- tabItem(
    
    tabName = "widgets",
    
    h2("Widgets tab content"),
    
    box(
      
      title = "Individual File Selector",
      
      fileInput(
        "file_in_",
        label = "Manual Input File:",
        accept = c(".pdf")
      )
      
    ),
    
    box(
      
      title = "Table Selector",
      
      sliderInput(
        "table_num_",
        label = "Table Number",
        min = 1,
        max = 20,
        value = 1
      )
      
    ),
    
    box(
      
      title = "Table Information",
      
      textOutput("selection_")
      
    ),
    
    box(
      
      title = "Extracted Table",
      
      DTOutput("data_1")
      
    ),
    plotOutput("graph_")
    
  )

ui <- dashboardPage(
   header,
   sidebar,
   dashboardBody(
     tabItems(
    tab1,
    tab2
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
  
  output$data_1 <- renderDT({
    
    req(pdf_data())
    
    idx <- input$table_num_
    
    req(idx <= length(pdf_data()$tables))
    
    as.data.frame(
      pdf_data()$tables[[idx]]$data
    )
    
  })
  

  
  output$graph_<-renderPlot({
    ggplot(file_1,
           aes(x=`Description`, y=`Subtotal`))+
      geom_point()
    
  })
}
  


shinyApp(ui, server)