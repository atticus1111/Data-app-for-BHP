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
  sidebarMenu(menuItem("File Input", tabName = "file_input", icon = icon("dashboard")),
              menuItem("Golden West", icon = icon("th"), tabName = "GW_data")
  ))



  tab1<-tabItem(
    tabName = "file_input",
    h2("File Inputs and Selector"),
    
  )
  
  tab2 <- tabItem(
    
    tabName = "GW_data",
    
    h2("Widgets tab content"),
    
    box(
      
      title = "Golden West Data (Apr 25 - May 26)",
      
    plotOutput("graph_"),
    
    textOutput("out_")
    
    )
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
  
  # renaming columns so they are in a date format
  for (i in 2:(ncol(file_1))){
    n <- colnames(file_1)[i]
    n<- gsub("^X","",n)
    m<-as.Date(n, format="%m.%d.%Y")
    if(!is.na(m)) {
      colnames(file_1)[i] <- as.character(m)
    }   
  }
  # 3 extra cols added at end for some reason, this dropps
  
  file_1<-file_1 %>% 
    subset(select=-c(14:16)) 
  
  output$graph_<-renderPlot({
    ggplot(file_1,
           aes(x=`Description`, y=`Subtotal`))+
      geom_area()
    #we want a stacked area chart for this
  })
  
  output$out_ <- renderText({
    names (file_1)
  })
  
}
  
shinyApp(ui, server)