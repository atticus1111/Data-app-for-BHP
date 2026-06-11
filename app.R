library(shiny)
library(bslib)

# features that I want: 
# - file upload for the excell documents, ideally could just read from the excell folder
# directly 
# - a data display of uploaded data, tracking rates, maxes, averages, costs,




ui <- page_sidebar(
  title ="hello",
  sidebar=sidebar(
    sliderInput("num", "Number of bins:", 1, 50, 30)
  ),
  plotOutput("distPlot"),
  fileInput("file", "upload data"),
  textInput("text", "Enter text:"),
  renderText("text")


)

server<-function(input, output) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$num + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
 

  


}


shinyApp(ui=ui, server=server)