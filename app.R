library(shiny)
library(bslib)

ui <- page_sidebar(
  title ="hello",
  sidebar=sidebar(
    sliderInput("num", "Number of bins:", 1, 50, 30)
  ),
  plotOutput("distPlot")

)

server<-function(input, output) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$num + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}


shinyApp(ui=ui, server=server)