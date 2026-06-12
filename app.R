library(shiny)
library(bslib)

# features that I want: 
# - file upload for the excell documents, ideally could just read from the excell folder
# directly 
# - a data display of uploaded data, tracking rates, maxes, averages, costs,



# define UI
ui <- page_fluid( 
  title ="BHP dashboard",

  layout_sidebar(
    
    sidebar = sidebar("controlls",
                      position = "left",
                      
                      card(
                        card_header("File Folder Selecter"),
                        fileInput("foler_in_",
                                  label ="Input folder: "
                        )
                      ),
                      card(
                        card_header("Individual File Selector"),
                        fileInput("file_in_",
                                  label = "Input File:"
                        )
                      ),
                      card(
                        card_header("Graph controlls"),
                        radioButtons(inputId = "controlls_",
                                     label = "cont",
                          choices = c( "1", "2", "3")  )
                        ),
                      card(
                        card_header("Graph Sliders"),
                        sliderInput("date_",
                                    label = "date range:",
                                    min=0,
                                    max= 100,
                                    value = c(50,75)
                        )
                      )
    ),
    card(
      card_header("Analyitics"),
      textOutput("selection_")
    )
  )

  )



# define Server function
server<-function(input, output) {

  
  output$selection_ <- renderText({
    
    switch(
      input$controlls_,
      "1" = paste("你的書", input$controlls_),
      "2" = paste("your book", input$controlls_),
      "3" = paste("tu libro", input$controlls_)
    )
    
  })
  


}

# runs app
shinyApp(ui=ui, server=server)