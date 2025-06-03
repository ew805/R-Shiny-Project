#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


ui <- fluidPage(
  
  # Application title
  titlePanel("Quiz"),
  
  
  radioButtons("question", "What is 1 + 1", choices =
                 c("1", "2", "3", "4")),
  textOutput("results"),
  textOutput("lives")
  
)
server <- function(input, output) { 
  lives <- reactiveVal(3)
  output$results <- renderText({
    if (input$question =="2"){"Correct"}
    else {lives(lives()-1)
      "Incorrect" 
      }
  })
 output$lives <- renderText({paste("Lives left:", lives())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)