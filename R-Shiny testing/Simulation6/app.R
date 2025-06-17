#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Title",

    # Application title
    tabPanel("Page 1",
             textInput("name", "Enter your name:")
    ),
    tabPanel("Page 2", "Your name is",
             textOutput("name")
             )
)

server <- function(input, output) {
  output$name <- renderText({ input$name })
}
# Run the application 
shinyApp(ui = ui, server = server)
