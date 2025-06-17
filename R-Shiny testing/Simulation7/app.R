#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


ui <- navbarPage("Counter",
                 tabPanel("Counter",
                          actionButton("count", "Click!")),
                 tabPanel("Result",
                          textOutput("Number")),
)
server <- function(input, output){
  Number <- reactiveVal(0)
  observeEvent(input$count, {Number(Number() + 1)})
  output$Number <- renderText({paste(Number(), "clicks")})
}
# Run the application 
shinyApp(ui = ui, server = server)
