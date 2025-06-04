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

    
    titlePanel("Results"),
    h4("Reducing hearts for free tier"),

    
    sidebarLayout(
        sidebarPanel(
          textOutput("press"),
          actionButton("resultsbutton", "Press here for results!")
        ),

       
        mainPanel(
          textOutput("results")
           
        )
    )
)


server <- function(input, output) {
  output$press <- renderText ({"Press the button below to reveal the results
    of your test."})
  output$results <- renderText({
    if(input$resultsbutton > 0){
      "Here are the results:"
    }
    
  })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
