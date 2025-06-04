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
          actionButton("resultsbutton", "Press here for results!"),
          textOutput("CI"),
          actionButton("CIbutton", "Press here to see graphed confidence intervals")
        ),

       
        mainPanel(
          textOutput("results"),
          textOutput("CIgraphs")
           
        )
    )
)


server <- function(input, output) {
  output$press <- renderText ({"Press the button below to reveal the results
    of your test."})
  output$results <- renderText({
    if(input$resultsbutton > 0){
      "Here are the results:"
    } })
    output$CI <- renderText({"If you would like to see a graph of the confidence
    interval, press the button below."
    })
  output$CIgraphs <- renderText({
    if(input$CIbutton > 0){
      "Here is the confidence interval graph:"
    }
  })
    
  

   
}

# Run the application 
shinyApp(ui = ui, server = server)
