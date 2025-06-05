#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(later)

ui <- fluidPage(

    
    titlePanel("Results"),
    h4("Reducing hearts for free tier"),

    
    sidebarLayout(
        sidebarPanel(
          textOutput("press"),
          actionButton("resultsbutton", "Press here for results!"),
          textOutput("CI"),
          actionButton("CIbutton", "graphed confidence intervals")
        ),

       
        mainPanel(
          uiOutput("results"),
          textOutput("CIgraphs")
           
        )
    )
)


server <- function(input, output, session) {
  load <- reactiveVal("before")
  
  
  
  observeEvent(input$resultsbutton, {
    
    
    load("pressed")
    
  
    
  later(function() {
    
    load("loaded")
    },
    delay = 5)
  })
      
  
  output$press <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  output$results <- renderUI({
    
    
    if(load()=="pressed") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "70px")
      )
    }
    else if (load() == "loaded"){
      h3("These are the results of your test:")
    }
    else{
      NULL
    }
    })
  
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
