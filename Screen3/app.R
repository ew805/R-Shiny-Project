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

#data for table
number_subscribers_test <- 200
number_users_test <- 10000
number_subscribers_control <- 250
number_users_control <- 10000


  
#test
subscribers <- c(number_subscribers_test, number_subscribers_control)
users <- c(number_users_test, number_users_control)
rate <- round((subscribers/users)* 100, 2)
  
testresult <- prop.test(subscribers, users)
p_val <- signif(testresult$p.value, 3)

#results table

resultdata <- data.frame(
  Test = c("Subscribers", "Users", "Rate", "p-value"),
    Test_Group=c(number_subscribers_test, number_users_test, 
                 paste0(rate[1], "%"),"-"),
  Control_Group = c(number_subscribers_control, number_users_control, 
                    paste0(rate[2],"%"),"-"),
  Difference = c(number_subscribers_test - number_subscribers_control, "-", 
                 paste0(rate[1]-rate[2], "%"), "-"),
  P_Value = c("-", "-", "-", p_val)
  
)

ui <- fluidPage(

    
    titlePanel("Results"),
    h4("Reducing hearts for free tier"),

    
    sidebarLayout(
        sidebarPanel(
          textOutput("press"),
          br(),
          actionButton("resultsbutton", "Press here for results!"),
          br(),
          br(),
          textOutput("CI"),
          br(),
          actionButton("CIbutton", "graphed confidence intervals")
        ),

       
        mainPanel(
          uiOutput("results"),
          tableOutput("resultdata"),
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
  
  output$resultdata <- renderTable({ 
    if (load() == "loaded")
    resultdata
  })
  output$results <- renderUI({
    
    
    if(load()=="pressed") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "70px")
      )
    }
    else if (load() == "loaded"){
      tagList(
      h3("These are the results of your test:"),
      p("You chose to run the test for x days")
      )
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
