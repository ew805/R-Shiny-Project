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
library(ggplot2)

#data for table
number_subscribers_test <- 250
number_users_test <- 10000
number_subscribers_control <- 200
number_users_control <- 10000



ui <- fluidPage("Project",
                tabsetPanel(
                  tabPanel("Overview"),
                  tabPanel("Feature 1",
                           
                           h3("Reducing hearts for free tier"),
                           sidebarLayout(
                             sidebarPanel(
                               textOutput("feature1description"),
                               textOutput("feature1description2"),
                               textOutput("feature1description3"),
                              textOutput("feature1description4")
                             ),
                             mainPanel(
                               radioButtons("dayquestion", "How many days would you like to run the test?",
                                            choices =
                                              c(1, 2, 3, 4)),
                               
                               sliderInput("samplesize",
                                           "Choose your sample size:",
                                           min = 1000,
                                           max = 10000,
                                           value = 3000,
                                           step = 100),
                               textOutput("power")
                             )
                             
                           ),
                  ),
                  
                  tabPanel("Feature 1 results",
                           h3("Reducing hearts for free tier"),
                           sidebarLayout(
                             sidebarPanel(
                               textOutput("press"),
                               br(),
                               actionButton("resultsbutton", "Press here for results!"),
                               br(),
                               br(),
                               textOutput("CI"),
                               br(),
                               actionButton("CIbutton", "Confidence Interval")
                             ),
                             
                             
                             mainPanel(
                               uiOutput("results"),
                               tableOutput("resultdata"),
                               uiOutput("CInumbers"),
                               plotOutput("ciplot")
                               
                             )
                           )
                )

    

    
),
)


server <- function(input, output, session) {
  
  
  output$feature1description <- renderText({ 
    "You are going to reduce the number of hearts on the free tier from 5 to 3.
    This should force more users to upgrade."
  })
  
  output$feature1description2 <- renderText({ "Currently, you gain 300 users a day 
   and 15 subscribers. It is thought that this feature will increase subscription 
     starts by 33%."
  })
  
  output$feature1description3 <- renderText({"You are going to test the effectiveness
     of this feature. Choose the number of days you want to test this feature for."})
  
  output$feature1description4 <- renderText({"Use the power calculator to help you choose
    the sample size for the test."})
  
  #power result
  
  
  output$power <- renderText({
    daynumber <- as.numeric(input$dayquestion)
    sample <- input$samplesize * daynumber
    result <- power.prop.test(n = sample, 
                              p1 = 0.025, 
                              p2 = 0.02, 
                              sig.level = 0.05)
    paste0("The estimated power for your sample size is ",
           round(result$power * 100, 2), "%")
    
  })
  
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
    req(input$dayquestion)
    days <- as.numeric(input$dayquestion)
    
    #test
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize)
    
    subscribers <- c(number_subscribers_test * days , number_subscribers_control * days)
    users <- c(number_users_test * days, number_users_control * days)
    rate <- round((subscribers/users)* 100, 2)
    
    testresult <- prop.test(subscribers, users)
    p_val <- signif(testresult$p.value, 3)
    
    
    #results table
    if (load() == "loaded") 
    data.frame(
      Test = c("Subscribers", "Users", "Rate", "p-value"),
      Test_Group=c(number_subscribers_test * days, number_users_test * days, 
                   paste0(rate[1], "%"),"-"),
      Control_Group = c(number_subscribers_control * days, number_users_control * days, 
                        paste0(rate[2],"%"),"-"),
      Difference = c(number_subscribers_test * days - number_subscribers_control * days,
                     "-", 
                     paste0(rate[1]-rate[2], "%"), "-"),
      P_Value = c("-", "-", "-", p_val))
    
  })
  output$results <- renderUI({
    
    
    if(load()=="pressed") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "70px")
      )
    }
    else if (load() == "loaded"){
      daynumber <- as.numeric(input$dayquestion)
      sample <- input$samplesize * daynumber
      result <- power.prop.test(n = sample, 
                                p1 = 0.025, 
                                p2 = 0.02, 
                                sig.level = 0.05)
      resultpower <- round(result$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("You chose to run the test for", input$dayquestion, "day(s) and with
                a sample size of", input$samplesize,". The power of your test is",
                resultpower, "%."))
       
      )
    }
    else{
      NULL
    }
  })
  
  output$CI <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers <- renderUI({
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize)
    
    days <- as.numeric(input$dayquestion)
    subscribers <- c(number_subscribers_test * days , number_subscribers_control * days)
    users <- c(number_users_test * days, number_users_control * days)
    rate <- round((subscribers/users)* 100, 2)
    
    testresult <- prop.test(subscribers, users)
    p_val <- signif(testresult$p.value, 3)
    
    
    
    ci <- signif(testresult$conf.int * 100, 3)
    if(input$CIbutton > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is:",
        br(),
        br(),
        "[",ci[1], "%, ", ci[2], "%]" 
      )
    }
    
  })
  
  output$ciplot <- renderPlot({
    if(input$CIbutton > 0){
      number_subscribers_test <- 250
      number_users_test <- as.numeric(input$samplesize)
      number_subscribers_control <- 200
      number_users_control <- as.numeric(input$samplesize)
      
      days <- as.numeric(input$dayquestion)
      subscribers <- c(number_subscribers_test * days , number_subscribers_control * days)
      users <- c(number_users_test * days, number_users_control * days)
      rate <- round((subscribers/users)* 100, 2)
      testresult <- prop.test(subscribers, users)
      ci <- signif(testresult$conf.int * 100, 3)
      
      cidata <- data.frame(
        x = "title",
        diff = rate[1]-rate[2],
        lower = ci[1],
        upper = ci[2]
      )
      ylim_min <- min(-0.1, ci[1] - 0.05)
      ylim_max <- max(0.1, ci[2] + 0.05)
      ggplot(cidata, aes(x = x, y = diff)) +
        geom_point(size = 5, color = "blue") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      width = 0.1, color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        ylim(ylim_min, ylim_max) +
        labs(
          title = "title",
          y = "yaxis",
          x = "xaxis") +
        theme_minimal()
      
    }
    
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

