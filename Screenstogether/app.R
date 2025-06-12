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
library(shinydashboard)

#data for table
number_subscribers_test <- 250
number_users_test <- 10000
number_subscribers_control <- 200
number_users_control <- 10000



ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Project"),
                  
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Overview", tabName = "Overview"),
                      menuItem("Feature 1", tabName = "Feature1"),
                      menuItem("Feature 1 results", tabName = "Feature1results"),
                      menuItem("Feature 2", tabName = "Feature2"),
                      menuItem("Feature 2 results", tabName = "Feature2results"),
                      menuItem("Feature 3", tabName = "Feature3"),
                      menuItem("Feature 3 results", tabName = "Feature3Results"),
                      menuItem("Feature 4", tabName = "Feature4"),
                      menuItem("Feature 4 results", tabName = "Feature4results"),
                      menuItem("One Year Later", tabName = "OneYearLater")
                    )
                  ),
                  
  dashboardBody(
                tabItems(
                
                  tabItem(tabName = "Overview", 
                           h1("MonoBingo"),
                           fluidRow(
                             box(
                               title = "Summary",
                               width = 12,
                               solidHeader = TRUE,
                               status = "primary",
                               textOutput("overview")
                             )
                           ),
                           br(),
                           br(),
                           fluidRow(
                             column(6,
                                    box(width = 6,
                                    title = "Instructions",
                                    uiOutput("instructions"),
                                    status = "primary",
                                    solidHeader = TRUE,
                                    )
                                    ),
                          
                             column(6,
                                    box(width = 6,
                                    title = "Purpose",
                                    textOutput("purpose"),
                                    status = "primary",
                                    solidHeader = TRUE,
                                    )
                           )
                          )
                          ),
                  tabItem(tabName = "Feature1",
                           
                           h1("Reducing hearts for free tier"),
                           fluidRow(
                             box(width = 5,
                                 title = "Information",
                                 status = "primary",
                                 solidHeader = TRUE,
                               textOutput("feature1description"),
                               br(),
                               textOutput("feature1description2"),
                               br(),
                               textOutput("feature1description3"),
                               br(),
                              textOutput("feature1description4")
                             ),
                             box(width = 7,
                                 title = "Choices",
                                 status = "primary",
                                 solidHeader = TRUE,
                               radioButtons("dayquestion", "How many days would you like to run the test?",
                                            choices =
                                              c(1, 2, 3, 4),
                                          selected = character(0)),
                               
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
                  
                  tabItem(tabName = "Feature1results",
                           h1("Reducing hearts for free tier"),
                           fluidRow(
                             box(
                               width = 4,
                               
                               status = "primary",
                               solidHeader = TRUE,
                               textOutput("press"),
                               br(),
                               actionButton("resultsbutton", "Press here for results!"),
                               br(),
                               br(),
                               textOutput("CI"),
                               br(),
                               actionButton("CIbutton", "Confidence Interval"),
                               br(),
                               br(),
                               textOutput("decision1"),
                               br(),
                               radioButtons("decision1", "Introduce feature 1?",
                                            choices = c("Yes", "No"), 
                                            selected = character(0))
                             ),
                             
                             
                             box(
                               width = 8,
                               title = "Results",
                               status = "primary",
                               solidHeader = TRUE,
                               uiOutput("results"),
                               tableOutput("resultdata"),
                               uiOutput("CInumbers"),
                               plotOutput("ciplot")
                               
                             )
                           )
                ),
                tabItem(tabName = "Feature2",
                         h1("Reducing wait time for subscribers"),
                         fluidRow(
                           box(width = 5,
                               
                               title = "Information",
                               status = "primary",
                               solidHeader = TRUE,
                             textOutput("feature2des1"),
                             br(),
                             textOutput("feature2des2"),
                             br(),
                             textOutput("feature2des3")
                           ),
                           box(width = 7,
                               
                               title = "Choices",
                               status = "primary",
                               solidHeader = TRUE,
                             radioButtons("dayquestion2", "How many days would you like to run the test?",
                                          choices =
                                            c(1, 2, 3, 4),
                                          selected = character(0)),
                             
                             sliderInput("samplesize2",
                                         "Choose your sample size:",
                                         min = 1000,
                                         max = 10000,
                                         value = 3000,
                                         step = 100),
                             textOutput("power2")
                           )
                         )
                         
                         
                         
                         
                ),
                tabItem(tabName = "Feature2results",
                         h1("Reducing wait time for subscribers"),
                         fluidRow(
                           box(width = 4,
                               
                               status = "primary",
                               solidHeader = TRUE,
                             textOutput("press2"),
                             br(),
                             actionButton("resultsbutton2", "Press here for results!"),
                             br(),
                             br(),
                             textOutput("CI2"),
                             br(),
                             actionButton("CIbutton2", "Confidence Interval"),
                             br(),
                             br(),
                             textOutput("decision2"),
                             br(),
                             radioButtons("decision2", "Introduce feature 2?",
                                          choices = c("Yes", "No"), 
                                          selected = character(0))
                           ),
                           
                           
                           box(width = 8,
                               title = "Results",
                               status = "primary",
                               solidHeader = TRUE,
                             uiOutput("results2"),
                             tableOutput("resultdata2"),
                             uiOutput("CInumbers2"),
                             plotOutput("ciplot2")
                             
                           )
                         )
                ),
                tabItem(tabName = "Feature3",
                         h1("Increasing adverts for free tier"),
                         fluidRow(
                           box(width = 5,
                               title = "Information",
                               solidHeader = TRUE,
                               status = "primary",
                                        textOutput("feature3des1"),
                                        br(),
                                        textOutput("feature3des2"),
                                        br(),
                                        textOutput("feature3des3")
                                        ),
                           box(width = 7,
                               
                               title = "Choices",
                               status = "primary",
                               solidHeader = TRUE,
                             radioButtons("dayquestion3", "How many days would you like to run the test?",
                                                  choices =
                                                    c(1, 2, 3, 4),
                                                  selected = character(0)),
                                     
                                     sliderInput("samplesize3",
                                                 "Choose your sample size:",
                                                 min = 1000,
                                                 max = 10000,
                                                 value = 3000,
                                                 step = 100),
                                     textOutput("power3"))
                         )
                         ),
                tabItem(tabName = "Feature3Results",
                         h1("Increasing adverts for free tier"),
                         fluidRow(
                           box(width = 4,
                               
                               status = "primary",
                               solidHeader = TRUE,
                             textOutput("press3"),
                             br(),
                             actionButton("resultsbutton3", "Press here for results!"),
                             br(),
                             br(),
                             textOutput("CI3"),
                             br(),
                             actionButton("CIbutton3", "Confidence Interval"),
                             br(),
                             br(),
                             textOutput("decision3"),
                             br(),
                             radioButtons("decision3", "Introduce feature 3?",
                                          choices = c("Yes", "No"), 
                                          selected = character(0))
                           ),
                           box(width = 8,
                               title = "Results",
                               status = "primary",
                               solidHeader = TRUE,
                             uiOutput("results3"),
                             tableOutput("resultdata3"),
                             uiOutput("CInumbers3"),
                             plotOutput("ciplot3")
                           )
                         )),
                tabItem(tabName = "Feature4",
                         h1("Introducing streaks for subscription users"),
                         fluidRow(
                           box(width = 5,
                               
                               title = "Information",
                               status = "primary",
                               solidHeader = TRUE,
                                        textOutput("feature4des1"),
                                        br(),
                                        textOutput("feature4des2"),
                                        br(),
                                        textOutput("feature4des3")
                           ),
                           box(width = 7,
                               
                               title = "Choices",
                               status = "primary",
                               solidHeader = TRUE,
                                     radioButtons("dayquestion4", "How many days would you like to run the test?",
                                                  choices =
                                                    c(1, 2, 3, 4),
                                                  selected = character(0)),
                                     
                                     sliderInput("samplesize4",
                                                 "Choose your sample size:",
                                                 min = 1000,
                                                 max = 10000,
                                                 value = 3000,
                                                 step = 100),
                                     textOutput("power4"))
                         )),
                tabItem(tabName = "Feature4results",
                         h1("Introducing streaks for subscription users"),
                         fluidRow(
                           box(width = 4,
                               
                               status = "primary",
                               solidHeader = TRUE,
                             textOutput("press4"),
                             br(),
                             actionButton("resultsbutton4", "Press here for results!"),
                             br(),
                             br(),
                             textOutput("CI4"),
                             br(),
                             actionButton("CIbutton4", "Confidence Interval"),
                             br(),
                             br(),
                             textOutput("decision4"),
                             br(),
                             radioButtons("decision4", "Introduce feature 4?",
                                          choices = c("Yes", "No"), 
                                          selected = character(0))
                           ),
                           box(width = 8,
                               title = "Results",
                               status = "primary",
                               solidHeader = TRUE,
                             uiOutput("results4"),
                             tableOutput("resultdata4"),
                             uiOutput("CInumbers4"),
                             plotOutput("ciplot4")
                           )
                         )),
                
                 tabItem(tabName = "OneYearLater",
                          h1("Status of MonoBingo one year later"),
                          fluidRow(
                            box(width = 5,
                               
                                title = "Information",
                                status = "primary",
                                solidHeader = TRUE,
                              textOutput("yearlater"),
                              br(),
                              actionButton("yearbutton", "One Year Later"),
                              br(),
                              br(),
                              uiOutput("featureschosen"),
                              br(),
                              uiOutput("chosenfeature1"),
                              uiOutput("chosenfeature2"),
                              uiOutput("chosenfeature3"),
                              uiOutput("chosenfeature4")
                            ),
                            
                            box(width = 7,
                                
                                title = "Results",
                                status = "primary",
                                solidHeader = TRUE,
                              uiOutput("yearresults"),
                              tableOutput("yeartable"),
                              uiOutput("yeartext")
                            )
                          ))
                
                
)
),

)



server <- function(input, output, session) {
  #screen 1 text
  
  output$overview <- renderText({
    "You are a product manager for MonoBingo. Your task is to select, develop and 
    release features which enhance the product. You can conduct A/B tests to help
    you determine whether or not to release a feature."
  })
  output$instructions <- renderUI({
    tags$ul(
      tags$li("Work through the features in order."),
      tags$li("Choose your test conditions and then look at the results tab for each."),
      tags$li("On each results tab decide if you want to introduce the feature."),
      tags$li("After testing each feature look at the status of MonoBingo one year later.")
    
    )
  })
  output$purpose <- renderText({
    "This is a simulation app being used to study the transfer of learning."
  })
  
  #screen 2 text
  
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
                              p1 = 0.02, 
                              p2 = 0.0266, 
                              sig.level = 0.05)
    paste0("The estimated power for your sample size is ",
           round(result$power * 100, 2), "%")
    
  })
  
  #screen 3 
  
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
  #screen 3 loading/text
  
  output$results <- renderUI({
    
    
    if(load()=="pressed") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (load() == "loaded"){
      daynumber <- as.numeric(input$dayquestion)
      sample <- input$samplesize * daynumber
      result <- power.prop.test(n = sample, 
                                p1 = 0.02, 
                                p2 = 0.0266, 
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
  
  #screen 3 CI
  
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
        "[",ci[1], "%, ", ci[2], "%]" ,
        br()
      )
    }
    
  })
  #Screen 3 CI graph
  
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
        x = "Difference",
        diff = rate[1]-rate[2],
        lower = ci[1],
        upper = ci[2]
      )
      ylim_min <- min(-0.5, ci[1] - 0.5)
      ylim_max <- max(1, ci[2] + 0.5)
      ggplot(cidata, aes(x = x, y = diff)) +
        geom_point(size = 5, color = "blue") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      width = 0.1, color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        ylim(ylim_min, ylim_max) +
        labs(
          title = "Graph to show the Confidence Interval",
          y = "Percentage Difference in Rate",
          x = "") +
        theme_minimal()
      
    }
    
  })
  #screen 3 decision
  
  output$decision1 <- renderText({
    "Now you've seen the results for this test you must decide if you want to introduce feature 1:
    reducing the number of hearts on the free tier."
  })

  
  #screen 5 feature 2
  
  output$feature2des1 <- renderText({
    "The second feature that can be installed is reducing the wait time for
    those who subscribe. Currently, the free tier requires 3 minutes wait between
    each round. This feature would mean those who subscribe only wait 30 seconds."
  })
  output$feature2des2 <- renderText({
    "It is thought this feature will increase subscriptions by 20%."
  })
  output$feature2des3 <- renderText({
    "You are going to test the effectiveness of this feature. Please choose the test 
    length and sample size by using the power calculator."
  })
  output$power2 <- renderText({
  daynumber2 <- as.numeric(input$dayquestion2)
  sample2 <- input$samplesize2 * daynumber2
  result2 <- power.prop.test(n = sample2, 
                            p1 = 0.02, 
                            p2 = 0.024, 
                            sig.level = 0.05)
  paste0("The estimated power for your sample size is ",
         round(result2$power * 100, 2), "%")
  })
  
  #screen 5 feature 2 results
  
  loadfeature2 <- reactiveVal("beforefeature2")
  
  
  
  observeEvent(input$resultsbutton2, {
    
    
    loadfeature2("pressedfeature2")
    
    
    
    later(function() {
      
      loadfeature2("loadedfeature2")
    },
    delay = 5)
  })
  
  
  output$press2 <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  output$resultdata2 <- renderTable({ 
    req(input$dayquestion2)
    days2 <- as.numeric(input$dayquestion2)
    
    #test
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize2)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize2)
    
    subscribers2 <- c(number_subscribers_test * days2 , number_subscribers_control * days2)
    users2 <- c(number_users_test * days2, number_users_control * days2)
    rate2 <- round((subscribers2/users2)* 100, 2)
    
    testresult2 <- prop.test(subscribers2, users2)
    p_val2 <- signif(testresult2$p.value, 3)
    
    
    #results table
    if (loadfeature2() == "loadedfeature2") 
      data.frame(
        Test = c("Subscribers", "Users", "Rate", "p-value"),
        Test_Group=c(number_subscribers_test * days2, number_users_test * days2, 
                     paste0(rate2[1], "%"),"-"),
        Control_Group = c(number_subscribers_control * days2, number_users_control * days2, 
                          paste0(rate2[2],"%"),"-"),
        Difference = c(number_subscribers_test * days2 - number_subscribers_control * days2,
                       "-", 
                       paste0(rate2[1]-rate2[2], "%"), "-"),
        P_Value = c("-", "-", "-", p_val2))
    
  })
  #screen 5 loading/text
  
  output$results2 <- renderUI({
    
    
    if(loadfeature2()=="pressedfeature2") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature2() == "loadedfeature2"){
      daynumber2 <- as.numeric(input$dayquestion2)
      sample2 <- input$samplesize2 * daynumber2
      result2 <- power.prop.test(n = sample2, 
                                p1 = 0.02, 
                                p2 = 0.024, 
                                sig.level = 0.05)
      resultpower2 <- round(result2$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("You chose to run the test for", input$dayquestion2, "day(s) and with
                a sample size of", input$samplesize2,". The power of your test is",
                resultpower2, "%."))
        
      )
    }
    else{
      NULL
    }
  })
  
  #screen 5 CI
  
  output$CI2 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers2 <- renderUI({
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize2)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize2)
    
    days2 <- as.numeric(input$dayquestion2)
    subscribers2 <- c(number_subscribers_test * days2 , number_subscribers_control * days2)
    users2 <- c(number_users_test * days2, number_users_control * days2)
    rate2 <- round((subscribers2/users2)* 100, 2)
    
    testresult2 <- prop.test(subscribers2, users2)
    p_val2 <- signif(testresult2$p.value, 3)
    
    
    
    ci2 <- signif(testresult2$conf.int * 100, 3)
    if(input$CIbutton2 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is:",
        br(),
        br(),
        "[",ci2[1], "%, ", ci2[2], "%]" ,
        br()
      )
    }
    
  })
  #Screen 5 CI graph
  
  output$ciplot2 <- renderPlot({
    if(input$CIbutton2 > 0){
      number_subscribers_test <- 250
      number_users_test <- as.numeric(input$samplesize2)
      number_subscribers_control <- 200
      number_users_control <- as.numeric(input$samplesize2)
      
      days2 <- as.numeric(input$dayquestion2)
      subscribers2 <- c(number_subscribers_test * days2 , number_subscribers_control * days2)
      users2 <- c(number_users_test * days2, number_users_control * days2)
      rate2 <- round((subscribers2/users2)* 100, 2)
      testresult2 <- prop.test(subscribers2, users2)
      ci2 <- signif(testresult2$conf.int * 100, 3)
      
      cidata2 <- data.frame(
        x = "Difference",
        diff2 = rate2[1]-rate2[2],
        lower2 = ci2[1],
        upper2 = ci2[2]
      )
      ylim_min2 <- min(-0.5, ci2[1] - 0.5)
      ylim_max2 <- max(1, ci2[2] + 0.5)
      ggplot(cidata2, aes(x = x, y = diff2)) +
        geom_point(size = 5, color = "blue") +
        geom_errorbar(aes(ymin = lower2, ymax = upper2),
                      width = 0.1, color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        ylim(ylim_min2, ylim_max2) +
        labs(
          title = "Graph to show the Confidence Interval",
          y = "Percentage Difference in Rate",
          x = "") +
        theme_minimal()
      
    }
    
  })
  #screen 5 feature 2 decision
  
  output$decision2 <- renderText({
    "Now you've seen the results for this test you must decide if you want to introduce feature 2:
    reducing the wait time for subscription."
  })
  
  #screen 6 feature 3
  
  output$feature3des1 <- renderText({
    "This potential feature is increasing the advert frequency for free tier users. Currently,
    there is an advert every 5 rounds. For free tier it will change to every 3 rounds."
  })
  output$feature3des2 <- renderText({
    "It is thought that increasing adverts for free tier will cause 40% more subscribers."
  })
  output$feature3des3 <- renderText({
    "Test the effectiveness of this feature before you decide whether to introduce it. Choose length
    of test and the sample size by using the power calculator."
  })
  output$power3 <- renderText({
    daynumber3 <- as.numeric(input$dayquestion3)
    sample3 <- input$samplesize3 * daynumber3
    result3 <- power.prop.test(n = sample3, 
                               p1 = 0.02, 
                               p2 = 0.028, 
                               sig.level = 0.05)
    paste0("The estimated power for your sample size is ",
           round(result3$power * 100, 2), "%")
  })
  #screen 7 feature 3 results
  
  
  
  loadfeature3 <- reactiveVal("beforefeature3")
  
  
  
  observeEvent(input$resultsbutton3, {
    
    
    loadfeature3("pressedfeature3")
    
    
    
    later(function() {
      
      loadfeature3("loadedfeature3")
    },
    delay = 5)
  })
  
  
  output$press3 <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  output$resultdata3 <- renderTable({ 
    req(input$dayquestion3)
    days3 <- as.numeric(input$dayquestion3)
    
    #test
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize3)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize3)
    
    subscribers3 <- c(number_subscribers_test * days3 , number_subscribers_control * days3)
    users3 <- c(number_users_test * days3, number_users_control * days3)
    rate3 <- round((subscribers3/users3)* 100, 2)
    
    testresult3 <- prop.test(subscribers3, users3)
    p_val3 <- signif(testresult3$p.value, 3)
    
    
    #results table
    if (loadfeature3() == "loadedfeature3") 
      data.frame(
        Test = c("Subscribers", "Users", "Rate", "p-value"),
        Test_Group=c(number_subscribers_test * days3, number_users_test * days3, 
                     paste0(rate3[1], "%"),"-"),
        Control_Group = c(number_subscribers_control * days3, number_users_control * days3, 
                          paste0(rate3[2],"%"),"-"),
        Difference = c(number_subscribers_test * days3 - number_subscribers_control * days3,
                       "-", 
                       paste0(rate3[1]-rate3[2], "%"), "-"),
        P_Value = c("-", "-", "-", p_val3))
    
  })
  #screen 7 loading/text
  
  output$results3 <- renderUI({
    
    
    if(loadfeature3()=="pressedfeature3") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature3() == "loadedfeature3"){
      daynumber3 <- as.numeric(input$dayquestion3)
      sample3 <- input$samplesize3 * daynumber3
      result3 <- power.prop.test(n = sample3, 
                                 p1 = 0.02, 
                                 p2 = 0.028, 
                                 sig.level = 0.05)
      resultpower3 <- round(result3$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("You chose to run the test for", input$dayquestion3, "day(s) and with
                a sample size of", input$samplesize3,". The power of your test is",
                resultpower3, "%."))
        
      )
    }
    else{
      NULL
    }
  })
  
  #screen 7 CI
  
  output$CI3 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers3 <- renderUI({
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize3)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize3)
    
    days3 <- as.numeric(input$dayquestion3)
    subscribers3 <- c(number_subscribers_test * days3 , number_subscribers_control * days3)
    users3 <- c(number_users_test * days3, number_users_control * days3)
    rate3 <- round((subscribers3/users3)* 100, 2)
    
    testresult3 <- prop.test(subscribers3, users3)
    p_val3 <- signif(testresult3$p.value, 3)
    
    
    
    ci3 <- signif(testresult3$conf.int * 100, 3)
    if(input$CIbutton3 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is:",
        br(),
        br(),
        "[",ci3[1], "%, ", ci3[2], "%]" ,
        br()
      )
    }
    
  })
  #Screen 7 CI graph
  
  output$ciplot3 <- renderPlot({
    if(input$CIbutton3 > 0){
      number_subscribers_test <- 250
      number_users_test <- as.numeric(input$samplesize3)
      number_subscribers_control <- 200
      number_users_control <- as.numeric(input$samplesize3)
      
      days3 <- as.numeric(input$dayquestion3)
      subscribers3 <- c(number_subscribers_test * days3 , number_subscribers_control * days3)
      users3 <- c(number_users_test * days3, number_users_control * days3)
      rate3 <- round((subscribers3/users3)* 100, 2)
      testresult3 <- prop.test(subscribers3, users3)
      ci3 <- signif(testresult3$conf.int * 100, 3)
      
      cidata3 <- data.frame(
        x = "Difference",
        diff3 = rate3[1]-rate3[2],
        lower3 = ci3[1],
        upper3 = ci3[2]
      )
      ylim_min3 <- min(-0.5, ci3[1] - 0.5)
      ylim_max3 <- max(1, ci3[2] + 0.5)
      ggplot(cidata3, aes(x = x, y = diff3)) +
        geom_point(size = 5, color = "blue") +
        geom_errorbar(aes(ymin = lower3, ymax = upper3),
                      width = 0.1, color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        ylim(ylim_min3, ylim_max3) +
        labs(
          title = "Graph to show the Confidence Interval",
          y = "Percentage Difference in Rate",
          x = "") +
        theme_minimal()
      
    }
    
  })
  #screen 7 feature 3 decision
  
  output$decision3 <- renderText({
    "Now you've seen the results for this test you must decide if you want to introduce feature 3:
    increasing adverts for the free tier."
  })
  
  
  #screen 8 feature 4
  
  output$feature4des1 <- renderText({
    "This feature is introducing streaks to subscribers. Currently, free tier do not have this
    feature. This will give subscribers a way to track progress"
  })
  output$feature4des2 <- renderText({
    "It is thought that introducing this will cause 25% more subscribers."
  })
  output$feature4des3 <- renderText({
    "Test the effectiveness of this feature before you decide whether to introduce it. Choose length
    of test and the sample size by using the power calculator."
  })
  output$power4 <- renderText({
    daynumber4 <- as.numeric(input$dayquestion4)
    sample4 <- input$samplesize4 * daynumber4
    result4 <- power.prop.test(n = sample4, 
                               p1 = 0.02, 
                               p2 = 0.025, 
                               sig.level = 0.05)
    paste0("The estimated power for your sample size is ",
           round(result4$power * 100, 2), "%")
  })
  #screen 9 feature 4 results
  
  
  
  loadfeature4 <- reactiveVal("beforefeature4")
  
  
  
  observeEvent(input$resultsbutton4, {
    
    
    loadfeature4("pressedfeature4")
    
    
    
    later(function() {
      
      loadfeature4("loadedfeature4")
    },
    delay = 5)
  })
  
  
  output$press4 <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  output$resultdata4 <- renderTable({ 
    req(input$dayquestion4)
    days4 <- as.numeric(input$dayquestion4)
    
    #test
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize4)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize4)
    
    subscribers4 <- c(number_subscribers_test * days4 , number_subscribers_control * days4)
    users4 <- c(number_users_test * days4, number_users_control * days4)
    rate4 <- round((subscribers4/users4)* 100, 2)
    
    testresult4 <- prop.test(subscribers4, users4)
    p_val4 <- signif(testresult4$p.value, 3)
    
    
    #results table
    if (loadfeature4() == "loadedfeature4") 
      data.frame(
        Test = c("Subscribers", "Users", "Rate", "p-value"),
        Test_Group=c(number_subscribers_test * days4, number_users_test * days4, 
                     paste0(rate4[1], "%"),"-"),
        Control_Group = c(number_subscribers_control * days4, number_users_control * days4, 
                          paste0(rate4[2],"%"),"-"),
        Difference = c(number_subscribers_test * days4 - number_subscribers_control * days4,
                       "-", 
                       paste0(rate4[1]-rate4[2], "%"), "-"),
        P_Value = c("-", "-", "-", p_val4))
    
  })
  #screen 7 loading/text
  
  output$results4 <- renderUI({
    
    
    if(loadfeature4()=="pressedfeature4") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature4() == "loadedfeature4"){
      daynumber4 <- as.numeric(input$dayquestion4)
      sample4 <- input$samplesize4 * daynumber4
      result4 <- power.prop.test(n = sample4, 
                                 p1 = 0.02, 
                                 p2 = 0.025, 
                                 sig.level = 0.05)
      resultpower4 <- round(result4$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("You chose to run the test for", input$dayquestion4, "day(s) and with
                a sample size of", input$samplesize4,". The power of your test is",
                resultpower4, "%."))
        
      )
    }
    else{
      NULL
    }
  })
  
  #screen 9 CI
  
  output$CI4 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers4 <- renderUI({
    #data for table
    number_subscribers_test <- 250
    number_users_test <- as.numeric(input$samplesize4)
    number_subscribers_control <- 200
    number_users_control <- as.numeric(input$samplesize4)
    
    days4 <- as.numeric(input$dayquestion4)
    subscribers4 <- c(number_subscribers_test * days4 , number_subscribers_control * days4)
    users4 <- c(number_users_test * days4, number_users_control * days4)
    rate4 <- round((subscribers4/users4)* 100, 2)
    
    testresult4 <- prop.test(subscribers4, users4)
    p_val4 <- signif(testresult4$p.value, 3)
    
    
    
    ci4 <- signif(testresult4$conf.int * 100, 3)
    if(input$CIbutton4 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is:",
        br(),
        br(),
        "[",ci4[1], "%, ", ci4[2], "%]" ,
        br()
      )
    }
    
  })
  #Screen 9 CI graph
  
  output$ciplot4 <- renderPlot({
    if(input$CIbutton4 > 0){
      number_subscribers_test <- 250
      number_users_test <- as.numeric(input$samplesize4)
      number_subscribers_control <- 200
      number_users_control <- as.numeric(input$samplesize4)
      
      days4 <- as.numeric(input$dayquestion4)
      subscribers4 <- c(number_subscribers_test * days4 , number_subscribers_control * days4)
      users4 <- c(number_users_test * days4, number_users_control * days4)
      rate4 <- round((subscribers4/users4)* 100, 2)
      testresult4 <- prop.test(subscribers4, users4)
      ci4 <- signif(testresult4$conf.int * 100, 3)
      
      cidata4 <- data.frame(
        x = "Difference",
        diff4 = rate4[1]-rate4[2],
        lower4 = ci4[1],
        upper4 = ci4[2]
      )
      ylim_min4 <- min(-0.5, ci4[1] - 0.5)
      ylim_max4 <- max(1, ci4[2] + 0.5)
      ggplot(cidata4, aes(x = x, y = diff4)) +
        geom_point(size = 5, color = "blue") +
        geom_errorbar(aes(ymin = lower4, ymax = upper4),
                      width = 0.1, color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        ylim(ylim_min4, ylim_max4) +
        labs(
          title = "Graph to show the Confidence Interval",
          y = "Percentage Difference in Rate",
          x = "") +
        theme_minimal()
      
    }
    
  })
  #screen 9 feature 4 decision
  
  output$decision4 <- renderText({
    "Now you've seen the results for this test you must decide if you want to introduce feature 4:
    streaks for subscribers."
  })
  #final screen , a year later
  
  output$yearlater <- renderText({
    "We now look at the status of MonoBingo a year after deciding
    which features to introduce. 
    To see the number of users and subscribers now, a year later, click the button below."
  })  
  
  output$featureschosen <- renderUI({
    tagList(
    h4("Below is the list of features you chose to introduce to MonoBingo: ")
    )
  })
  
  output$chosenfeature1 <- renderUI({
    if (input$decision1 == "Yes"){
      "Reducing number of hearts for free tier"
    }
    else {NULL}
    })
    output$chosenfeature2 <- renderUI({
    if (input$decision2 == "Yes"){
      "Reducing wait time for subscription"
    }
      else {NULL}
      })
    output$chosenfeature3 <- renderUI({
      if (input$decision3 == "Yes") {
        "Increasing adverts for free tier"
      }
    })
    output$chosenfeature4 <- renderUI({
      if (input$decision4 == "Yes"){
        "Introducing streaks for subscribers"
      }
    })
    
  
  load2 <- reactiveVal("before2")
  
  observeEvent(input$yearbutton, {
    load2("pressed2")
    later(function(){
      load2("loaded2")
    },
    delay=5)
  })
  output$yeartable <-renderTable({ 
    
    number_subscribers_test2 <- 600
    number_users_test2 <- 20000
    number_subscribers_control2 <- 500
    number_users_control2 <- 22000 
    answers <- c(input$decision1, input$decision2, input$decision3, input$decision4)
    yesanswers <- sum(answers == "Yes", na.rm = TRUE)
    
    
    if (load2() == "loaded2" && yesanswers >= 1){
      
      data.frame( 
        Yearlater = c("Subscribers", "Users"),
        Test_Group = c(number_subscribers_test2, number_users_test2),
        Control_Group = c(number_subscribers_control2, number_users_control2)
      )
    }
    else if (load2() == "loaded2" && yesanswers == 0) {
      data.frame(
        Yearlater = c("Subscribers", "Users"),
        Number = c(number_subscribers_control2, number_users_control2)
      )
    }
    
    else {NULL}
  })
  output$yearresults <- renderUI({
    
    answers <- c(input$decision1, input$decision2, input$decision3, input$decision4)
    yesanswers <- sum(answers == "Yes", na.rm = TRUE)
    
    
    if(load2()=="pressed2"){
      tagList(
        h3("Loading"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (load2() == "loaded2"){
      tagList(
        p(" You chose to introduce", yesanswers, "features."),
        p("Here are the number of subscribers and users one year later:") 
      )
    }
    else{
      NULL
    }
   })
  output$yeartext <- renderUI({
    if (load2() == "loaded2"){
      if(input$decision1 == "Yes"){
        p("While reducing the number of hearts for the free tier increased subscriber
        numbers, it also caused a descrease in the number of users.")}
      else { NULL}
    }
    else{
      NULL
    }
  })

}
# Run the application 
shinyApp(ui = ui, server = server)

