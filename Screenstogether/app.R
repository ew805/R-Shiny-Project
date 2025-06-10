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
                (tabsetPanel(
                  tabPanel("Overview", 
                           h3("MonoBingo"),
                           textOutput("overview")),
                  tabPanel("Feature 1",
                           
                           h3("Reducing hearts for free tier"),
                           sidebarLayout(
                             sidebarPanel(width = 5,
                               textOutput("feature1description"),
                               br(),
                               textOutput("feature1description2"),
                               br(),
                               textOutput("feature1description3"),
                               br(),
                              textOutput("feature1description4")
                             ),
                             mainPanel(width = 7,
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
                               actionButton("CIbutton", "Confidence Interval"),
                               br(),
                               br(),
                               textOutput("decision1"),
                               br(),
                               radioButtons("decision1", "Introduce feature 1?",
                                            choices = c("Yes", "No"), 
                                            selected = character(0))
                             ),
                             
                             
                             mainPanel(
                               uiOutput("results"),
                               tableOutput("resultdata"),
                               uiOutput("CInumbers"),
                               plotOutput("ciplot")
                               
                             )
                           )
                ),
                tabPanel("Feature 2",
                         h3("Reducing wait time for subscribers"),
                         sidebarLayout(
                           sidebarPanel(width = 5,
                             textOutput("feature2des1"),
                             br(),
                             textOutput("feature2des2"),
                             br(),
                             textOutput("feature2des3")
                           ),
                           mainPanel(width = 7,
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
                tabPanel("Feature 2 results",
                         h3("Reducing wait time for subscribers"),
                         sidebarLayout(
                           sidebarPanel(
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
                           
                           
                           mainPanel(
                             uiOutput("results2"),
                             tableOutput("resultdata2"),
                             uiOutput("CInumbers2"),
                             plotOutput("ciplot2")
                             
                           )
                         )
                ),
                tabPanel("Feature 3",
                         h3("Increasing adverts for free tier"),
                         sidebarLayout(
                           sidebarPanel(width = 5,
                                        textOutput("feature3des1"),
                                        br(),
                                        textOutput("feature3des2"),
                                        br(),
                                        textOutput("feature3des3")
                                        ),
                           mainPanel()
                         )
                         ),
                tabPanel("Feature 3 Results",
                         h3("Increasing adverts for free tier"),
                         sidebarLayout(
                           sidebarPanel(
                             textOutput("press3")
                           ),
                           mainPanel()
                         )),
                
                 tabPanel("One Year Later",
                          h3("Status of MonoBingo one year later"),
                          sidebarLayout(
                            sidebarPanel(width = 5,
                              textOutput("yearlater"),
                              br(),
                              actionButton("yearbutton", "One Year Later"),
                              br(),
                              br(),
                              uiOutput("featureschosen"),
                              br(),
                              uiOutput("chosenfeature1"),
                              uiOutput("chosenfeature2")
                            ),
                            
                            mainPanel(width = 5,
                              uiOutput("yearresults"),
                              tableOutput("yeartable"),
                              uiOutput("yeartext")
                            )
                          )),
                
                
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
  
  output$feature2des1 <- renderText(
    "The second feature that can be installed is reducing the wait time for
    those who subscribe. Currently, the free tier requires 3 minutes wait between
    each round. This feature would mean those who subscribe only wait 30 seconds."
  )
  output$feature2des2 <- renderText(
    "It is thought this feature will increase subscriptions by 20%."
  )
  output$feature2des3 <- renderText(
    "You are going to test the effectiveness of this feature. Please choose the test 
    length and sample size by using the power calculator."
  )
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
      number_users_test <- as.numeric(input$samplesize)
      number_subscribers_control <- 200
      number_users_control <- as.numeric(input$samplesize)
      
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
    "It is thought that increasing adverts for free tier will cause 25% more subscribers."
  })
  output$feature3des3 <- renderText({
    "Test the effectiveness of this feature before you decide whether to introduce it. Choose length
    of test and the sample size by using the power calculator."
  })
  
  #screen 7 feature 3 results
  
  output$press3 <- renderText({
    "Press button below to view the results of your test"
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
    
    
    if (load2() == "loaded2" && input$decision1 == "Yes"){
      
      data.frame( 
        Yearlater = c("Subscribers", "Users"),
        Test_Group = c(number_subscribers_test2, number_users_test2),
        Control_Group = c(number_subscribers_control2, number_users_control2)
      )
    }
    else if (load2() == "loaded2" && input$decision1 == "No") {
      data.frame(
        Yearlater = c("Subscribers", "Users"),
        Number = c(number_subscribers_control2, number_users_control2)
      )
    }
    
    else {NULL}
  })
  output$yearresults <- renderUI({
    if(load2()=="pressed2"){
      tagList(
        h3("Loading"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (load2() == "loaded2"){
      if (input$decision1 == "Yes") {
        p(" You chose to introduce the feature.
        Here are the number of subscribers and users one year later:") }
      else {
        p(" You chose not to introduce this feature.")
      }
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

