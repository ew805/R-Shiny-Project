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
library(sortable)
library(googlesheets4)
source("Rutils.R")

conn <- dbConnect(SQLite(), "sims.sqlite")
dbExecute(conn, "DROP TABLE IF EXISTS simulation")

##setting up google sheet to hold survey results
gs4_auth(path = "rshinyproject-462813-5ff788b1a509.json")
sheet_id <- "1NKqU0eYYXZPLW8OMQOzfXsmMUe39A95HOU2hAZM8bLo"

#pretrial simulation 
pretrialusers <- rep(400, 366)
for (i in c(-364:0)){
  dayta <- day_sim(pretrialusers[i + 366], 60, 180, i, "pretrial", 
                   create_subscription_decision(0.1))
  dbWriteTable(conn, "sim", dayta, append = TRUE)
}

#data for company metrics plots
#df_wide <- dbGetQuery(conn, daily_query, list(-30, -1))

#each feature rate
featurerates <- c(A = 0.33, B = 0.2, C = 0.4, D = 0.25, E = 0.18, F = 0.29)


#set up for ordering later on
inputrank <- c("decision1", "decision2", "decision3", "decision4", "decision5", "decision6")
labelsrank <- c("Feature 1: Reducing Hearts", "Feature 2: Reducing Wait Time",
                "Feature 3: Increasing Adverts", "Feature 4: Introducing Streaks",
                "Feature 5: Subscriber Exclusive Level", "Feature 6: Free Trial")

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Project"),
                  
                  dashboardSidebar(
                    sidebarMenu(
                      id = "menu",
                      menuItem("Overview", tabName = "Overview", icon = icon("home")),
                      menuItem("Company Metrics", tabName = "companymetrics", icon = icon("square-poll-vertical")),
                      menuItem("Feature 1", icon = icon("heart"),
                               menuSubItem("Choices", tabName = "Feature1", icon = icon("cogs")),
                               menuSubItem("Results", tabName = "Feature1results", icon = icon("chart-line"))),
                      menuItem("Feature 2", icon = icon("hourglass-start"),
                               menuSubItem("Choices", tabName = "Feature2", icon = icon("cogs")),
                               menuSubItem("Results", tabName = "Feature2results", icon = icon("chart-line"))),
                      menuItem("Feature 3", icon = icon("rectangle-ad"),
                               menuSubItem("Choices", tabName = "Feature3", icon = icon("cogs")),
                               menuSubItem("Results", tabName = "Feature3Results", icon = icon("chart-line"))),
                      menuItem("Feature 4", icon = icon("fire"),
                               menuSubItem("Choices", tabName = "Feature4", icon = icon("cogs")),
                               menuSubItem("Results", tabName = "Feature4results", icon = icon("chart-line"))),
                      menuItem("Feature 5", icon = icon("gamepad"),
                               menuSubItem("Choices", tabName = "Feature5", icon = icon("cogs")),
                               menuSubItem("Results", tabName = "Feature5results", icon = icon("chart-line"))),
                      menuItem("Feature 6", icon = icon("gift"),
                               menuSubItem("Choices", tabName = "Feature6", icon = icon("cogs")),
                               menuSubItem("Results", tabName = "Feature6results", icon = icon("chart-line"))),
                      menuItem("Order the features", tabName = "orderfeatures", icon = icon("sort")),
                      menuItem("One Year Later", tabName = "OneYearLater", icon = icon("chart-line"))
                    )
                  ),
                  
  dashboardBody(
                tabItems(
                
                  tabItem(tabName = "Overview", 
                           h2("MonoBingo", align = "center", style = "font-weight: bold"),
                          br(),
                          br(),
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
                                    box(width = 12,
                                    title = "Instructions",
                                    uiOutput("instructions"),
                                    status = "primary",
                                    solidHeader = TRUE
                                    )
                                    ),
                          
                             column(6,
                                    box(width = 12,
                                    title = "Purpose",
                                    uiOutput("purpose"),
                                    status = "primary",
                                    solidHeader = TRUE
                                    ) ) ),
                          fluidRow(
                            column(12, align = "right",
                                   actionButton("next0", "Next Page",
                                                style = "margin-top: 20px; 
                                                margin-right: 10px; 
                                                padding: 10px 20px")
                            )
                          )
                  ),    
                                    
                                        
                                      
                                      
                           
                          
                  tabItem(tabName = "companymetrics",
                          h2("MonoBingo Company Metrics", align = "center", style = "font-weight: bold"),
                          br(),
                          fluidRow(
                            
                          column(width = 3,
                                 valueBoxOutput("cm_users", width = 12),
                                ),
                          column(width = 3,
                                 valueBoxOutput("cm_subscribers", width = 12),
                                 valueBoxOutput("cm_subscribers2", width = 12),
                                ),
                          column(width = 3,
                                 valueBoxOutput("cm_subscribers2", width = 12),
                                 ),
                          column(width = 3,
                                 valueBoxOutput("cm_cr", width = 12)
                                 )),
                          fluidRow(
                            column(width = 4,
                                   plotOutput("cmplot1")),
                            column(width = 4,
                                   plotOutput("cmplot2")),
                            column(width = 4,
                                   box(width=12,
                                       plotOutput("cmplot3")
                                   ))
                          ),
                          fluidRow(
                            column(width=6, 
                                   align = "left",
                                   actionButton("previous1", "Previous Page",
                                                style = "margin-top: 20px; padding: 10px 20px;")
                            ),
                            column(width=6, 
                                   align = "right",
                                   actionButton("next1", "Next Page",
                                                style = "margin-top: 20px; padding: 10px 20px;")
                            )
                          )),
                  
                  
                  tabItem(tabName = "Feature1",
                           
                           h2("Reducing Hearts on the Free Tier",align = "center", style = "font-weight: bold"),
                          br(),
                           fluidRow(
                             column(width = 4,
                                    box(width=12,
                                 title = "Information",
                                 status = "primary",
                                 solidHeader = TRUE,
                               textOutput("feature1description")
                               ),
                               box(width = 12,
                                   title = "Expected Rate",
                                   status = "primary",
                                   solidHeader = TRUE,
                               textOutput("feature1description2")
                               ),
                               box(width = 12,
                                   title = "Instructions",
                                   status = "primary",
                                   solidHeader = TRUE,
                               textOutput("feature1description3")
                               )
                               ),
                             column(width = 8,
                                    box( width = 12,
                                         style = "min-height: 424px;",
                                 title = "Choices",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 selectInput(
                                   inputId = "sl1",                     
                                   label = "Choose your significance level:",               
                                   choices = list(
                                     "1%" = 0.01,
                                     "5%" = 0.05,
                                     "10%" = 0.10
                                   ),
                                   selected = 0.05                             
                                 ),
                                 br(),
                               radioButtons("dayquestion", 
                                            "How many days would you like to run the test?",
                                            choices =
                                              c(1, 2, 3, 4),
                                          selected = character(0)),
                               br(),
                               sliderInput("samplesize",
                                           "Choose your sample size:",
                                           min = 1000,
                                           max = 10000,
                                           value = 3000,
                                           step = 100),
                               br(),
                               textOutput("power"))
                             ) ) ,
                          fluidRow(
                            column(width=6, 
                                   align = "left",
                                   actionButton("previous2", "Previous Page",
                                                style = "margin-top: 20px; padding: 10px 20px;")
                            ),
                            column(width=6, 
                                   align = "right",
                                   actionButton("next2", "Next Page",
                                                style = "margin-top: 20px; padding: 10px 20px;")
                            )
                          )
                               
                             
                             
                           
                  ),
                  
                  tabItem(tabName = "Feature1results",
                           h2("Reducing hearts on the Free Tier", align = "center", style = "font-weight: bold"),
                          br(),
                           fluidRow(
                             column(
                               width = 4,
                               box(width=12,
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
                                            choices = c("Yes" = TRUE, "No" = FALSE), 
                                            selected = character(0)),
                               textInput("surveyquestion1", "Why did you make that decision?", 
                                         value = "")
                               
                             ) ),
                             
                             
                             column(
                               width = 8,
                               tabBox(width=12,
                                      
                                      id = "feature1tabs",
                               tabPanel(
                                 title = "Results",
                                 style = "min-height: 400px;",
                              
                               uiOutput("results"),
                               br(),
                               tableOutput("resultdata")
                               ),
                               tabPanel(
                                 title = "Confidence Interval",
                                 style = "min-height: 400px;",
                               uiOutput("CInumbers"),
                               br(),
                               plotOutput("ciplot")
                               )
                             ),
                             
                           ) ),
                          fluidRow(
                            column(width=6, 
                                   align = "left",
                                   actionButton("previous3", "Previous Page",
                                                style = "margin-top: 10px; padding: 10px 20px;")
                            ),
                            column(width=6, 
                                   align = "right",
                                   actionButton("next3", "Next Page",
                                                style = "margin-top: 10px; padding: 10px 20px;")
                            )
                          )
                ),
                tabItem(tabName = "Feature2",
                         h2("Reducing Wait Time for Subscribers", align = "center", style = "font-weight: bold"),
                        br(),
                         fluidRow(
                           column(width = 4,
                                  box(width=12,
                                      title = "Information",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature2des1")
                                  ),
                                  box(width = 12,
                                      title = "Expected Rate",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature2des2")
                                  ),
                                  box(width = 12,
                                      title = "Instructions",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature2des3")
                                  )
                                  ),
                           column(width = 8,
                                  box(width=12,
                                      style = "min-height: 405px;",
                               title = "Choices",
                               status = "primary",
                               solidHeader = TRUE,
                               selectInput(
                                 inputId = "sl2",                     
                                 label = "Choose your significance level:",               
                                 choices = list(
                                   "1%" = 0.01,
                                   "5%" = 0.05,
                                   "10%" = 0.10
                                 ),
                                 selected = 0.05                             
                               ),
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
                           ))),
                           
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous4", "Previous Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next4", "Next Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          )
                        )
                         
                         
                         
                         
                ),
                tabItem(tabName = "Feature2results",
                         h2("Reducing Wait Time for Subscribers", align = "center", style = "font-weight: bold"),
                        br(),
                         fluidRow(
                           column(width = 4,
                                  box(width=12,
                               
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
                                          choices = c("Yes" = TRUE, "No" = FALSE), 
                                          selected = character(0)),
                             textInput("surveyquestion2", "question?", 
                                       value = "")
                           )),
                           
                           
                           column(width = 8,
                                  
                                      tabBox(width=12,
                                             
                                             id = "feature2tabs",
                                             tabPanel(
                                               title = "Results",
                                               style = "min-height: 400px;",
                                               
                                               uiOutput("results2"),
                                               br(),
                                               tableOutput("resultdata2")
                                             ),
                                             tabPanel(
                                               title = "Confidence Interval",
                                               style = "min-height: 400px;",
                                               uiOutput("CInumbers2"),
                                               br(),
                                               plotOutput("ciplot2")
                                             )
                                      ),
                               
                             
                           )),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous5", "Previous Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next5", "Next Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          )
                        )
                ),
                tabItem(tabName = "Feature3",
                         h2("Increasing Adverts for Free Tier", align = "center", style = "font-weight: bold"),
                        br(),
                         fluidRow(
                           column(width = 4,
                                  box(width=12,
                                      title = "Information",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature3des1")
                                  ),
                                  box(width = 12,
                                      title = "Expected Rate",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature3des2")
                                  ),
                                  box(width = 12,
                                      title = "Instructions",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature3des3")
                                  )
                                  ),
                           column(width = 8,
                               box(width=12,
                                   style = "min-height: 385px;",
                               title = "Choices",
                               status = "primary",
                               solidHeader = TRUE,
                               selectInput(
                                 inputId = "sl3",                     
                                 label = "Choose your significance level:",               
                                 choices = list(
                                   "1%" = 0.01,
                                   "5%" = 0.05,
                                   "10%" = 0.10
                                 ),
                                 selected = 0.05                             
                               ),
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
                                     textOutput("power3")))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous6", "Previous Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next6", "Next Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          )
                        )
                         ),
                tabItem(tabName = "Feature3Results",
                         h2("Increasing Adverts for Free Tier", align = "center", style = "font-weight: bold"),
                        br(),
                         fluidRow(
                           column(width = 4,
                               box(width=12,
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
                                          choices = c("Yes" = TRUE, "No" = FALSE), 
                                          selected = character(0)),
                             textInput("surveyquestion3", "question?", 
                                       value = "")
                           )),
                           column(width = 8,
                                  tabBox(width=12,
                                         
                                         id = "feature3tabs",
                                         tabPanel(
                                           title = "Results",
                                           style = "min-height: 400px;",
                                           
                                           uiOutput("results3"),
                                           br(),
                                           tableOutput("resultdata3")
                                         ),
                                         tabPanel(
                                           title = "Confidence Interval",
                                           style = "min-height: 400px;",
                                           uiOutput("CInumbers3"),
                                           br(),
                                           plotOutput("ciplot3")
                                         )
                                  ))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous7", "Previous Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next7", "Next Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          )
                        ) ),
                tabItem(tabName = "Feature4",
                         h2("Introducing Streaks for Subscription Users", align = "center", style = "font-weight: bold"),
                        br(),
                         fluidRow(
                           column(width = 4,
                                  box(width=12,
                                      title = "Information",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature4des1")
                                  ),
                                  box(width = 12,
                                      title = "Expected Rate",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature4des2")
                                  ),
                                  box(width = 12,
                                      title = "Instructions",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      textOutput("feature4des3")
                                  )
                           ),
                           column(width = 8,
                               box(width = 12,
                                   style = "min-height: 385px;",
                               title = "Choices",
                               status = "primary",
                               solidHeader = TRUE,
                               selectInput(
                                 inputId = "sl4",                     
                                 label = "Choose your significance level:",               
                                 choices = list(
                                   "1%" = 0.01,
                                   "5%" = 0.05,
                                   "10%" = 0.10
                                 ),
                                 selected = 0.05                             
                               ),
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
                                     textOutput("power4")))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous8", "Previous Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next8", "Next Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          )
                        )
                              ),
                tabItem(tabName = "Feature4results",
                         h2("Introducing Streaks for Subscription Users", align = "center", style = "font-weight: bold"),
                        br(),
                         fluidRow(
                           column(width = 4,
                               box(width=12,
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
                                          choices = c("Yes" = TRUE, "No" = FALSE), 
                                          selected = character(0)),
                             textInput("surveyquestion4", "question?", 
                                       value = "")
                           )),
                           column(width = 8,
                                  tabBox(width=12,
                                         
                                         id = "feature4tabs",
                                         tabPanel(
                                           title = "Results",
                                           style = "min-height: 400px;",
                                           
                                           uiOutput("results4"),
                                           br(),
                                           tableOutput("resultdata4")
                                         ),
                                         tabPanel(
                                           title = "Confidence Interval",
                                           style = "min-height: 400px;",
                                           uiOutput("CInumbers4"),
                                           br(),
                                           plotOutput("ciplot4")
                                         )
                                  ))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous9", "Previous Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next9", "Next Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          )
                        )),
                
                tabItem(tabName = "Feature5",
                        h2("Introducing Subscription Exclusive Levels", align = "center", style = "font-weight: bold"),
                        br(),
                        fluidRow(
                          column(width = 4,
                                 box(width=12,
                                     title = "Information",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("feature5des1")
                                 ),
                                 box(width = 12,
                                     title = "Expected Rate",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("feature5des2")
                                 ),
                                 box(width = 12,
                                     title = "Instructions",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("feature5des3")
                                 )
                                 ),
                          column(width = 8,
                                 box(width = 12,
                                     style = "min-height: 405px;",
                                     title = "Choices",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     selectInput(
                                       inputId = "sl5",                     
                                       label = "Choose your significance level:",               
                                       choices = list(
                                         "1%" = 0.01,
                                         "5%" = 0.05,
                                         "10%" = 0.10
                                       ),
                                       selected = 0.05                             
                                     ),
                                     radioButtons("dayquestion5", "How many days would you like to run the test?",
                                                  choices =
                                                    c(1, 2, 3, 4),
                                                  selected = character(0)),
                                     
                                     sliderInput("samplesize5",
                                                 "Choose your sample size:",
                                                 min = 1000,
                                                 max = 10000,
                                                 value = 3000,
                                                 step = 100),
                                     textOutput("power5")))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous91", "Previous Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next91", "Next Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          )
                        )),
                tabItem(tabName = "Feature5results",
                        h2("Introducing Subscription Exclusive Levels", align = "center", style = "font-weight: bold"),
                        br(),
                        fluidRow(
                          column(width = 4,
                                 box(width=12,
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("press5"),
                                     br(),
                                     actionButton("resultsbutton5", "Press here for results!"),
                                     br(),
                                     br(),
                                     textOutput("CI5"),
                                     br(),
                                     actionButton("CIbutton5", "Confidence Interval"),
                                     br(),
                                     br(),
                                     textOutput("decision5"),
                                     br(),
                                     radioButtons("decision5", "Introduce feature 5?",
                                                  choices = c("Yes" = TRUE, "No" = FALSE), 
                                                  selected = character(0)),
                                     textInput("surveyquestion5", "question?", 
                                               value = "")
                                 )),
                          column(width = 8,
                                 tabBox(width=12,
                                        
                                        id = "feature5tabs",
                                        tabPanel(
                                          title = "Results",
                                          style = "min-height: 400px;",
                                          
                                          uiOutput("results5"),
                                          br(),
                                          tableOutput("resultdata5")
                                        ),
                                        tabPanel(
                                          title = "Confidence Interval",
                                          style = "min-height: 400px;",
                                          uiOutput("CInumbers5"),
                                          br(),
                                          plotOutput("ciplot5")
                                        )
                                 ))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous92", "Previous Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next92", "Next Page",
                                              style = "margin-top: 10px; padding: 10px 20px;")
                          )
                        )),
                
                tabItem(tabName = "Feature6",
                        h2("Offering a Free Trial of the Subscription", align = "center", style = "font-weight: bold"),
                        br(),
                        fluidRow(
                          column(width = 4,
                                 box(width=12,
                                     title = "Information",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("feature6des1")
                                 ),
                                 box(width = 12,
                                     title = "Expected Rate",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("feature6des2")
                                 ),
                                 box(width = 12,
                                     title = "Instructions",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("feature6des3")
                                 )
                                 ),
                          column(width = 8,
                                 box(width = 12,
                                     style = "min-height: 405px;",
                                     title = "Choices",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     selectInput(
                                       inputId = "sl6",                     
                                       label = "Choose your significance level:",               
                                       choices = list(
                                         "1%" = 0.01,
                                         "5%" = 0.05,
                                         "10%" = 0.10
                                       ),
                                       selected = 0.05                             
                                     ),
                                     radioButtons("dayquestion6", "How many days would you like to run the test?",
                                                  choices =
                                                    c(1, 2, 3, 4),
                                                  selected = character(0)),
                                     
                                     sliderInput("samplesize6",
                                                 "Choose your sample size:",
                                                 min = 1000,
                                                 max = 10000,
                                                 value = 3000,
                                                 step = 100),
                                     textOutput("power6")))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous93", "Previous Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next93", "Next Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          )
                        )),
                tabItem(tabName = "Feature6results",
                        h2("Offering a Free Trial of the Subscription", align = "center", style = "font-weight: bold"),
                        br(),
                        fluidRow(
                          column(width = 4,
                                 box(width=12,
                                     status = "primary",
                                     solidHeader = TRUE,
                                     textOutput("press6"),
                                     br(),
                                     actionButton("resultsbutton6", "Press here for results!"),
                                     br(),
                                     br(),
                                     textOutput("CI6"),
                                     br(),
                                     actionButton("CIbutton6", "Confidence Interval"),
                                     br(),
                                     br(),
                                     textOutput("decision6"),
                                     br(),
                                     radioButtons("decision6", "Introduce feature 6?",
                                                  choices = c("Yes" = TRUE, "No" = FALSE), 
                                                  selected = character(0)),
                                     textInput("surveyquestion6", "question?", 
                                               value = "")
                                 )),
                          column(width = 8,
                                 tabBox(width=12,
                                        
                                        id = "feature6tabs",
                                        tabPanel(
                                          title = "Results",
                                          style = "min-height: 400px;",
                                          
                                          uiOutput("results6"),
                                          br(),
                                          tableOutput("resultdata6")
                                        ),
                                        tabPanel(
                                          title = "Confidence Interval",
                                          style = "min-height: 400px;",
                                          uiOutput("CInumbers6"),
                                          br(),
                                          plotOutput("ciplot6")
                                        )
                                 ))),
                                     fluidRow(
                                       column(width=6, 
                                              align = "left",
                                              actionButton("previous94", "Previous Page",
                                                           style = "margin-top: 10px; padding: 10px 20px;")
                                       ),
                                       column(width=6, 
                                              align = "right",
                                              actionButton("next94", "Next Page",
                                                           style = "margin-top: 10px; padding: 10px 20px;")
                                       )
                                     )),
                
                
                tabItem(tabName = "orderfeatures",
                        h2("Order the Chosen Features", align = "center", style = "font-weight: bold"),
                        br(),
                        fluidRow(
                          column(
                            width =6,
                            box(
                            width = 12,
                              title = "Instructions",
                              status = "primary",
                              solidHeader = TRUE,
                              uiOutput("orderinfo"),
                              br(),
                            textInput("surveyquestionfinal", "Why did you choose that order?",
                                      value = ""),
                            actionButton("submitbutton", "Submit all"),
                            textOutput("submitted")
                            )
                              ),
                          column(
                            width = 6,
                            box(
                              width = 12,
                            
                            title = "Order",
                            status = "primary",
                            solidHeader = TRUE,
                            uiOutput("orderedlist")
                              ))),
                        fluidRow(
                          column(width=6, 
                                 align = "left",
                                 actionButton("previous10", "Previous Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          ),
                          column(width=6, 
                                 align = "right",
                                 actionButton("next10", "Next Page",
                                              style = "margin-top: 20px; padding: 10px 20px;")
                          )
                        )
                          ),
                        
                
                 tabItem(tabName = "OneYearLater",
                          h2("Status of MonoBingo One Year Later", align = "center", style = "font-weight: bold"),
                         br(),
                          fluidRow(
                            column(width = 5,
                               box(width = 12,
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
                              uiOutput("order_list")
                            )),
                            
                            column(width = 7,
                                   tabBox(width=12,
                                          
                                          id = "yearlatertabs",
                                          tabPanel(
                                            title = "Results",
                                            style = "min-height: 400px;",
                                            uiOutput("yearresults"),
                                            br(),
                                            tableOutput("yeartable")
                                            ),
                                          tabPanel(
                                            title = "Results Summary",
                                            style = "min-height: 400px;",
                                            fluidRow(
                                              column(6,
                                                     tags$h4("Successful Choices", 
                                                             align = "center", style = "font-weight: bold"),
                                                     uiOutput("goodresult")
                                              ),
                                              column(6,
                                                     tags$h4("Unsuccessful Choices", 
                                                             align = "center", style = "font-weight: bold"),
                                                     uiOutput("badresult")
                                              )
                                            )
                                            
                                            
                                          ),
                                          tabPanel(
                                            title = "Results Plot",
                                            style = "min-height: 400px;",
                                            plotOutput("yearbarchart")
                                          )
                                   )
                               )),
                         fluidRow(
                           column(width=6, 
                                  align = "left",
                                  actionButton("previous11", "Previous Page",
                                               style = "margin-top: 20px; padding: 10px 20px;")
                           ),
                           
                         ) )
                
                
)
),

)



server <- function(input, output, session) {
  
  ##page changing
  
  #forward
  
  observeEvent(input$next0,
               {updateTabItems(session, "menu", "companymetrics")}
               )
  observeEvent(input$next1,
               {updateTabItems(session, "menu", "Feature1")}
  )
  observeEvent(input$next2,
               {updateTabItems(session, "menu", "Feature1results")}
  )
  observeEvent(input$next3,
               {updateTabItems(session, "menu", "Feature2")}
  )
  observeEvent(input$next4,
               {updateTabItems(session, "menu", "Feature2results")}
  )
  observeEvent(input$next5,
               {updateTabItems(session, "menu", "Feature3")}
  )
  observeEvent(input$next6,
               {updateTabItems(session, "menu", "Feature3Results")}
  )
  observeEvent(input$next7,
               {updateTabItems(session, "menu", "Feature4")}
  )
  observeEvent(input$next8,
               {updateTabItems(session, "menu", "Feature4results")}
  )
  observeEvent(input$next9,
               {updateTabItems(session, "menu", "Feature5")}
  )
  observeEvent(input$next91,
               {updateTabItems(session, "menu", "Feature5results")}
  )
  observeEvent(input$next92,
               {updateTabItems(session, "menu", "Feature6")}
  )
  observeEvent(input$next93,
               {updateTabItems(session, "menu", "Feature6results")}
  )
  observeEvent(input$next94,
               {updateTabItems(session, "menu", "orderfeatures")}
  )
  observeEvent(input$next10,
               {updateTabItems(session, "menu", "OneYearLater")}
  )
  
  
  #back
  observeEvent(input$previous1,
               {updateTabItems(session, "menu", "Overview")}
  )
  observeEvent(input$previous2,
               {updateTabItems(session, "menu", "companymetrics")}
  )
  observeEvent(input$previous3,
               {updateTabItems(session, "menu", "Feature1")}
  )
  observeEvent(input$previous4,
               {updateTabItems(session, "menu", "Feature1results")}
  )
  observeEvent(input$previous5,
               {updateTabItems(session, "menu", "Feature2")}
  )
  observeEvent(input$previous6,
               {updateTabItems(session, "menu", "Feature2results")}
  )
  observeEvent(input$previous7,
               {updateTabItems(session, "menu", "Feature3")}
  )
  observeEvent(input$previous8,
               {updateTabItems(session, "menu", "Feature3Results")}
  )
  observeEvent(input$previous9,
               {updateTabItems(session, "menu", "Feature4")}
  )
  observeEvent(input$previous91,
               {updateTabItems(session, "menu", "Feature4results")}
  )
  observeEvent(input$previous92,
               {updateTabItems(session, "menu", "Feature5")}
  )
  observeEvent(input$previous93,
               {updateTabItems(session, "menu", "Feature5results")}
  )
  observeEvent(input$previous94,
               {updateTabItems(session, "menu", "Feature6")}
  )
  observeEvent(input$previous10,
               {updateTabItems(session, "menu", "Feature6results")}
  )
  observeEvent(input$previous11,
               {updateTabItems(session, "menu", "orderfeatures")}
  )
  
  #feature decision throughout
  #creates rate used for a year later
  featuredecisions <- reactiveValues(A = FALSE, B = FALSE, C = FALSE, D = FALSE, E = FALSE, F = FALSE)
  
  observeEvent(input$decision1, { featuredecisions$A <- as.logical(input$decision1) })
  observeEvent(input$decision2, { featuredecisions$B <- as.logical(input$decision2) })
  observeEvent(input$decision3, { featuredecisions$C <- as.logical(input$decision3) })
  observeEvent(input$decision4, { featuredecisions$D <- as.logical(input$decision4) })
  observeEvent(input$decision5, { featuredecisions$E <- as.logical(input$decision5) })
  observeEvent(input$decision6, { featuredecisions$F <- as.logical(input$decision6) })
  
  applyrate <- function(baserate, ratelift) {
    totallift <- prod(1 + ratelift)
    finalrate <- baserate * totallift
    return(finalrate)
  }
  
  calcRate <- reactive({
    selected <- names(which(unlist(reactiveValuesToList(featuredecisions))))
    ratelift <- featurerates[selected]
    
    baserate <- 0.05
    if (length(ratelift) == 0) {
      return(baserate)
    } else {
      return(applyrate(baserate, ratelift))
    }
  })
  
  
  ##screen 1 text overview
  
  output$overview <- renderText({
    "You are a product manager for MonoBingo. Your task is to select, develop and 
    release features which enhance the product. You can conduct A/B tests to help
    you determine whether or not to release a feature."
  })
  output$instructions <- renderUI({
    tags$ul(
      tags$li("Read the company metrics on the next page"),
      tags$li("Work through the features in order."),
      tags$li("Choose your test conditions and then look at the results tab for each."),
      tags$li("On each results tab decide if you want to introduce the feature."),
      tags$li("Answer the written questions as you go along."),
      tags$li("Choose the order you wish to introduce the features."),
      tags$li("Then look at the status of MonoBingo one year later to see the effect
              of your choices.")
    
    )
  })
  output$purpose <- renderUI({
    tagList(
    p("This is a simulation app being used for research of the transfer of learning."),
    p("Your decisions and responses are saved to study how you apply knowledge to an unfamiliar context.")
 ) })
  
  ##company metrics page
 
  
   #pretrial stats
  userlength <- signif(mean(dayta$user_leaves - dayta$user_starts), 3)
  nonsubscriberuserlength <- signif(mean(dayta$user_leaves[is.na(dayta$user_subscribes)] - 
         dayta$user_starts[is.na(dayta$user_subscribes)]),3)
  
  subscriptionrate <- signif((mean(!is.na(dayta$user_subscribes)))*100, 3) 
  
  subscriptiondays <- signif(mean(dayta$user_subscribes - dayta$user_starts, na.rm = TRUE),3)
  
  output$companymetricssummary <- renderText({
    "Here is ainformation about MonoBingo. This includes information about
    their subscribers and users currently with some plots to visualise it. Your task
    is to improve these stats."
  })
  
  #finding churn rate
  
  alldays <- seq(min(dayta$user_starts, na.rm = TRUE), max(dayta$user_leaves, na.rm = TRUE))
  
  dailychurn <- data.frame(
    day = alldays,
    activeusers = NA_integer_,
    usersleft = NA_integer_,
    churnrate = NA_real_
  )

  for (i in seq_along(alldays)) {
    day <- alldays[i]

    activeusers <- dayta$user_starts <= day & dayta$user_leaves > day
    
    leavers <- dayta$user_leaves == day
    
    dailychurn$activeusers[i] <- sum(activeusers)
    dailychurn$usersleft[i]   <- sum(leavers)
    
    if (dailychurn$activeusers[i] > 0) {
      dailychurn$churnrate[i] <- dailychurn$usersleft[i] / dailychurn$activeusers[i]
    } else {
      dailychurn$churnrate[i] <- NA
    }
  }
  
  avgchurn <- signif((mean(dailychurn$churnrate, na.rm = TRUE))*100,3)
  
  #info displayed company metrics
  
  output$cm_users <- renderValueBox({
    valueBox(
      value = paste(userlength, "days"),
      subtitle = "Average User Lifespan",
      icon = icon("users"),
      color = "blue"
    )
  })
  output$cm_subscribers <- renderValueBox({
    valueBox(
      value = paste(subscriptionrate, "%"),
      subtitle = "Average Subscription Rate",
      icon = icon("users"),
      color = "blue"
    )
  })
  output$cm_subscribers2 <- renderValueBox({
    valueBox(
      value = paste(subscriptiondays, "days"),
      subtitle = "Average Time to Subscribe",
      icon = icon("users"),
      color = "blue"
    )
  })
  output$cm_cr <- renderValueBox({
    valueBox(
      value = paste(avgchurn, "%"),
      subtitle = "Average Churn Rate",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  
  
  
  
  ##company metrics plots
 
   #output$cmplot1 <- renderPlot({
     #df_wide %>%
      # pivot_longer(
      #   cols = c(active_users, subscribers),
      #   names_to = "metric",
      #   values_to = "count"
      # ) %>%
      # mutate(count = coalesce(count, 0)) %>%
      # mutate(day = as.Date(Sys.Date() + day)) %>%
      # ggplot(aes(x = day, y = count, col = metric)) + 
     #  geom_line() +
     #  theme_bw() + 
     #  labs(title = "MonoBingo: users and subscribers in last month") +
      # xlab("Date") + 
     #  ylab("Number") +
     #  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
     #  theme_bw()
  # })
   
  # output$cmplot2 <- renderPlot({
   #  df_wide %>%
    #   mutate(conversion = subscribers / active_users) %>%
     #  mutate(day = as.Date(Sys.Date() + day)) %>%
     #  ggplot(aes(x = day, y = conversion)) +
      # geom_line() +
      # theme_bw() + 
      # labs(title = "MonoBingo: Conversion rate") +
     #  xlab("Date") + 
     #  ylab("Number") +
     #  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
     #  theme_bw()
  # })
   
  #churn rate plot
   output$cmplot3 <- renderPlot({
     ggplot(dailychurn, aes(x = day, y = churnrate)) +
       geom_line() +
       labs(title = "Daily Churn Rate Over Time", y = "Churn Rate", x = "Day") +
       theme_bw()
   }
   )

  ##screen 2 text feature 1
  
  output$feature1description <- renderText({ 
    "This feature is reducing the number of hearts per day on the free tier from 5 to 3. 
    Each round on the app uses a heart. When the users runs out of hearts, they can't use the app.
    This should force more users to upgrade."
  })
  
  output$feature1description2 <- renderText({ "It is thought that this feature will increase
  subscription starts by 33%."
  })
  
  output$feature1description3 <- renderText({"You are going to test the effectiveness
     of this feature. To do this, choose your test parameters on the right,  useing the power
     calculator to help you decide. Then go to the next page for results."})
  
  #power result
  #gives power for inputted sample size
  
  output$power <- renderText({
    daynumber <- as.numeric(input$dayquestion)
    sample <- input$samplesize * daynumber
    siglevel <- as.numeric(input$sl1)
    result <- power.prop.test(n = sample, 
                              p1 = 0.05, 
                              p2 = 0.0665, 
                              sig.level = siglevel)
    paste0("The estimated power for your parameters is ",
           round(result$power * 100, 2),"%")
    
  })
  
  ## screen 3 feature 1 results
  #setting up button to reveal results
  
  load <- reactiveVal("before")
  
  
  
  observeEvent(input$resultsbutton, {
    
    
    load("pressed")
    
    
    #5 second delay before revealing
    later(function() {
      
      load("loaded")
    },
    delay = 5)
  })
  
  
  output$press <- renderText ({"Press the button below to reveal the results
    of your test."})
  test1data <- reactive({
    #only runs if question answered
    validate(
      need(input$dayquestion != "", "Please answer the questions on the previous page.")
    )
    
    days <- as.numeric(input$dayquestion)
    samplesize <- as.numeric(input$samplesize)
    usersnumber <- samplesize * days
    users <- rep(usersnumber, 50)
    siglevel <- as.numeric(input$sl1)
    
    #feature rate
    baserate <- 0.05
    lift <- 0.33
    testrate <- baserate * (1 + lift) 
    
    dbExecute(conn, "DELETE FROM sim")
    
    ##simulation
    #test simulation
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "test", create_subscription_decision(testrate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    #control simulation
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "default", create_subscription_decision(baserate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    
    #simulation for active users
    query_days_given_weeks <- function(number_of_weeks) {
      days_in_week <- 7
      (number_of_weeks - 1) * days_in_week 
    }
    
    result <- dbGetQuery(conn, weekly_query, params = list(0, query_days_given_weeks(7)))
    
  })
  
  #creating data from simulation and choices
  output$resultdata <- renderTable({
    validate(
      need(input$dayquestion != "", "Please answer the questions on the previous page.")
    )
    result <- test1data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl1)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    
    rate_test <- round((x[1] / n[1]) * 100, 2)
    rate_control <- round((x[2] / n[2]) * 100, 2)
    rate_diff <- round(rate_test - rate_control, 2)
    sub_diff <- x[1] - x[2]
    p_val <- signif(test_result$p.value, 3)
    
    if (load() == "loaded")
    
      #table with data
    data.frame(
      Test = c("Subscribers", "Users", "Subscription Rate", "p-value"),
      Test_Group = c(x[1], n[1], paste0(rate_test, "%"), "-"),
      Control_Group = c(x[2], n[2], paste0(rate_control, "%"), "-"),
      Difference = c(sub_diff, "-", paste0(rate_diff, "%"), "-"),
      P_Value = c("-", "-", "-", p_val)
    )
  })
  #screen 3 feature 1 results 
  # loading image/text
  
  output$results <- renderUI({
    validate(
      need(input$dayquestion != "", "")
    )
    
    if(load()=="pressed") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (load() == "loaded"){
      #displays choices made
      daynumber <- as.numeric(input$dayquestion)
      sample <- input$samplesize * daynumber
      siglevel <- as.numeric(input$sl1)
      result <- power.prop.test(n = sample, 
                                p1 = 0.05, 
                                p2 = 0.0665, 
                                sig.level = siglevel)
      resultpower <- round(result$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("Your test is at the", siglevel * 100, "% significance level.")),
        p(paste("You chose to run the test for", input$dayquestion, "day(s) and with
                a sample size of", input$samplesize,".")),
        p(paste("The power of your test is",resultpower, "%."))
      )
    }
    else{NULL}
  })
  
  #screen 3  feature 1 results CI
  
  output$CI <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers <- renderUI({
    validate(
      need(input$dayquestion != "", "")
    )
    #data for table
    req(test1data())
    #using earlier data
    result <- test1data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    #test to find confidence intervals
    
    siglevel <- as.numeric(input$sl1)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    
    ci <- signif(test_result$conf.int * 100, 3)
    
    
    ##display confidence interval result
    ci <- signif(test_result$conf.int * 100, 3)
    if(input$CIbutton > 0 && input$resultsbutton > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is",

        "[",ci[1], "%, ", ci[2], "%]." ,
        br()
      )
    }
    else if (input$resultsbutton == 0){
      tagList(
    "Please view results first."
      )
    }
    else {NULL}
    
  })
  #Screen 3  feature 1 CI graph
  
  output$ciplot <- renderPlot({
    validate(
      need(input$dayquestion != "", "")
    )
    if(input$CIbutton > 0 && input$resultsbutton > 0){
      req(test1data())
      
      result <- test1data()
      
      w <- 2
      week_data <- result[result$week_number == w + 52, ]
      
      test_row <- week_data[week_data$grouping == "test", ]
      control_row <- week_data[week_data$grouping == "default", ]
      
      x <- c(test_row$subscribers, control_row$subscribers)
      n <- c(test_row$active_users, control_row$active_users)
      
      siglevel <- as.numeric(input$sl1)
      conflevel <- 1 - siglevel
      test_result <- prop.test(x, n, conf.level = conflevel)
      
      ci <- signif(test_result$conf.int * 100, 3)
      rate_test <- round((x[1] / n[1]) * 100, 2)
      rate_control <- round((x[2] / n[2]) * 100, 2)
      rate_diff <- round(rate_test - rate_control, 2)
      sub_diff <- x[1] - x[2]
      
      ##creating data for plot
      cidata <- data.frame(
        x = "Difference",
        diff = rate_diff,
        lower = ci[1],
        upper = ci[2]
      )
      
      #plotting confidence intervals
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
        theme_bw()
      
    }
    
  })
  #screen 3 decision
  
  output$decision1 <- renderText({
    "Now you've seen the results, you must decide if you want to 
    introduce feature 1:
    reducing the number of hearts on the free tier."
  })

  
  #screen 5 feature 2
  
  output$feature2des1 <- renderText({
    "The second feature is reducing the wait time for
    those who subscribe. Currently, the free tier requires 3 minutes wait between
    each round. This feature would mean those who subscribe only wait 30 seconds."
  })
  output$feature2des2 <- renderText({
    "It is thought this feature will increase subscriptions by 20%."
  })
  output$feature2des3 <- renderText({
    "You are going to test the effectiveness of this feature. Please choose the test 
    parameters, using the power calculator to help. Then, view the results on the next page."
  })
  output$power2 <- renderText({
  daynumber2 <- as.numeric(input$dayquestion2)
  sample2 <- input$samplesize2 * daynumber2
  siglevel2 <- as.numeric(input$sl2)
  result2 <- power.prop.test(n = sample2, 
                            p1 = 0.05, 
                            p2 = 0.06, 
                            sig.level = siglevel2)
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
  
  #data using choices and simulation
  test2data <- reactive({
    validate(
      need(input$dayquestion2 != "", "Please answer the questions on the previous page.")
    )
    
    days <- as.numeric(input$dayquestion2)
    samplesize2 <- as.numeric(input$samplesize2)
    usersnumber <- samplesize2 * days
    users <- rep(usersnumber, 50)
    siglevel <- as.numeric(input$sl2)
    
    baserate <- 0.05
    lift <- 0.2
    testrate <- baserate * (1 + lift) 
    
    dbExecute(conn, "DELETE FROM sim")
    
    ##simulation code
    #test
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "test", create_subscription_decision(testrate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    #control
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "default", create_subscription_decision(baserate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    
 #simulation active users
    query_days_given_weeks <- function(number_of_weeks) {
      days_in_week <- 7
      (number_of_weeks - 1) * days_in_week 
    }
    
    result <- dbGetQuery(conn, weekly_query, params = list(0, query_days_given_weeks(7)))
    
  })
  
  #result table data and table
  output$resultdata2 <- renderTable({
    validate(
      need(input$dayquestion2 != "", "Please answer the questions on the previous page.")
    )
    result <- test2data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl2)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    
    rate_test <- round((x[1] / n[1]) * 100, 2)
    rate_control <- round((x[2] / n[2]) * 100, 2)
    rate_diff <- round(rate_test - rate_control, 2)
    sub_diff <- x[1] - x[2]
    p_val <- signif(test_result$p.value, 3)
    
    if (loadfeature2() == "loadedfeature2")
      
      #result table
      data.frame(
        Test = c("Subscribers", "Users", "Subscription Rate", "p-value"),
        Test_Group = c(x[1], n[1], paste0(rate_test, "%"), "-"),
        Control_Group = c(x[2], n[2], paste0(rate_control, "%"), "-"),
        Difference = c(sub_diff, "-", paste0(rate_diff, "%"), "-"),
        P_Value = c("-", "-", "-", p_val)
      )
  })
  #screen 5 feature 2 results 
  # loading image/text
  
  output$results2 <- renderUI({
    validate(
      need(input$dayquestion2 != "", "")
    )
    
    if(loadfeature2()=="pressedfeature2") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature2() == "loadedfeature2"){
      daynumber <- as.numeric(input$dayquestion2)
      sample <- input$samplesize2 * daynumber
      siglevel2 <- as.numeric(input$sl2)
      result <- power.prop.test(n = sample, 
                                p1 = 0.05, 
                                p2 = 0.06, 
                                sig.level = siglevel2)
      resultpower <- round(result$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("Your test is at the", siglevel2 * 100, "% significance level.")),
        p(paste("You chose to run the test for", input$dayquestion2, "day(s) and with
                a sample size of", input$samplesize2,".")),
        p(paste("The power of your test is",
                resultpower, "%.")))
        
      
    }
    else{
      NULL
    }
  })
  
  #screen 5  feature 2 results CI
  
  output$CI2 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers2 <- renderUI({
    validate(
      need(input$dayquestion2 != "", "")
    )
    #data for table
    req(test2data())
    
    result <- test2data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl2)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    ci <- signif(test_result$conf.int * 100, 3)
    
    #displaying the confidence interval
    
    ci <- signif(test_result$conf.int * 100, 3)
    if(input$CIbutton2 > 0 && input$resultsbutton2 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is",
        
        "[",ci[1], "%, ", ci[2], "%]." ,
        br()
      )
    }
    else if (input$resultsbutton2 == 0){
      tagList(
        "Please view results first."
      )
    }
    else {NULL}
    
  })
  #Screen 5  feature 2 CI graph
  
  output$ciplot2 <- renderPlot({
    validate(
      need(input$dayquestion2 != "", "")
    )
    if(input$CIbutton2 > 0 && input$resultsbutton2 > 0){
      req(test2data())
      
      result <- test2data()
      
      w <- 2
      week_data <- result[result$week_number == w + 52, ]
      
      test_row <- week_data[week_data$grouping == "test", ]
      control_row <- week_data[week_data$grouping == "default", ]
      
      x <- c(test_row$subscribers, control_row$subscribers)
      n <- c(test_row$active_users, control_row$active_users)
      
      siglevel <- as.numeric(input$sl2)
      conflevel <- 1 - siglevel
      test_result <- prop.test(x, n, conf.level = conflevel)
      ci <- signif(test_result$conf.int * 100, 3)
      rate_test <- round((x[1] / n[1]) * 100, 2)
      rate_control <- round((x[2] / n[2]) * 100, 2)
      rate_diff <- round(rate_test - rate_control, 2)
      sub_diff <- x[1] - x[2]
      
      #data frame for plot
      cidata <- data.frame(
        x = "Difference",
        diff = rate_diff,
        lower = ci[1],
        upper = ci[2]
      )
      
      #plot
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
        theme_bw()
      
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
    "It is thought that increasing adverts for the free tier will cause 40% more subscribers."
  })
  output$feature3des3 <- renderText({
    "Test the effectiveness of this feature before you decide whether to introduce it. Choose your
    test parameters using the power calculator. Then view results on the next page."
  })
  output$power3 <- renderText({
    daynumber3 <- as.numeric(input$dayquestion3)
    sample3 <- input$samplesize3 * daynumber3
    siglevel3 <- as.numeric(input$sl3)
    result3 <- power.prop.test(n = sample3, 
                               p1 = 0.05, 
                               p2 = 0.07, 
                               sig.level = siglevel3)
    paste0("The estimated power for your sample size is ",
           round(result3$power * 100, 2), "%")
  })
  #screen 7 feature 3 results
  
  
  loadfeature3 <- reactiveVal("beforefeature3")
  
  observeEvent(input$resultsbutton3, {
    
    loadfeature3("pressedfeature3")
    
    # 5 second delay to reveal results
    later(function() {
      
      loadfeature3("loadedfeature3")
    },
    delay = 5)
  })
  
  
  output$press3 <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  test3data <- reactive({
    validate(
      need(input$dayquestion3 != "", "Please answer the questions on the previous page.")
    )
    
    days <- as.numeric(input$dayquestion3)
    samplesize3 <- as.numeric(input$samplesize3)
    usersnumber <- samplesize3 * days
    users <- rep(usersnumber, 50)
    siglevel <- as.numeric(input$sl13)
    
    baserate <- 0.05
    lift <- 0.4
    testrate <- baserate * (1 + lift) 
    
    dbExecute(conn, "DELETE FROM sim")
    
    ##simualation
    #test
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "test", create_subscription_decision(testrate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    #control
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "default", create_subscription_decision(baserate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    #simulation active users
    query_days_given_weeks <- function(number_of_weeks) {
      days_in_week <- 7
      (number_of_weeks - 1) * days_in_week 
    }
    
    result <- dbGetQuery(conn, weekly_query, params = list(0, query_days_given_weeks(7)))
    
  })
  output$resultdata3 <- renderTable({
    validate(
      need(input$dayquestion3 != "", "Please answer the questions on the previous page.")
    )
    result <- test3data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl3)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    
    rate_test <- round((x[1] / n[1]) * 100, 2)
    rate_control <- round((x[2] / n[2]) * 100, 2)
    rate_diff <- round(rate_test - rate_control, 2)
    sub_diff <- x[1] - x[2]
    p_val <- signif(test_result$p.value, 3)
    
    if (loadfeature3() == "loadedfeature3")
      #result table
      data.frame(
        Test = c("Subscribers", "Users", "Subscription Rate", "p-value"),
        Test_Group = c(x[1], n[1], paste0(rate_test, "%"), "-"),
        Control_Group = c(x[2], n[2], paste0(rate_control, "%"), "-"),
        Difference = c(sub_diff, "-", paste0(rate_diff, "%"), "-"),
        P_Value = c("-", "-", "-", p_val)
      )
  })
  #screen 7 feature 3 results 
  # loading image/text
  
  output$results3 <- renderUI({
    validate(
      need(input$dayquestion3 != "", "")
    )
    
    if(loadfeature3()=="pressedfeature3") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature3() == "loadedfeature3"){
      daynumber <- as.numeric(input$dayquestion3)
      sample <- input$samplesize3 * daynumber
      siglevel3 <- as.numeric(input$sl3)
      result <- power.prop.test(n = sample, 
                                p1 = 0.05, 
                                p2 = 0.07, 
                                sig.level = siglevel3)
      resultpower <- round(result$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("Your test is at the", siglevel3 * 100, "% significance level.")),
        p(paste("You chose to run the test for", input$dayquestion3, "day(s) and with
                a sample size of", input$samplesize3,".")),
        p(paste("The power of your test is",
                resultpower, "%."))
        
      )
    }
    else{
      NULL
    }
  })
  
  #screen 7  feature 3 results CI
  
  output$CI3 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers3 <- renderUI({
    validate(
      need(input$dayquestion3 != "", "")
    )
    #data for table
    req(test3data())
    
    result <- test3data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl3)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    ci <- signif(test_result$conf.int * 100, 3)
    
    #display confidence interval
    
    ci <- signif(test_result$conf.int * 100, 3)
    if(input$CIbutton3 > 0 && input$resultsbutton3 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is",
        
        "[",ci[1], "%, ", ci[2], "%]." ,
        br()
      )
    }
    else if (input$resultsbutton3 == 0){
      tagList(
        "Please view results first."
      )
    }
    else {NULL}
    
  })
  #Screen 7  feature 3 CI graph
  
  output$ciplot3 <- renderPlot({
    validate(
      need(input$dayquestion3 != "", "")
    )
    if(input$CIbutton3 > 0 && input$resultsbutton3 > 0){
      req(test3data())
      
      result <- test3data()
      
      w <- 2
      week_data <- result[result$week_number == w + 52, ]
      
      test_row <- week_data[week_data$grouping == "test", ]
      control_row <- week_data[week_data$grouping == "default", ]
      
      x <- c(test_row$subscribers, control_row$subscribers)
      n <- c(test_row$active_users, control_row$active_users)
      
      siglevel <- as.numeric(input$sl3)
      conflevel <- 1 - siglevel
      test_result <- prop.test(x, n, conf.level = conflevel)
      ci <- signif(test_result$conf.int * 100, 3)
      rate_test <- round((x[1] / n[1]) * 100, 2)
      rate_control <- round((x[2] / n[2]) * 100, 2)
      rate_diff <- round(rate_test - rate_control, 2)
      sub_diff <- x[1] - x[2]
      
      #data for plot
      cidata <- data.frame(
        x = "Difference",
        diff = rate_diff,
        lower = ci[1],
        upper = ci[2]
      )
      #plot
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
        theme_bw()
      
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
    feature. This will give subscribers a way to track their progress."
  })
  output$feature4des2 <- renderText({
    "It is thought that introducing this will cause 25% more subscribers."
  })
  output$feature4des3 <- renderText({
    "Test the effectiveness of this feature before you decide whether to introduce it. Choose your
    test parameters using the power calculator. Then, view results on the next page."
  })
  output$power4 <- renderText({
    daynumber4 <- as.numeric(input$dayquestion4)
    sample4 <- input$samplesize4 * daynumber4
    siglevel4 <- as.numeric(input$sl4)
    result4 <- power.prop.test(n = sample4, 
                               p1 = 0.05, 
                               p2 = 0.0625, 
                               sig.level = siglevel4)
    paste0("The estimated power for your sample size is ",
           round(result4$power * 100, 2), "%")
  })
  #screen 9 feature 4 results
  
  
  loadfeature4 <- reactiveVal("beforefeature4")
  
  
  observeEvent(input$resultsbutton4, {
    
    
    loadfeature4("pressedfeature4")
    
    #5 second delay before revealing results
    
    later(function() {
      
      loadfeature4("loadedfeature4")
    },
    delay = 5)
  })
  
  
  output$press4 <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  test4data <- reactive({
    validate(
      need(input$dayquestion4 != "", "Please answer the questions on the previous page.")
    )
    
    days <- as.numeric(input$dayquestion4)
    samplesize4 <- as.numeric(input$samplesize4)
    usersnumber <- samplesize4 * days
    users <- rep(usersnumber, 50)
    siglevel <- as.numeric(input$sl4)
    
    baserate <- 0.05
    lift <- 0.25
    testrate <- baserate * (1 + lift) 
    
    dbExecute(conn, "DELETE FROM sim")
    
    ##simulation
    #test
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "test", create_subscription_decision(testrate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    #control
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "default", create_subscription_decision(baserate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    
    # simulation active users
    query_days_given_weeks <- function(number_of_weeks) {
      days_in_week <- 7
      (number_of_weeks - 1) * days_in_week 
    }
    
    result <- dbGetQuery(conn, weekly_query, params = list(0, query_days_given_weeks(7)))
    
  })
  output$resultdata4 <- renderTable({
    validate(
      need(input$dayquestion4 != "", "Please answer the questions on the previous page.")
    )
    result <- test4data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl4)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    
    rate_test <- round((x[1] / n[1]) * 100, 2)
    rate_control <- round((x[2] / n[2]) * 100, 2)
    rate_diff <- round(rate_test - rate_control, 2)
    sub_diff <- x[1] - x[2]
    p_val <- signif(test_result$p.value, 3)
    
    if (loadfeature4() == "loadedfeature4")
      #results table
      data.frame(
        Test = c("Subscribers", "Users", "Subscription Rate", "p-value"),
        Test_Group = c(x[1], n[1], paste0(rate_test, "%"), "-"),
        Control_Group = c(x[2], n[2], paste0(rate_control, "%"), "-"),
        Difference = c(sub_diff, "-", paste0(rate_diff, "%"), "-"),
        P_Value = c("-", "-", "-", p_val)
      )
  })
  #screen 9 feature 4 results 
  # loading image/text
  
  output$results4 <- renderUI({
    validate(
      need(input$dayquestion4 != "", "")
    )
    
    if(loadfeature4()=="pressedfeature4") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature4() == "loadedfeature4"){
      daynumber <- as.numeric(input$dayquestion4)
      sample <- input$samplesize4 * daynumber
      siglevel4 <- as.numeric(input$sl4)
      result <- power.prop.test(n = sample, 
                                p1 = 0.05, 
                                p2 = 0.0625, 
                                sig.level = siglevel4)
      resultpower <- round(result$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("Your test is at the", siglevel4 * 100, "% significance level.")),
        p(paste("You chose to run the test for", input$dayquestion4, "day(s) and with
                a sample size of", input$samplesize4,".")),
                p(paste("The power of your test is",
                resultpower, "%."))
        
      )
    }
    else{
      NULL
    }
  })
  
  #screen 9  feature 4 results CI
  
  output$CI4 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers4 <- renderUI({
    validate(
      need(input$dayquestion4 != "", "")
    )
    #data for table
    req(test4data())
    
    result <- test4data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl4)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    ci <- signif(test_result$conf.int * 100, 3)
    
    #display confidence interval
    
    ci <- signif(test_result$conf.int * 100, 3)
    if(input$CIbutton4 > 0 && input$resultsbutton4 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is",
        
        "[",ci[1], "%, ", ci[2], "%]." ,
        br()
      )
    }
    else if (input$resultsbutton4 == 0){
      tagList(
        "Please view results first."
      )
    }
    else {NULL}
    
  })
  #Screen 9  feature 4 CI graph
  
  output$ciplot4 <- renderPlot({
    validate(
      need(input$dayquestion4 != "", "")
    )
    if(input$CIbutton4 > 0 && input$resultsbutton4 > 0){
      req(test4data())
      
      result <- test4data()
      
      w <- 2
      week_data <- result[result$week_number == w + 52, ]
      
      test_row <- week_data[week_data$grouping == "test", ]
      control_row <- week_data[week_data$grouping == "default", ]
      
      x <- c(test_row$subscribers, control_row$subscribers)
      n <- c(test_row$active_users, control_row$active_users)
      
      siglevel <- as.numeric(input$sl4)
      conflevel <- 1 - siglevel
      test_result <- prop.test(x, n, conf.level = conflevel)
      ci <- signif(test_result$conf.int * 100, 3)
      rate_test <- round((x[1] / n[1]) * 100, 2)
      rate_control <- round((x[2] / n[2]) * 100, 2)
      rate_diff <- round(rate_test - rate_control, 2)
      sub_diff <- x[1] - x[2]
      
      #data frame for plot
      cidata <- data.frame(
        x = "Difference",
        diff = rate_diff,
        lower = ci[1],
        upper = ci[2]
      )
      #plot
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
        theme_bw()
      
    }
    
  })
  
  #screen 9 feature 4 decision
  
  output$decision4 <- renderText({
    "Now you've seen the results for this test you must decide if you want to introduce feature 4:
    streaks for subscribers."
  })
  
  #screen 10 feature 5
  
  output$feature5des1 <- renderText({
    "This feature is introducing some levels exclusively for subscribers. 
    Currently, free tier and subscribers can access the same levels.
    This will give more opportunities to subscribers "
  })
  output$feature5des2 <- renderText({
    "It is thought that introducing this feature will cause 18% more subscribers."
  })
  output$feature5des3 <- renderText({
    "Test the effectiveness of this feature before you decide whether to introduce it. Choose 
    your test parameters using the power calculator. Then, view results on the next page."
  })
  output$power5 <- renderText({
    daynumber5 <- as.numeric(input$dayquestion5)
    sample5 <- input$samplesize5 * daynumber5
    siglevel5 <- as.numeric(input$sl5)
    result5 <- power.prop.test(n = sample5, 
                               p1 = 0.05, 
                               p2 = 0.059, 
                               sig.level = siglevel5)
    paste0("The estimated power for your sample size is ",
           round(result5$power * 100, 2), "%")
  })
  #screen 11 feature 5 results
  
  loadfeature5 <- reactiveVal("beforefeature5")
  
  observeEvent(input$resultsbutton5, {
    
    
    loadfeature5("pressedfeature5")
    
    #5 second delay before revealing results
    
    later(function() {
      
      loadfeature5("loadedfeature5")
    },
    delay = 5)
  })
  
  
  output$press5 <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  test5data <- reactive({
    validate(
      need(input$dayquestion5 != "", "Please answer the questions on the previous page.")
    )
    
    days <- as.numeric(input$dayquestion5)
    samplesize5 <- as.numeric(input$samplesize5)
    usersnumber <- samplesize5 * days
    users <- rep(usersnumber, 50)
    siglevel <- as.numeric(input$sl5)
    
    baserate <- 0.05
    lift <- 0.18
    testrate <- baserate * (1 + lift) 
    
    dbExecute(conn, "DELETE FROM sim")
    
    #simulation
    #test
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "test", create_subscription_decision(testrate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    #control
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "default", create_subscription_decision(baserate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    
    # simulation active users
    query_days_given_weeks <- function(number_of_weeks) {
      days_in_week <- 7
      (number_of_weeks - 1) * days_in_week 
    }
    
    result <- dbGetQuery(conn, weekly_query, params = list(0, query_days_given_weeks(7)))
    
  })
  output$resultdata5 <- renderTable({
    validate(
      need(input$dayquestion5 != "", "Please answer the questions on the previous page.")
    )
    result <- test5data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl5)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    
    rate_test <- round((x[1] / n[1]) * 100, 2)
    rate_control <- round((x[2] / n[2]) * 100, 2)
    rate_diff <- round(rate_test - rate_control, 2)
    sub_diff <- x[1] - x[2]
    p_val <- signif(test_result$p.value, 3)
    
    if (loadfeature5() == "loadedfeature5")
      #results table
      data.frame(
        Test = c("Subscribers", "Users", "Subscription Rate", "p-value"),
        Test_Group = c(x[1], n[1], paste0(rate_test, "%"), "-"),
        Control_Group = c(x[2], n[2], paste0(rate_control, "%"), "-"),
        Difference = c(sub_diff, "-", paste0(rate_diff, "%"), "-"),
        P_Value = c("-", "-", "-", p_val)
      )
  })
  #screen 11 feature 5 results 
  # loading image/text
  
  output$results5 <- renderUI({
    validate(
      need(input$dayquestion5 != "", "")
    )
    
    if(loadfeature5()=="pressedfeature5") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature5() == "loadedfeature5"){
      daynumber <- as.numeric(input$dayquestion5)
      sample <- input$samplesize5 * daynumber
      siglevel5 <- as.numeric(input$sl5)
      result <- power.prop.test(n = sample, 
                                p1 = 0.05, 
                                p2 = 0.059, 
                                sig.level = siglevel5)
      resultpower <- round(result$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("Your test is at the", siglevel5 * 100, "% significance level.")),
        p(paste("You chose to run the test for", input$dayquestion5, "day(s) and with
                a sample size of", input$samplesize5,".")),
                p(paste("The power of your test is",
                resultpower, "%."))
      )
    }
    else{
      NULL
    }
  })
  
  #screen 9  feature 4 results CI
  
  output$CI5 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers5 <- renderUI({
    validate(
      need(input$dayquestion5 != "", "")
    )
    #data for table
    req(test5data())
    
    result <- test5data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl5)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    ci <- signif(test_result$conf.int * 100, 3)
    
    #displaying confidence interval result
    
    ci <- signif(test_result$conf.int * 100, 3)
    if(input$CIbutton5 > 0 && input$resultsbutton5 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is",
        
        "[",ci[1], "%, ", ci[2], "%]." ,
        br()
      )
    }
    else if (input$resultsbutton5 == 0){
      tagList(
        "Please view results first."
      )
    }
    else {NULL}
    
  })
  #Screen 11  feature 5 CI graph
  
  output$ciplot5 <- renderPlot({
    validate(
      need(input$dayquestion5 != "", "")
    )
    if(input$CIbutton5 > 0 && input$resultsbutton5 > 0){
      req(test5data())
      
      result <- test5data()
      
      w <- 2
      week_data <- result[result$week_number == w + 52, ]
      
      test_row <- week_data[week_data$grouping == "test", ]
      control_row <- week_data[week_data$grouping == "default", ]
      
      x <- c(test_row$subscribers, control_row$subscribers)
      n <- c(test_row$active_users, control_row$active_users)
      
      siglevel <- as.numeric(input$sl5)
      conflevel <- 1 - siglevel
      test_result <- prop.test(x, n, conf.level = conflevel)
      ci <- signif(test_result$conf.int * 100, 3)
      rate_test <- round((x[1] / n[1]) * 100, 2)
      rate_control <- round((x[2] / n[2]) * 100, 2)
      rate_diff <- round(rate_test - rate_control, 2)
      sub_diff <- x[1] - x[2]
      
      #dataframe for plot
      cidata <- data.frame(
        x = "Difference",
        diff = rate_diff,
        lower = ci[1],
        upper = ci[2]
      )
      #plot
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
        theme_bw()
      
    }
    
  })
  
  #screen 11 feature 5 decision
  
  output$decision5 <- renderText({
    "Now you've seen the results for this test you must decide if you want to 
    introduce feature 5:
    subscriber only levels."
  })
  
  #screen 12 feature 6
  
  output$feature6des1 <- renderText({
    "This feature is offering a free trial for the subscription. 
    This will give free tier users an opportunity to see the benefits
    of the subscription and convince them to subscibe afterwards."
  })
  output$feature6des2 <- renderText({
    "It is thought that offering this will cause 29% more subscribers."
  })
  output$feature6des3 <- renderText({
    "Test the effectiveness of this feature before you decide whether to introduce it. Choose 
    your test parameters using the power calculator. Then, view results on the next page."
  })
  output$power6 <- renderText({
    daynumber6 <- as.numeric(input$dayquestion6)
    sample6 <- input$samplesize6 * daynumber6
    siglevel6 <- as.numeric(input$sl6)
    result6 <- power.prop.test(n = sample6, 
                               p1 = 0.05, 
                               p2 = 0.0645, 
                               sig.level = siglevel6)
    paste0("The estimated power for your sample size is ",
           round(result6$power * 100, 2), "%")
  })
  #screen 13 feature 6 results
  
  loadfeature6 <- reactiveVal("beforefeature6")
  
  observeEvent(input$resultsbutton6, {
    
    
    loadfeature6("pressedfeature6")
    
    #5 second delay before results
    
    later(function() {
      
      loadfeature6("loadedfeature6")
    },
    delay = 5)
  })
  
  
  output$press6 <- renderText ({"Press the button below to reveal the results
    of your test."})
  
  test6data <- reactive({
    validate(
      need(input$dayquestion6 != "", "Please answer the questions on the previous page.")
    )
    
    days <- as.numeric(input$dayquestion6)
    samplesize6 <- as.numeric(input$samplesize6)
    usersnumber <- samplesize6 * days
    users <- rep(usersnumber, 50)
    siglevel <- as.numeric(input$sl6)
    
    baserate <- 0.05
    lift <- 0.29
    testrate <- baserate * (1 + lift) 
    
    dbExecute(conn, "DELETE FROM sim")
    
    #simulation
    #test
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "test", create_subscription_decision(testrate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    #control
    for (i in c(1:7 * 6)) {
      dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "default", create_subscription_decision(baserate))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    
    # simulation active users
    query_days_given_weeks <- function(number_of_weeks) {
      days_in_week <- 7
      (number_of_weeks - 1) * days_in_week 
    }
    
    result <- dbGetQuery(conn, weekly_query, params = list(0, query_days_given_weeks(7)))
    
  })
  output$resultdata6 <- renderTable({
    validate(
      need(input$dayquestion6 != "", "Please answer the questions on the previous page.")
    )
    result <- test6data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl6)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    
    rate_test <- round((x[1] / n[1]) * 100, 2)
    rate_control <- round((x[2] / n[2]) * 100, 2)
    rate_diff <- round(rate_test - rate_control, 2)
    sub_diff <- x[1] - x[2]
    p_val <- signif(test_result$p.value, 3)
    
    if (loadfeature6() == "loadedfeature6")
      #results table
      data.frame(
        Test = c("Subscribers", "Users", "Subscription Rate", "p-value"),
        Test_Group = c(x[1], n[1], paste0(rate_test, "%"), "-"),
        Control_Group = c(x[2], n[2], paste0(rate_control, "%"), "-"),
        Difference = c(sub_diff, "-", paste0(rate_diff, "%"), "-"),
        P_Value = c("-", "-", "-", p_val)
      )
  })
  #screen 13 feature 6 results 
  # loading image/text
  
  output$results6 <- renderUI({
    validate(
      need(input$dayquestion6 != "", "")
    )
    
    if(loadfeature6()=="pressedfeature6") {
      tagList(
        h3("Loading results"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (loadfeature6() == "loadedfeature6"){
      daynumber <- as.numeric(input$dayquestion6)
      sample <- input$samplesize6 * daynumber
      siglevel6 <- as.numeric(input$sl6)
      result <- power.prop.test(n = sample, 
                                p1 = 0.05, 
                                p2 = 0.0645, 
                                sig.level = siglevel6)
      resultpower <- round(result$power * 100, 2)
      tagList(
        h3("These are the results of your test:"),
        p(paste("Your test is at the", siglevel6 * 100, "% significance level.")),
        p(paste("You chose to run the test for", input$dayquestion6, "day(s) and with
                a sample size of", input$samplesize6,".")),
                p(paste("The power of your test is",
                resultpower, "%."))
      )
    }
    else{
      NULL
    }
  })
  
  #screen 13  feature 6 results CI
  
  output$CI6 <- renderText({"If you would like to see the confidence
    intervals, press the button below."
  })
  output$CInumbers6 <- renderUI({
    validate(
      need(input$dayquestion6 != "", "")
    )
    #data for table
    req(test6data())
    
    result <- test6data()
    
    w <- 2
    week_data <- result[result$week_number == w + 52, ]
    
    test_row <- week_data[week_data$grouping == "test", ]
    control_row <- week_data[week_data$grouping == "default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    siglevel <- as.numeric(input$sl6)
    conflevel <- 1 - siglevel
    test_result <- prop.test(x, n, conf.level = conflevel)
    ci <- signif(test_result$conf.int * 100, 3)
    
    #displaying confidence interval
    
    ci <- signif(test_result$conf.int * 100, 3)
    if(input$CIbutton6 > 0 && input$resultsbutton6 > 0){
      tagList(
        "The 95% confidence interval for the percentage difference of rate in your test is",
        
        "[",ci[1], "%, ", ci[2], "%]." ,
        br()
      )
    }
    else if (input$resultsbutton6 == 0){
      tagList(
        "Please view results first."
      )
    }
    else {NULL}
    
  })
  #Screen 13  feature 6 CI graph
  
  output$ciplot6 <- renderPlot({
    validate(
      need(input$dayquestion6 != "", "")
    )
    if(input$CIbutton6 > 0 && input$resultsbutton6 > 0){
      req(test6data())
      
      result <- test6data()
      
      w <- 2
      week_data <- result[result$week_number == w + 52, ]
      
      test_row <- week_data[week_data$grouping == "test", ]
      control_row <- week_data[week_data$grouping == "default", ]
      
      x <- c(test_row$subscribers, control_row$subscribers)
      n <- c(test_row$active_users, control_row$active_users)
      
      siglevel <- as.numeric(input$sl6)
      conflevel <- 1 - siglevel
      test_result <- prop.test(x, n, conf.level = conflevel)
      ci <- signif(test_result$conf.int * 100, 3)
      rate_test <- round((x[1] / n[1]) * 100, 2)
      rate_control <- round((x[2] / n[2]) * 100, 2)
      rate_diff <- round(rate_test - rate_control, 2)
      sub_diff <- x[1] - x[2]
      #data frame for plot
      cidata <- data.frame(
        x = "Difference",
        diff = rate_diff,
        lower = ci[1],
        upper = ci[2]
      )
      #plot
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
        theme_bw()
      
    }
    
  })
  
  #screen 13 feature 6 decision
  
  output$decision6 <- renderText({
    "Now you've seen the results for this test you must decide if you want to 
    introduce feature 6:
    offering a free trial."
  })
  
  
  
  ##penultimate screen, order choices
  
  output$orderinfo <- renderUI({
    tagList(
    p("On the right is the list of all the features you chose to introduce."),
    p("Please drag to reorder them
    to the order you wish them to be introduced to MonoBingo."),
    p("You are aiming for the most effective order to gain subscribers.")
    )
  })
  ##ordering features
  output$orderedlist <- renderUI({
    
    validate( #checking all questions are answered
      need(input$decision1, "Please decide whether to add each feature 1"),
      need(input$decision2, "Please decide whether to add each feature 2"),
      need(input$decision3, "Please decide whether to add each feature 3"),
      need(input$decision4, "Please decide whether to add each feature 4"),
      need(input$decision5, "Please decide whether to add each feature 5"),
      need(input$decision6, "Please decide whether to add each feature 6")
    )
    
    selected <- labelsrank[unlist(lapply(inputrank, function(id) {
      val <- input[[id]]
      !is.null(val) && val == TRUE
    }))]
    
    bucket_list(
      header = "Drag to reorder",
      group_name = "ordering",
      orientation = "vertical",
      add_rank_list(
        text = "Features adding to MonoBingo",
        labels = selected,
        input_id = "ordered_items"
      )
    )
  })
  ##saving survey responses after pressing submit
  observeEvent(input$submitbutton, {
    
    #responses in saveable format
    responses <- list(
      Q1 = input$surveyquestion1,
      Q2 = input$surveyquestion2,
      Q3 = input$surveyquestion3,
      Q4 = input$surveyquestion4,
      Q5 = input$surveyquestion5,
      Q6 = input$surveyquestion6,
      Qfinal = input$surveyquestionfinal,
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    surveyresponses <- data.frame(
      Q1 = input$surveyquestion1,
      Q2 = input$surveyquestion2,
      Q3 = input$surveyquestion3,
      Q4 = input$surveyquestion4,
      Q5 = input$surveyquestion5,
      Q6 = input$surveyquestion6,
      Qfinal = input$surveyquestionfinal,
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    response_df <- as.data.frame(responses, stringsAsFactors = FALSE)
    
    #save as csv
    file <- "responses.csv"
    
    if (!file.exists(file)) {
      
      write.csv(response_df, file, row.names = FALSE)
    } else {
      
      write.table(response_df, file, sep = ",", row.names = FALSE,
                  col.names = FALSE, append = TRUE)
    }
    
    #save as google sheet
    sheet_append(ss = sheet_id, data = surveyresponses)
    output$submitted <- renderText("Your responses have been saved. Thank you!")
  })
  #final screen , a year later
  
  output$yearlater <- renderText({
    "Now we have the status of MonoBingo a year after introducing the features
    you chose.
    To see the number of users and subscribers now, a year later, click the button below."
  })  
  
  output$featureschosen <- renderUI({
    tagList(
    h4("Below is the list of features you chose to introduce to MonoBingo in the order
    you chose to introduce them:"
    )
    )
  })

    
    output$order_list <- renderUI({
      req(input$ordered_items)
      
      
      #displays chosen features in order
      tags$ul(
        lapply(input$ordered_items, function(item) {
          tags$li(item)
        })
      )
    })

    
    #setting up loading button
  load2 <- reactiveVal("before2")
  
  observeEvent(input$yearbutton, {
    load2("pressed2")
    later(function(){
      load2("loaded2")
    },
    delay=5)
  })
  
  yearlaterdata <- reactive({
    basep <- calcRate() 
    
    users <- rep(100000, 50)
    
    
    ##order affects basep
    label_to_id <- setNames(inputrank, labelsrank)
    ordered_labels <- input$ordered_items 
    ordered_ids <- label_to_id[ordered_labels]
    
    #effects from different orders
    
    if ("decision1" %in% ordered_ids && "decision2" %in% ordered_ids) {
      if (match("decision2", ordered_ids) < match("decision1", ordered_ids)) {
        basep <- basep*0.95
      }
    }
    
    if ("decision3" %in% ordered_ids && match("decision3", ordered_ids) == 1) {
      basep <- basep + 0.03
    }
    
    if ("decision4" %in% ordered_ids && "decision5" %in% ordered_ids) {
      if (match("decision5", ordered_ids) < match("decision4", ordered_ids)) {
        basep <- basep*0.97
      }
    }
    
    if ("decision6" %in% ordered_ids && match("decision6", ordered_ids) <= 4) {
      basep <- basep * 0.92
    }
    
    if ("decision6" %in% ordered_ids && match("decision6", ordered_ids) == 6) {
      basep <- basep + 0.02
    }
    
    print(basep)
    
    dbExecute(conn, "DELETE FROM sim")
    
    #simulation
    #test
    for (i in c(1:7 * 6)){
      dayta <- day_sim(users[i], 60, 180, i, "implemented_test", 
                       create_subscription_decision(basep))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    
    #control
    for (i in c(1:7 * 6)){
      dayta <- day_sim(users[i], 60, 180, i, "implemented_default", 
                       create_subscription_decision(0.05))
      dbWriteTable(conn, "sim", dayta, append = TRUE)
    }
    
    # active user simualtion
    query_days_given_weeks <- function(number_of_weeks) {
      days_in_week <- 7
      (number_of_weeks - 1) * days_in_week 
    }
    
    #to get week number
    result <- dbGetQuery(conn, "
  SELECT 
    FLOOR(user_starts / 7) + 1 AS week_number,
    grouping,
    COUNT(*) AS active_users,
    SUM(CASE WHEN user_subscribes IS NOT NULL THEN 1 ELSE 0 END) AS subscribers
  FROM sim
  GROUP BY week_number, grouping
")

  })
  output$yeartable <-renderTable({ 
    validate( #checking all questions are answered
      need(input$decision1, "Please decide whether to add each feature 1"),
      need(input$decision2, "Please decide whether to add each feature 2"),
      need(input$decision3, "Please decide whether to add each feature 3"),
      need(input$decision4, "Please decide whether to add each feature 4"),
      need(input$decision5, "Please decide whether to add each feature 5"),
      need(input$decision6, "Please decide whether to add each feature 6")
    )
    result <- yearlaterdata()
    
    w <- 2
    week_data <- result[result$week_number == w, ]
    
    test_row <- week_data[week_data$grouping == "implemented_test", ]
    control_row <- week_data[week_data$grouping == "implemented_default", ]
    
    x <- c(test_row$subscribers, control_row$subscribers)
    n <- c(test_row$active_users, control_row$active_users)
    
    ##feature 1 causes less users
    if(input$decision1 == TRUE){
      n[1] <- n[1]*0.9
      x[1] <- x[1]*0.9
    }
    answers <- c(input$decision1, input$decision2, input$decision3, input$decision4,
                 input$decision5, input$decision6)
    yesanswers <- sum(answers == TRUE, na.rm = TRUE)
    
    #too many features means less users
    if(yesanswers>= 5){
      n[1] <- n[1]*0.94
      x[1] <- x[1]*0.94
    }
    
    
    rate_test <- round((x[1] / n[1]) * 100, 2)
    rate_control <- round((x[2] / n[2]) * 100, 2)
    rate_diff <- round(rate_test - rate_control, 2)
    sub_diff <- round(x[1] - x[2])
    user_diff <- round(n[1] - n[2])
    
    
    if (load2() == "loaded2")
      #results table
      data.frame(
        Test = c("Subscribers", "Users", "Subscription Rate"),
        Features_added = c(round(x[1]), round(n[1]), paste0(rate_test, "%")),
        No_features_added = c(round(x[2]), round(n[2]), paste0(rate_control, "%")),
        Difference = c(sub_diff, user_diff, paste0(rate_diff, "%"))
        
      )
  })
  
  
  
  output$yearresults <- renderUI({
    validate(
      need(input$decision1, "")
    )
    answers <- c(input$decision1, input$decision2, input$decision3, input$decision4,
                 input$decision5, input$decision6)
    yesanswers <- sum(answers == TRUE, na.rm = TRUE)
    basep <- calcRate()
    label_to_id <- setNames(inputrank, labelsrank)
    ordered_labels <- input$ordered_items 
    ordered_ids <- label_to_id[ordered_labels]
    if ("decision1" %in% ordered_ids && "decision2" %in% ordered_ids) {
      if (match("decision2", ordered_ids) < match("decision1", ordered_ids)) {
        basep <- basep*0.95}}
    if ("decision3" %in% ordered_ids && match("decision3", ordered_ids) == 1) {
      basep <- basep + 0.03}
    if ("decision4" %in% ordered_ids && "decision5" %in% ordered_ids) {
      if (match("decision5", ordered_ids) < match("decision4", ordered_ids)) {
        basep <- basep*0.97}}
    if ("decision6" %in% ordered_ids && match("decision6", ordered_ids) <= 4) {
      basep <- basep * 0.92}
    if ("decision6" %in% ordered_ids && match("decision6", ordered_ids) == 6) {
      basep <- basep + 0.02}
    
    if(load2()=="pressed2"){
      tagList(
        h3("Loading"),
        tags$img(src = "loading.jpg", height = "200px")
      )
    }
    else if (load2() == "loaded2"){
      tagList(
        p(" You chose to introduce", yesanswers, "features."),
        p("Based on the features you chose to introduce, the expected subscription
          rate is", round(basep * 100, 2), "%."),
        p("Here are the actual number of subscribers and users one year later both with the
          features you chose to add and without any added, for comparison:")
        
      )
    }
    else{
      NULL
    }
   })
  feedback <- reactive({
    validate(
    need(input$decision1, ""),
    need(input$decision2, ""),
    need(input$decision3, ""),
    need(input$decision4, ""),
    need(input$decision5, ""),
    need(input$decision6, "")
  )
    label_to_id <- setNames(inputrank, labelsrank)
    ordered_labels <- input$ordered_items 
    ordered_ids <- label_to_id[ordered_labels]
    answers <- c(input$decision1, input$decision2, input$decision3, input$decision4, input$decision5)
    yesanswers <- sum(answers == TRUE, na.rm = TRUE)
    
    
    #overview of impact of order on results displayed
    goodfeedback <- list()
    badfeedback <- list()
    if (load2() == "loaded2"){
 
      if(input$decision1 == TRUE){
        badfeedback <- append(badfeedback, list(p("While reducing the number of hearts
        for the free tier increased subscribers, it also caused a descrease in 
          the number of users.")))}
      if ("decision1" %in% ordered_ids && "decision2" %in% ordered_ids) {
        if (match("decision2", ordered_ids) < match("decision1", ordered_ids)) {
          badfeedback <- append(badfeedback, list(p("Choosing to reduce wait time before
                                              reducing hearts reduced the effectiveness." 
                                              )
       )) } }
      if ("decision1" %in% ordered_ids && "decision2" %in% ordered_ids){
        if (match("decision1", ordered_ids) < match("decision2", ordered_ids)) {
          goodfeedback <- append(goodfeedback, list(p("Choosing to reduce hearts before 
                                              reducing wait time was the more effective order."
                                              )
        ))} }
        
        if ("decision3" %in% ordered_ids && match("decision3", ordered_ids) == 1) {
          goodfeedback <- append(goodfeedback, list(p("Increasing adverts as the first feature
                                              was most effective feature to have first."
                                              )
          )) }
      if ("decision6" %in% ordered_ids && match("decision6", ordered_ids) == 6) {
        goodfeedback <- append(goodfeedback, list(p("Offering the free trial as the last
                                            feature is most effective."
                                            )
        )) }
      if ("decision6" %in% ordered_ids && match("decision6", ordered_ids) <= 4) {
        badfeedback <- append(badfeedback, list(p("Offering the free trial too early is
                                            less effective.")
        )) }
       
      if ("decision4" %in% ordered_ids && "decision5" %in% ordered_ids) {
        if (match("decision5", ordered_ids) < match("decision4", ordered_ids)) {
          badfeedback <- append(badfeedback, list(p("Choosing to introduce subscriber 
          only levels before introducing streaks reduced the
          effectiveness.")))} }
      if ("decision4" %in% ordered_ids && "decision5" %in% ordered_ids) {
        if (match("decision4", ordered_ids) < match("decision5", ordered_ids)) {
          goodfeedback <- append(goodfeedback, list(p("Choosing to introduce streaks before 
          subsciber only levels was
            the more effective order.")))
        }
      }
      if (yesanswers >= 5){
        badfeedback <- append(badfeedback, list(p("You introduced lots of features. This 
          caused a loss in users as the free version was no longer as good.")))
      }    
      
      }
      
    else{
      NULL
    }
    
    
    return(list(good = goodfeedback, bad = badfeedback))
  })
    output$goodresult <- renderUI({
      tagList(feedback()$good)
    })
    
    output$badresult <- renderUI({
      tagList(feedback()$bad)
    })
  
  
  output$yearbarchart <- renderPlot({
    validate(
      need(input$decision1, ""),
      need(input$decision2, ""),
      need(input$decision3, ""),
      need(input$decision4, ""),
      need(input$decision5, ""),
      need(input$decision6, "")
    )
      result <- yearlaterdata()
      
      w <- 2
      week_data <- result[result$week_number == w, ]
      
      test_row <- week_data[week_data$grouping == "implemented_test", ]
      control_row <- week_data[week_data$grouping == "implemented_default", ]
      
      x <- c(test_row$subscribers, control_row$subscribers)
      n <- c(test_row$active_users, control_row$active_users)
      
      ##feature 1 causes less users
      if(input$decision1 == TRUE){
        n[1] <- n[1]*0.9
        x[1] <- x[1]*0.9
      }
      
      answers <- c(input$decision1, input$decision2, input$decision3, input$decision4,
                   input$decision5, input$decision6)
      yesanswers <- sum(answers == TRUE, na.rm = TRUE)
      
      #too many features means less users
      if(yesanswers>= 5){
        n[1] <- n[1]*0.94
        x[1] <- x[1]*0.94
      }
      #bar chart to show users and subscribers
      barchartdf <- data.frame(
        Group = c("Added", "NotAdded"),
        Users = c(n[1], n[2]),
        Subscribers = c(x[1], x[2])
      )
      
      barchartdf <- barchartdf %>%
        mutate(NonSubscribers = Users - Subscribers)
      
      barchartdflong <- barchartdf %>%
        select(Group, Subscribers, NonSubscribers) %>%
        pivot_longer(cols = c(Subscribers, NonSubscribers),
                     names_to = "Type", values_to = "Count")
      #plot
      if (load2() == "loaded2"){
        ggplot(barchartdflong, aes(x = Group, y = Count, fill = Type)) +
          geom_bar(stat = "identity") +
          labs(title = "Subscribers and Users",
               x = "Type", y = "Count") +
          scale_fill_manual(values = c("Subscribers" = "pink", "NonSubscribers" = "lightblue")) +
          theme_bw()
      }
  })

}
# Run the application 
shinyApp(ui = ui, server = server)

