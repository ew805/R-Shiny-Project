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
  titlePanel("Feature 1"),
  h3("Reducing hearts for free tier"),
  sidebarLayout(
    sidebarPanel(
      textOutput("feature1description"),
      textOutput("feature1description2"),
      textOutput("feature1description3")
    ),
    mainPanel(
      radioButtons("dayquestion", "How many days would you like to run the test?",
                   choices =
                     c("1 day", "2 days", "3 days", "4 days")),
      
    )
  )
)
server <- function(input, output) {
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
}

# Run the application 
shinyApp(ui = ui, server = server)
