#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Time to next eruption"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            #choose output graph
            radioButtons("plottype", "Plot Type", choices = 
                           c("Histogram", "Density")),
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)
#choose output graph
radioButtons("plottype", "Plot Type", choices = 
               c("Histogram", "Density"))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram or density depending on chosen
        if(input$plottype == "Histogram"){
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')}
        else{
          hist(x, breaks = bins, col = 'darkgray', border = 'white',
               xlab = 'Waiting time to next eruption (in mins)',
               main = 'Histogram of waiting times (density)', probability = TRUE)
        lines(density(x), col = 'red', lwd = 2)}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
