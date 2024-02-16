library(shiny)

#options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Mes onglets"),

  mainPanel(
    tabsetPanel(
      tabPanel("Selector",
               sliderInput("bins",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30)
               ),
      tabPanel("Plot",
               plotOutput("distPlot")
               )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
print("toto")
shinyApp(ui = ui, server = server)
