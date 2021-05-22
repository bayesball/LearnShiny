library(shiny)
library(ggplot2)
ui <- fluidPage(
  column(4, 
    sliderInput("span", "Smoothing parameter:",
                min = 0.1, max = 1, value = 0.4)
  ),
  column(8,
    plotOutput("my_smooth") 
  )
)
server <- function(input, output, session) {
    output$my_smooth <- renderPlot(
      ggplot(mtcars, aes(disp, mpg)) +
        geom_point() +
        geom_smooth(method = "loess",
                    formula = "y ~ x",
                    span = input$span)
    )
}
shinyApp(ui = ui, server = server)
