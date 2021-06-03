# load necessary packages
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# read in data from Github
sc_pitcher_2019 <- read_delim(
  "https://raw.githubusercontent.com/bayesball/ShinyBaseball/main/data/sc_pitcher_2019.txt",
  delim = " ")

# read in function that constructs the graph
source("construct_plot.R")

# user interface function
ui <- fluidPage(
  column(4, wellPanel(

  h3("Pitch Locations by Pitch Type & Count"),
  textInput(inputId = "pid",
            label = "Pitcher MLBAM Id:",
            value = "605400"),

  checkboxGroupInput(inputId = "pitch_type",
                     label = "Pitch Type:",
                     choices = c("CH", "CU", "EP", "FC",
                                 "FF", "FO",  "FS", "FT",
                                 "KC", "KN", "SI", "SL"),
                     selected = "FF",
                     inline = TRUE),

  checkboxGroupInput(inputId = "count",
                     label = "Count:",
                     choices = c("0-0", "1-0", "0-1", "2-0",
                                "1-1", "0-2", "3-0", "2-1", "1-2",
                                "3-1", "2-2", "3-2"),
                    selected = "0-0",
                    inline = TRUE)
  )),
  column(8,
         plotOutput(
           outputId = "plot",
           height = '540px')
         )
)

# server function
server <- function(input, output, session) {

  output$plot <- renderPlot({
    # make sure you have inputs
    req(length(input$count) > 0  &
        nchar(input$pid) > 0 &
        length(input$pitch_type) > 0)
    # runs function that creates graph
    construct_plot(sc_pitcher_2019,
               input$count,
               input$pitch_type,
               input$pid
               )

}, res = 96)
}

shinyApp(ui = ui, server = server)
