## Brushing Batting Averages App
## Jim Albert, May 2021
## albert@bgsu.edu

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)

# read in data from Github site
sc2019_ip <- read_delim("https://raw.githubusercontent.com/bayesball/ShinyBaseball/main/data/sc2019_ip.txt",
           delim = " ")

# user interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
         h3("Brushing Batting Average"),
         textInput("name", "Enter Batter Name:",
            value = "Mike Trout"),
         h4("Scatterplot shows pitch locations for balls put
            into play.  Brush over a region of the scatterplot 
            and you will see the in-play batting average
            over the region.")
  )),
  column(8,
         plotOutput("plot", 
                    brush = brushOpts("plot_brush",
                            fill = "#0000ff"),
              width = '455px'),
         tableOutput("data")
         )
)

# server function
server <- function(input, output, session) {
  # graph output
  output$plot <- renderPlot({
    correctinput <- function(st){
      str_to_title(str_squish(st))
    }
    add_zone <- function(){
      topKzone <- 3.5
      botKzone <- 1.6
      inKzone <- -0.85
      outKzone <- 0.85
      kZone <- data.frame(
        x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
        y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
      )
      geom_path(aes(.data$x, .data$y),
                data=kZone, lwd = 1)
    }
    mytitle <- paste("2019",
                     correctinput(input$name),
                     "-", "In-Play AVG")
    th1 <- theme(plot.background =
                   element_rect(fill = "deepskyblue4"),
                 axis.text = element_text(colour = "white"),
                 axis.title = element_text(colour = "white"),
                 plot.title = element_text(
                   colour = "white", size = 14,
                   hjust = 0.5, vjust = 0.8, angle = 0))

    sc2019_ip$HIT <- as.character(sc2019_ip$H)
    ggplot() +
      geom_point(data = filter(sc2019_ip,
              player_name == correctinput(input$name)),
                 aes(plate_x, plate_z, color = HIT)) +
      add_zone() +
      ggtitle(mytitle) +
      scale_colour_manual(values =
                   c("tan", "red")) +
      th1 + coord_equal()
  }, res = 96)

  # table output
  output$data <- renderTable({
    correctinput <- function(st){
      str_to_title(str_squish(st))
    }
    req(input$plot_brush)
    sc1 <- brushedPoints(filter(sc2019_ip,
                player_name == correctinput(input$name)),
                      input$plot_brush)
    data.frame(Name = correctinput(input$name),
               BIP = nrow(sc1),
               H = as.character(sum(sc1$H)),
               BABIP = sum(sc1$H) / nrow(sc1))
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats",
  caption.placement = "top")
}

# function to execute app
shinyApp(ui = ui, server = server)
