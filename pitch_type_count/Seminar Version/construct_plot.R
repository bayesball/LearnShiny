construct_plot <- function(sc_data,
                           counts,
                           pitch_types,
                           pid){
  
  add_zone <- function(color = "black"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    geom_path(aes(.data$x, .data$y),
              data=kZone, lwd = 1, color = color)
  }
  
  sc_new <- filter(sc_data,
                   Count %in% counts,
                   pitch_type %in% pitch_types,
                   pitcher == pid)
  
  ggplot() +
    geom_point(data = sc_new,
               aes(plate_x, plate_z),
               size = 1, color = "red") +
    add_zone() +
    coord_equal() +
    xlim(-2.5, 2.5) +
    ylim(0, 5) +
    facet_grid(Count ~ pitch_type) +
    labs(title = paste("Pitcher", pid)) 
}