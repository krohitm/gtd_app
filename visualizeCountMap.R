source('helper.R', local = T)

#helper function to visualize maps for varaibles for counts
visualizeCountMap <- function(global_table, title) {
  #import world map data
  world <- map_data("world")
  
  global_table <-
    global_table %>%
    rename(region = nation)
  
  #join the data found with world data
  world_table <- right_join(global_table, world, by = "region")
  world_table$Total[which(is.na(world_table$Total))] = 0
  
  graph <-
    ggplot(data = world, aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    theme(legend.position = "none", panel.background = NULL) +
    geom_polygon(data = world_table,
                 aes(fill = Total),
                 color = "white") +
    scale_fill_gradientn(colours = c("#e1eec3", "#f05053")) +
    #ggtitle(title) +
    ditch_the_axes
  
  ggplotly(graph)
}