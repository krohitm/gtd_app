source('./functions/helper.R', local = T)

#helper function to visualize maps for varaibles for counts
visualizeCountMap <- function(global_table, world, title) {
  
  global_table <-
    global_table %>%
    rename(region = nation)
  
  #join the data found with world data
  world_table <- right_join(global_table, world, by = "region")
  world_table$Total[which(is.na(world_table$Total))] = 0
  
  graph <-
    ggplot(data = world, aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    theme(panel.background = NULL) +
    geom_polygon(data = world_table,
                 aes(label = region, fill = Total),
                 color = "white") +
    scale_fill_gradientn(colours = c("#e1eec3", "#f05053")) +
    #ggtitle(title) +
    ditch_the_axes
  
  #removed plotly for lite version
  #ggplotly(graph) %>%
    return(graph)
}