

#function to visualize maps of factors
visualizeFactorMap <- function(terr, factor, title) {
  world <- map_data("world")
  
  global_table <- 
    terr[c('nation', factor)] %>% 
    group_by_('nation', factor) %>%
    summarise(Total=n()) %>%
    filter(Total==max(Total)) %>%
    group_by(nation)%>%
    rename(region=nation)
  
  
  #join the data found with world data
  worldFactortypes <-
    right_join(global_table, world, by = "region")
  
  worldFactortypes[factor][is.na(worldFactortypes[factor])] <- "Unknown"
  num_unique_factors <- global_table[factor] %>%
    n_distinct()
  
  #View(worldFactortypes)
  
  g5 <-
    ggplot(data = world, aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    ditch_the_axes +
    theme(panel.background = NULL) +
    geom_polygon(data = worldFactortypes,
                 aes_string(label = 'region', fill = (factor)),
                 color = "white") +
    scale_fill_manual(values = c(
      rainbow(
        num_unique_factors,
        s = 0.6,
        v = 0.8,
        start = 0,
        end = max(1, num_unique_factors - 1) /
          num_unique_factors,
        alpha = 0.7
      )
    )) +
    ggtitle(title)
  ggplotly(g5)
}