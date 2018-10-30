#global attacks over years
attacksOverYears <- function(terr, xstart=1970, xend=2015) {
  attacksCount <- terr %>%
    group_by(year) %>%
    summarize(NoOfAttacks = n()) 
  
  #adding missing years when no attacks occured
  yearRange <- data.frame(year = seq(xstart, xend, by = 1))
  attacksCountFull <- yearRange %>%
    left_join(attacksCount, by = 'year')
  attacksCountFull$NoOfAttacks[is.na(attacksCountFull$NoOfAttacks)] <- 0
  
  graph <- attacksCountFull %>%
    ggplot(mapping = aes(year, NoOfAttacks)) +
    geom_line(color = "red") +
    theme(
      legend.position = "none",
      panel.background = NULL,
      axis.text.x = element_text(angle = 45, vjust = 1)
    ) +
    labs(x = "Year", y = "Number of attacks")
  
  #removed plotly for lite version
  #ggplotly(graph) %>%
  return(graph)
}