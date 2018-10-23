#global attacks over years
attacksOverYears <- function(terr) {
  graph <- terr %>%
    group_by(year) %>%
    summarize(NoOfAttacks = n()) %>%
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