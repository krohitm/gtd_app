#function to plot casualties over years
casualtiesOverYears <- function(terr) {
  #global casualties over years
  graph <- terr %>%
    group_by(year) %>%
    summarise(
      killed = sum(Killed),
      wounded = sum(wounded)
    ) %>%
    gather(key = effect,
           value = value,
           killed,
           wounded) %>%
    ggplot(mapping = aes(x = year, y = value, color = effect)) +
    geom_line() +
    theme(panel.background = NULL,
          axis.text.x = element_text(angle = 45, vjust = 1)) +
    labs(x = "Year", y = "Count")#,
  #title = "Number of casualties over years")
  ggplotly(graph)
}