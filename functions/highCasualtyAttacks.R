
# function for attacks by highest casualties(killed+wounded)
highCasualtyAttacks <- function(terr, n) {
  graph <- terr %>%
    top_n(n, casualties) %>%
    ggplot(mapping = aes(
      x = reorder(target1,-casualties),
      y = casualties,
      fill = target1
    )) +
    geom_bar(stat = 'identity') +
    theme(
      legend.position = "none",
      panel.background = NULL,
      axis.text.x =  element_text(angle = 50, vjust = 1)
    ) +
    labs(x = "Target of attack", y = "Number of casualties",
         title = "Terrorist attacks with most casualties")
  ggplotly(graph) %>%
    return()
}
