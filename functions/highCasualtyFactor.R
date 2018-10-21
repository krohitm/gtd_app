# function for attacks by highest casualties(killed+wounded)
highCasualtyFactor <- function(data, factor, var, xlab, ylab) {
  graph <- data %>%
    ggplot(mapping = aes_string(
      x = factor,
      y = var,
      fill = factor
    )) +
    geom_bar(stat = 'identity') +
    theme(
      legend.position = "none",
      panel.background = NULL,
      axis.text.x =  element_text(angle = 50, vjust = 1)
    ) +
    labs(x = xlab, y = ylab)
  ggplotly(graph) %>%
    return()
}