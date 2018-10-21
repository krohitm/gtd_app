# function for attacks by highest casualties(killed+wounded)
highCasualtyFactor <- function(data, factor, var, xlab, ylab) {
  graph <- data %>%
    ggplot(mapping = aes_string(x = factor,
                                y = var,
                                fill = var)) +
    geom_bar(stat = 'identity') +
    theme(
      legend.position = "none",
      panel.background = NULL,
      axis.text.x =  element_text(angle = 50, vjust = 1)
    ) +
    scale_fill_gradientn(colours = c("#e1eec3", "#f05053")) +
    labs(x = xlab, y = ylab)
  ggplotly(graph) %>%
    return()
}