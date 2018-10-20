#function for plotting countries with highest number of attacks

mostAttackedCountries <- function(terr, n) {
  terr %>%
    group_by(nation) %>%
    summarize(Total = n()) %>%
    arrange(desc(Total)) %>%
    top_n(n, Total) %>%
    ggplot(mapping = aes(
      x = reorder(nation,-Total),
      y = Total,
      fill = nation
    )) +
    geom_bar(stat = 'identity') +
    theme(
      legend.position = "none",
      panel.background = NULL,
      axis.text.x = element_text(angle = 90, vjust = 1)
    ) +
    labs(x = "Countries", y = "Number of attacks",
         title = "Countries with most number of terrorist attacks")
}
