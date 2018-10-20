#function for terrorist groups leading to high casualties

highCasualtyGroups <- function(terr, n) {
  terr[c('gname', 'casualties')] %>%
    filter(gname != 'Unknown') %>%
    group_by(gname) %>%
    summarize(Total = n()) %>%
    top_n(n, Total) %>%
    ggplot(mapping = aes(
      x = reorder(gname,-Total),
      y = Total,
      fill = gname
    )) +
    geom_bar(stat = 'identity') +
    theme(
      legend.position = "none",
      panel.background = NULL,
      axis.text.x =  element_text(angle = 50, vjust = 1)
    ) +
    labs(x = "Terrorist group", y = "Number of casulaties",
         title = "Terrorist groups leading to most casualties")
}