

# function for attacks by highest casualties(killed+wounded)
highCasualtyAttacks <- function(terr, n) {
  terr$casualties <- as.integer(terr$Killed + terr$wounded)
  terr$casualties[which(is.na(terr$casualties))] <- 0
  
  terr %>%
    top_n(n, casualties) %>%
    ggplot(mapping = aes(
      x = reorder(target1, -casualties),
      y = casualties,
      fill = target1
    )) +
    geom_bar(stat = 'identity') +
    theme(
      legend.position = "none",
      panel.background = NULL,
      axis.text.x =  element_text(angle = 25, vjust = 1)
    ) +
    labs(x = "Target of attack", y = "Number of casualties",
         title = "Terrorist attacks with most casualties")
}
