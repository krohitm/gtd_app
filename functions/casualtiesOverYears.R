#function to plot casualties over years
casualtiesOverYears <- function(terr, xstart=1970, xend = 2015) {
  #global casualties over years
  casualtiesCount <- terr[c('year', 'Killed', 'wounded')] %>%
    group_by(year) %>%
    summarise(
      killed = sum(Killed),
      wounded = sum(wounded)
    ) 
  
  #adding missing years when no casualties occured
  yearRange <- data.frame(year = seq(xstart, xend, by = 1))
  casualtiesCountFull <- yearRange %>%
    left_join(casualtiesCount, by = 'year')
  casualtiesCountFull$killed[is.na(casualtiesCountFull$killed)] <- 0
  casualtiesCountFull$wounded[is.na(casualtiesCountFull$wounded)] <- 0
  
  graph <- casualtiesCountFull %>%
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
  
  #removed plotly for lite version
  #ggplotly(graph) %>%
  return(graph)
}