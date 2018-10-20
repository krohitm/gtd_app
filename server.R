library(tidyr)
library(dplyr)
#library(plotly)
library(rworldmap)
library(maps)
library(ggmap)
library(reshape2)
library(raster)
library(rgdal)
library(rgeos)
library(ggmosaic)
library(shiny)

#input data
terr = read.csv(
  './data/globalterrorismdb_0616dist.csv',
  check.names = FALSE,
  header = TRUE,
  stringsAsFactors = FALSE
)

#data cleaning
terr = rename(
  terr,
  id = eventid,
  year = iyear,
  nation = country_txt,
  Region = region_txt,
  attack = attacktype1_txt,
  target = targtype1_txt,
  weapon = weaptype1_txt,
  Killed = nkill,
  wounded = nwound
)


terr$Killed = as.integer(terr$Killed)
terr$wounded = as.integer(terr$wounded)

terr$Killed[which(is.na(terr$Killed))] = 0
terr$wounded[which(is.na(terr$wounded))] = 0

terr$casualties = as.integer(terr$Killed + terr$wounded)

terr$nation[terr$nation == "United States"] <- "USA"
terr$nation[terr$nation == "United Kingdom"] <- "UK"
terr$nation[terr$nation == "People's Republic of the Congo"] <-
  "Republic of Congo"
terr$nation[terr$nation == "Bosnia-Herzegovina"] <-
  "Bosnia and Herzegovina"
terr$nation[terr$nation == "Slovak Republic"] <- "Slovakia"


global_t <-
  terr %>%
  group_by(year, nation, Region) %>%
  summarize(Total = n())

global_y <-
  global_t %>%
  group_by(year) %>%
  summarize(Total = sum(Total))

global_attacks <-
  global_t %>%
  group_by(nation) %>%
  summarize(Total = sum(Total)) %>%
  arrange(desc(Total))

attach(global_attacks)
global_n <- global_attacks[order(-Total), ]
detach(global_attacks)

gy <- global_y %>%
  ggplot(mapping = aes(year, Total)) +
  geom_line(color = "red") +
  theme(
    legend.position = "none",
    panel.background = NULL,
    axis.text.x = element_text(angle = 45, vjust = 1)
  ) +
  labs(x = "Year", y = "Number of attacks", 
       title = "Number of global attacks over years")
#ggplotly(gy, width = 800, height = 480)

global_kills_years <-
  terr %>%
  group_by(year) %>%
  summarize(killed = sum(Killed))

global_wound_years <-
  terr %>%
  group_by(year) %>%
  summarize(wounded = sum(wounded))

globe <-
  global_kills_years %>%
  inner_join(global_wound_years, by = "year") %>%
  inner_join(global_y)

df <- melt(globe, "year")
df = rename(df, effect = variable)

gky <- df %>%
  ggplot(mapping = aes(x = year, y = value, color = effect)) +
  geom_line() +
  theme(panel.background = NULL,
        axis.text.x = element_text(angle = 45, vjust = 1)) +
  labs(x = "Year", y = "Count", title = "Number of people killed/wounded over years against attacks")
#ggplotly(gky, width = 800, height = 450)

## attacks by highest casualties(killed+wounded)
terr$casualties = as.integer(terr$Killed + terr$wounded)
terr$casualties[which(is.na(terr$casualties))] = 0

g_max_cas <- terr %>%
  top_n(10, casualties) %>%
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
  labs(x = "Target of attack", y = "Number of casulaties", title = "Terrorist attacks with most casualties")
#ggplotly(g_max_cas)


gname_max_cas <- terr[c('gname', 'casualties')] %>%
  filter(gname != 'Unknown') %>%
  group_by(gname) %>%
  summarize(Total = n())

g <- gname_max_cas %>%
  top_n(40, Total) %>%
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
  labs(x = "Terrorist group", y = "Number of casulaties", title = "Terrorist groups leading to most casualties")
#ggplotly(g, width = 800, height = 450)

#40 countries with maximum number of terrorist attacks, and 40 countries with least number of terrorist attacks
g2 <- global_n %>%
  top_n(40) %>%
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
  labs(x = "Countries", y = "Number of attacks", title = "Countries with most number of terrorist attacks")

#ggplotly(g2, width = 800, height = 450)

g2 <- global_n %>%
  top_n(-40) %>%
  ggplot(mapping = aes(
    x = reorder(nation, Total),
    y = Total,
    fill = nation
  )) +
  geom_bar(stat = 'identity') +
  theme(
    legend.position = "none",
    panel.background = NULL,
    axis.text.x = element_text(angle = 90, vjust = 1)
  ) +
  labs(x = "Countries", y = "Number of attacks", title = "Countries with least number of terrorist attacks")
#ggplotly(g2, width = 800, height = 450)


# Relationships and inferences

## Casualties by region
g1 <- terr %>%
  ggplot(aes(x = Region, y = casualties, fill = Region)) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x =  element_text(angle = 45))

ylim1 = boxplot.stats(terr$casualties)$stats[c(1, 5)]
g2 <- g1 + coord_cartesian(ylim = ylim1 * 1.05)
#ggplotly(g2)

## Casualties by attack type
g1 <- terr %>%
  ggplot(aes(x = attack, y = casualties, fill = attack)) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x =  element_text(angle = 45))

ylim1 = boxplot.stats(terr$casualties)$stats[c(1, 5)]
g2 <- g1 + coord_cartesian(ylim = ylim1 * 1.05)
#ggplotly(g2, height = 500)

## Casualties by weapon

g1 <- terr %>%
  ggplot(aes(x = weapon, y = casualties, fill = weapon)) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x =  element_text(angle = 45))

ylim1 = boxplot.stats(terr$casualties)$stats[c(1, 5)]
g2 <- g1 + coord_cartesian(ylim = ylim1 * 1.05)
#ggplotly(g2, height = 500)

## Casualties by target

g1 <- terr %>%
  ggplot(aes(x = target, y = casualties, fill = target)) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x =  element_text(angle = 45))

ylim1 = boxplot.stats(terr$casualties)$stats[c(1, 5)]
g2 <- g1 + coord_cartesian(ylim = ylim1 * 1.05)
#ggplotly(g2, height = 500)

## Casualties by type of attack

g1 <- terr %>%
  filter(INT_ANY != -9) %>%
  ggplot(aes(
    x = factor(INT_ANY),
    y = casualties,
    fill = INT_ANY
  )) +
  geom_boxplot() +
  theme(legend.position = "none",
        axis.text.x =  element_text(angle = 45))

ylim1 = boxplot.stats(terr$casualties)$stats[c(1, 5)]
g2 <- g1 + coord_cartesian(ylim = ylim1 * 1.05)
#ggplotly(g2, height = 500)

#getting proportions across attack and region
t = table(data.frame(terr$attack, terr$Region))

prop.table(t, 2) * 100

g <- ggplot(data = terr) +
  geom_mosaic(aes(fill = attack, x = product(Region))) +
  labs(x = "Attack type", y = "Proportion") +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.background = NULL,
    axis.text.y = element_text(angle = 50, vjust = 1)
  )
#g
#ggplotly(g, height = 500)


#getting proportions across attack and region
t = table(data.frame(terr$attack, terr$target))

prop.table(t, 2) * 100

g <- ggplot(data = terr) +
  geom_mosaic(aes(fill = attack, x = product(target))) +
  labs(x = "Attack type", y = "Proportion") +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.background = NULL,
    axis.text.y = element_blank()
  )#element_text(angle=50, vjust=1))
#g
#ggplotly(g, height = 500)


#getting proportions across attack and region
attacks_known_ntnlty <- terr %>%
  filter(INT_ANY != -9)
t = table(data.frame(attacks_known_ntnlty$INT_ANY, attacks_known_ntnlty$Region))

prop.table(t, 2) * 100

g <- ggplot(data = attacks_known_ntnlty) +
  geom_mosaic(aes(fill = factor(INT_ANY), x = product(Region))) +
  labs(x = "Region", y = "Proportion") +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.background = NULL,
    axis.text.y = element_blank()
  )#element_text(angle=50, vjust=1))
#g
#ggplotly(g)


## Ideology or logistics of attacks in top 10 most attacked countries

#getting proportions across attack and region
most_attacked <- global_n %>%
  top_n(10, Total)

most_attacked_nations <- terr %>%
  filter(nation %in% most_attacked$nation & INT_ANY != -9)

t = table(data.frame(most_attacked_nations$INT_ANY, most_attacked_nations$nation))

prop.table(t, 2) * 100

g <- ggplot(data = most_attacked_nations) +
  geom_mosaic(aes(fill = factor(INT_ANY), x = product(nation))) +
  labs(x = "Region", y = "Proportion") +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.background = NULL,
    axis.text.y = element_text(angle = 50, vjust = 1)
  )

#ggplotly(g)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$singleVar <- renderPlot({
    #global_y %>%
    #  ggplot(mapping=aes(year,Total))+
    #  geom_line(color="red")+
    #  theme(legend.position="none", panel.background = NULL, axis.text.x = element_text(angle=45, vjust = 1))+
    #  labs(x="Year", y="Number of attacks", title="Number of global attacks over years")
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x,
    #     breaks = bins,
    #     col = 'darkgray',
    #     border = 'white')
    gy
    
  })
  
})
