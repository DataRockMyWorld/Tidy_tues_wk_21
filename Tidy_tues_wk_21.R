# Setting Up Environment

library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(showtext)
library(cowplot)

font_add_google("Roboto","Roboto")
font_add_google("Berkshire Swash","Berkshire Swash")
font_add_google("Bungee Shade","Bungee Shade")

showtext_auto()


#Importing Data

tidy_data <- tt_load("2022-05-24")

fifteens <- tidy_data$fifteens
sevens <- tidy_data$sevens

#Exploring the fifteens Dataset

skim(fifteens)


# Rugby fifteens cummulative world cup victory margins

#wc winners and number of games won
fifteens_wc <- fifteens %>%
  filter(tournament == "World Cup") %>% 
  select(date, margin_of_victory, winner) %>%
  group_by(winner) %>% count() %>% 
  arrange(-n) %>% 
  rename("No_games_won" = n)

# Cummulative Margin of victory by winners
margin_victory <- fifteens %>% 
  filter(tournament == "World Cup")%>% 
  group_by(winner) %>%
  summarise(total_vic_margin = sum(margin_of_victory)) %>% 
  arrange(-total_vic_margin)

# Joining the Two Datasets
fiftns_wc_winners <- inner_join(fifteens_wc,margin_victory) %>% 
  filter(winner != "Draw") 


# Bar plot Top 11

p2 <- fiftns_wc_winners %>%
  arrange(-No_games_won) %>% 
  head(11) %>% 
  ggplot(aes(fct_reorder(winner,No_games_won),No_games_won)) +
  geom_col(aes(fill = No_games_won)) +
  geom_label(aes(label = winner), size = 2.5,fill = "black",color = "white",show.legend = F) +
  scale_fill_viridis_c(option = "inferno") +
  ylim(-60,60) +
  coord_polar(start = 0) +
  labs(title = "England & New Zealand Lead the Way In Games Won\n at The World Cup",
       caption = "Data from Tidy_Tuesday_wk_19 | plot by blake analytics",
       x = "Total Games Won",
       y = "Country") +
  theme_cowplot(11) +
  theme(
    plot.title = element_text(size = 14, family = "Berkshire Swash",colour = "black"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#eeeee4"),
    panel.background = element_rect(fill = "#eeeee4"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    
    
  )


ggsave("p_cool.png")


p_cool <- ggdraw(p2) +
  draw_image("https://i.pinimg.com/originals/41/ed/3a/41ed3a981cc90b1f328b87b1e97c9373.png",
             width = 0.2,height = 0.15,vjust = -3,hjust = -2.0, scale = 1.1)













