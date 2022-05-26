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


# Bar plot

p1 <- fiftns_wc_winners %>% 
  ggplot(aes(No_games_won,fct_reorder(winner,No_games_won)))+
  geom_col(aes(fill = No_games_won),) +
  geom_label(aes(label = No_games_won), size = 2) +
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "England & New Zealand Lead the Way In Games Won at The World Cup",
       caption = "Data from Tidy_Tuesday_wk_19 | plot by blake analytics",
       x = "Total Games Won",
       y = "Country") +
  theme_cowplot(11) +
  theme(
    plot.title = element_text(size = 14, family = "Berkshire Swash",colour = "White"),
    axis.text.y = element_text(size = 8, family = "Roboto", colour = "white"),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 9,family = "Roboto", face = "bold", colour = "white"),
    plot.background = element_rect(fill = "#1e2223"),
    axis.line = element_blank(),
    axis.ticks = element_blank()
    
  )

ggdraw(p1) +
  draw_image("https://i.pinimg.com/originals/41/ed/3a/41ed3a981cc90b1f328b87b1e97c9373.png",
             width = 0.2,height = 0.2,vjust = -1.5,hjust = -2.5, scale = 1.7)



ggdraw(p1) +
  draw_image("https://i.pinimg.com/originals/41/ed/3a/41ed3a981cc90b1f328b87b1e97c9373.png",
             scale = 0.5) +
  draw_plot(p1)



#1e2223
