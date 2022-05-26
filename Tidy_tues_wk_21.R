# Setting Up Environment

library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(showtext)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(magick)
library(cowplot)

font_add_google("Roboto","Roboto")
font_add_google("Berkshire Swash","Berkshire Swash")
font_add_google("Bungee Shade","Bungee Shade")

showtext_auto()

devtools::install_github("ropensci/rnaturalearthhires") 
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
  filter(winner != "Draw") %>% 
  rename("sovereignt" = winner)



# Plot of winners and their no of wins
world <- ne_countries(scale = "medium", returnclass ="sf") 

world1 <- full_join(world,fiftns_wc_winners, by = "sovereignt")

world1 %>% 
  ggplot() +
  geom_sf() +
  geom_sf(aes(fill = No_games_won)) +
  scale_color_gradient2(midpoint = mid, low = "#dd8a0b",
                        mid = "grey92", high = "#32a676") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#abdbe3"),
    panel.grid.major = element_line(color = "grey50", linetype = "dashed",size = 0.5),
    
  )



world_filter <- world1 %>% filter(sovereignt %in% countries)


countries <- c("England","New Zealand","United States","France",
  "Canada","Australia","Spain","Kazakhstan","Ireland","Wales",
  "Scotland","Japan","Italy","Netherlands","Samoa","South Africa",
  "Sweden","Hong Kong","Fiji","Germany","Thailand")

# Bar plot

p1 <- fiftns_wc_winners %>% 
  ggplot(aes(No_games_won,fct_reorder(sovereignt,No_games_won)))+
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
    panel.background = element_rect(fill = "#1e2223"),
    plot.background = element_rect(fill = "#1e2223"),
    axis.line = element_blank(),
    axis.ticks = element_blank()
    
  )

ggdraw(p1) +
  draw_image("https://d3gbf3ykm8gp5c.cloudfront.net/content/uploads/2017/08/06215750/1022.6666666666666x767__origin__0x0_Womens_World_Cup_pic.jpg",
             width = 0.2,height = 0.2,vjust = -1.5,hjust = -2.5, scale = 1.5)



ggdraw(p1) +
  draw_image("https://d3gbf3ykm8gp5c.cloudfront.net/content/uploads/2017/08/06215750/1022.6666666666666x767__origin__0x0_Womens_World_Cup_pic.jpg",
             scale = 0.3) +
  draw_plot(p1)




