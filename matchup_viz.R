library(tidyverse)
library(baseballr)
library(Lahman)
#so that we can match baseball savant hitter IDs to a player name
#okay, so we didn't end up using Lahman
library(RColorBrewer)
library(stringr)
#needs stringr to extract batter names from the description of the event

data <- read.csv("https://raw.githubusercontent.com/benhowell71/MLB_Projects/main/savant_data_matchup.csv")
#read file from GitHub

results <- data %>%
  #extract the batter name from the description of the play
  mutate(batter_name = word(des, start = 1, end = 2)) %>%
  group_by(player_name, batter_name) %>%
  summarise(n = n(), 
            wOBA = round(mean(woba_value, na.rm = TRUE), digits = 3),
            xwOBA = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), digits = 3),
            xBA = round(mean(estimated_ba_using_speedangle, na.rm = TRUE), digits = 3),
            .groups = "keep") %>%
  filter(batter_name != "Padres challenged") %>%
  #filter out weird instances
  #pretty sure there was only such event
  filter(n > 25)

results %>%
  ggplot() +
  geom_tile(aes(x = player_name, y = batter_name, fill = wOBA)) +
  geom_text(aes(x = player_name, y = batter_name, label = wOBA)) +
  scale_fill_gradient2(low = "lightblue", mid = "lightgreen", high = "red", midpoint = 0.344) +
  #can play around with the color scheme
  #but the data is on a continous scale I believe
  theme_minimal() +
  labs(title = "wOBA by Pitcher vs Batter (19-20 SDP vs LAD)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
