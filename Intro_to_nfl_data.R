install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")
installed.packages("gtExtras")
install.packages("remotes")
remotes::install_github("jthomasmock/gtExtras")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

pbp <- load_pbp(2021:2022)

nrow(pbp)

pbp |> head()

names(pbp)

pbp |> select(posteam, defteam, down, ydstogo, play_type)
# data set with only pass and rush
pbp_rp <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  filter (!is.na(epa))

nrow(pbp_rp)
# who was detroit's best rusher?
pbp_rp|>
  filter(posteam == "DET", rush == 1, !is.na(rusher_player_name)) |> 
  group_by(rusher_player_name) |> 
  summarize(rushes = n(),
            epa_rush = mean(epa)) |> 
  filter(rushes >= 10) |> 
  arrange(-epa_rush)
# who was Carolina's best QB?
pbp_rp |> 
  filter(posteam == "CAR", !is.na(id)) |> 
  group_by(name) |> 
  summarize(name = first(name),
            plays = n(),
            epa_per_play = mean(epa),
            pass_attempts= sum(complete_pass + incomplete_pass, na.rm = T)) |> 
  filter(plays >= 50, pass_attempts >= 10) |> 
  arrange(-epa_per_play)
# create data set on pass efficiency
pass_efficiency <- pbp_rp |> 
  filter(season == 2021, pass == 1) |> 
  group_by(posteam) |> 
  summarise(passes = n(),
            pass_epa = mean(epa))
#create data set on rush efficiency
rush_efficiency <- pbp_rp |> 
  filter(season == 2021, rush == 1) |> 
  group_by(posteam) |> 
  summarise(rushes = n(),
            rush_epa = mean(epa))
#data set total efficiency
total_eff <- pass_efficiency |> 
  left_join(rush_efficiency, by = "posteam")

View(teams_colors_logos)

total_eff <- total_eff |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


total_eff |> 
  ggplot(aes(x = pass_epa, y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff$rush_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(total_eff$pass_epa), linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9)+
  theme_bw() +
  labs(x = "EPA Per Pass",
       y = "EPA Per Rush",
       title = "EPA/Pass and EPA/Rush in 2021",
       subtitle = "Regular season and playoffs included",
       caption = "By Shana Mandelbaum")

ggsave('pass-rush-epa-21.png', width = 14, height = 10, dpi = "retina")









  
  
  
  
  
  