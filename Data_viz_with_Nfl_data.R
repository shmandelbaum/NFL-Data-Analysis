install.packages("ggrepel")
install.packages("ggthemes")
install.packages("scales")
install.packages("webshot2")
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(webshot2)

pbp <- load_pbp(2023)

#correlation between how often a qb passes and epa per play

qb_epa_play <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  filter(!is.na(epa)) |> 
  group_by(id) |> 
  summarise(name = first(name),
            team = last(posteam),
            plays = n(),
            epa_play = mean(epa),
            pass_attempts = sum(complete_pass + incomplete_pass, na.rm = T))|> 
  filter(plays >= 250, pass_attempts >= 75) |> 
  mutate(pass_rate = pass_attempts/plays) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

qb_epa_play |> 
  ggplot(aes(x = pass_rate, y = epa_play)) +
  geom_point(aes(fill = team_color, color = team_color2, size=plays),
             shape = 21, alpha= 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = name)) +
  theme_bw() +
  geom_hline(yintercept = mean(qb_epa_play$epa_play), linetype = "dashed") +
  geom_vline(xintercept = mean(qb_epa_play$pass_rate), linetype = "dashed") +
  labs(x = "Pass Rate",
       y = "EPA/Play",
       title = "EPA/Play and Pass Rate, 2023",
       subtitle = "Minimum of 250 plays and 75 pass attempts to be included") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust= 0.5))
ggsave('epa-pass-rate23.png', width = 14, height = 10, dpi = "retina")
  
qb_epa_play |> 
  ggplot(aes(x = epa_play, y = fct_reorder(name, epa_play))) +
  geom_bar(aes(fill = team_color, color=team_color2), stat = "identity", alpha=0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_minimal() +
  geom_image(aes(image = team_logo_espn, 
                 x = ifelse(epa_play > 0, epa_play +.01, epa_play - .01)),
             asp = 16/9, size = .035) +
  labs(x = "EPA/Play",
       y = "Quarterback",
       title= "Each QB's EPA/Play, 2023",
       subtitle = "Min of 250 plays and 75 pass attempts") +
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust= 0.5))
ggsave('qb_bar_epa.png', width = 14, height = 10, dpi = "retina")

qb_gt <- qb_epa_play |> 
  arrange(-epa_play) |> 
  mutate(rank = row_number()) |> 
  dplyr::select(rank, name, team_wordmark, pass_attempts, plays, pass_rate, epa_play) |> 
  mutate(pass_rate = 100*round(pass_rate, 3),
         epa_play = round(epa_play, 2)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_wordmark) |> 
  cols_label(rank = "Rank",
             name = "Quarterback",
             team_wordmark = "",
             pass_attempts = "Pass Attempts",
             plays= "Plays",
             pass_rate = "Pass Rate",
             epa_play = "EPA/Play") |> 
  gtExtras::gt_theme_espn() |> 
  gtExtras::gt_hulk_col_numeric(epa_play)
gtsave(qb_gt, "qb_gt.png")





