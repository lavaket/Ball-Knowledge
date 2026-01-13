library(nflreadr)

teams <- load_teams() %>%
  select(team_abbr, team_logo_espn)

library(tidyverse)
library(nflreadr)
library(ggimage)
library(ggrepel)

pbp <- load_pbp(2025)
rosters <- load_rosters(2025) %>%
  select(gsis_id, full_name, position, team)

teams <- load_teams() %>%
  select(team_abbr, team_logo_espn)

pbp_pass <- pbp %>%
  filter(pass == 1, !is.na(epa))

wr_2025_top25 <- pbp_pass %>%
  filter(!is.na(receiver_player_id)) %>%
  group_by(
    player_id = receiver_player_id,
    player = receiver_player_name
  ) %>%
  summarize(
    targets = n(),
    epa_per_target = mean(epa, na.rm = TRUE),
    team_abbr = last(posteam),
    .groups = "drop"
  ) %>%
  filter(targets >= 100) %>%                 # noise control
  slice_max(epa_per_target, n = 25) %>%     # TOP 25
  left_join(teams, by = "team_abbr")

ggplot(wr_2025_top25, aes(targets, epa_per_target)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_text_repel(aes(label = player), size = 3, max.overlaps = Inf) +
  theme_bw() +
  labs(
    title = "Top 25 Receivers by EPA per Target (2025)",
    x = "Targets",
    y = "EPA per Target",
    caption = "Min 50 targets | data: nflverse"
  )

ggsave('WR-epa.png', width = 14, height = 10, dpi = "retina")

