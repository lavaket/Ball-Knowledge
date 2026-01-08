library(tidyverse)
library(nflfastR)
library(ggimage)

pbp <- load_pbp(2024:2024)

pbp_rp <- pbp %>%
  filter((rush == 1 | pass == 1), !is.na(epa), !is.na(posteam), !is.na(defteam))

off_2024 <- pbp_rp %>%
  filter(season == 2024) %>%
  group_by(team = posteam) %>%
  summarize(off_epa = mean(epa, na.rm = TRUE), .groups = "drop")

def_2024 <- pbp_rp %>%
  filter(season == 2024) %>%
  group_by(team = defteam) %>%
  summarize(def_epa = mean(-epa, na.rm = TRUE), .groups = "drop")

epa_2024 <- off_2024 %>%
  left_join(def_2024, by = "team") %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

ggplot(epa_2024, aes(off_epa, def_epa)) +
  geom_hline(yintercept = mean(epa_2024$def_epa, na.rm = TRUE), linetype = "dashed") +
  geom_vline(xintercept = mean(epa_2024$off_epa, na.rm = TRUE), linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_bw() +
  labs(x = "Offensive EPA per Play",
       y = "Deffensive EPA per Play",
       title = "Offensive vs. Defensive EPA per Play in 2024",
       caption = "data by nflfastR | @lavaket") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) 


ggsave('off-def-epa-24.png', width = 14, height = 10, dpi = "retina")