library(tidyverse)
library(nflfastR)
library(nflreadr)
library(ggimage)
library(ggrepel)


pbp <- load_pbp(2025:2025)

qb_plays <- pbp %>%
  filter(pass == 1, !is.na(epa), !is.na(passer_player_id), !is.na(defteam)) %>%
  filter(sack == 0 | is.na(sack))  # Include or exclude sacks as desired

# Calculate defensive EPA allowed (strength of opposition)
def_strength <- qb_plays %>%
  group_by(defteam) %>%
  summarize(def_epa_allowed = mean(epa, na.rm = TRUE), .groups = "drop")

# Calculate QB DVOA
qb_dvoa <- qb_plays %>%
  left_join(def_strength, by = "defteam") %>%
  group_by(passer_player_id, passer_player_name, posteam) %>%
  summarize(
    plays = n(),
    raw_epa_per_play = mean(epa, na.rm = TRUE),
    avg_def_faced = mean(def_epa_allowed, na.rm = TRUE),
    dvoa_epa = mean(epa - def_epa_allowed, na.rm = TRUE),  # Adjust for defense
    .groups = "drop"
  ) %>%
  filter(plays >= 100) %>%  # Minimum play threshold
  arrange(desc(dvoa_epa))


qb_dvoa <- qb_dvoa %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

ggplot(qb_dvoa, aes(x = raw_epa_per_play, y = dvoa_epa)) +
  geom_hline(yintercept = mean(qb_dvoa$dvoa_epa, na.rm = TRUE), 
             linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = mean(qb_dvoa$raw_epa_per_play, na.rm = TRUE), 
             linetype = "dashed", color = "gray50") +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.04) +
  geom_text_repel(aes(label = passer_player_name), 
                  size = 3, 
                  max.overlaps = 15,
                  box.padding = 0.5) +
  theme_bw() +
  labs(
    x = "Raw EPA per Play",
    y = "Defense-Adjusted EPA per Play (DVOA-style)",
    title = "QB Performance: Raw EPA vs Defense-Adjusted EPA (2025)",
    subtitle = "Minimum 100 pass attempts",
    caption = "data by nflfastR | DVOA-style metric adjusts for opponent defensive strength"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  )

ggsave('qb-dvoa-epa-25.png', width = 14, height = 10, dpi = "retina")

# View top QBs
print(qb_dvoa %>% select(passer_player_name, posteam, plays, raw_epa_per_play, dvoa_epa) %>% head(n = 32))