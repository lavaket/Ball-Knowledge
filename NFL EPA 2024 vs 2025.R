install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")

# Load packages
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

# Load the play-by-play data for 2020 and 2021
pbp <- load_pbp(2024:2025)

# Let's look at the first five rows using head()
pbp %>% head()

# Let's do the same thing but only with a couple columns using select()
pbp %>% select(posteam, defteam, season, down, ydstogo, rush, pass, yards_gained) %>% head()

# Check and see the column names
names(pbp)

# Check how many rows are in pbp
nrow(pbp)

# Clean the dataset to get just rushes and passes using filter()
pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa))

# Now let's see how many rows are in pbp_rp
nrow(pbp_rp)

# Who was Dallas' best quarterback last season?
pbp_rp %>%
  filter(posteam == "DAL") %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarize(passes = n(),
            avg_epa = mean(epa)) %>%
  filter(passes >= 10) %>%
  arrange(-avg_epa)

# Let's compare offense in 2020 vs. 2021
offenses_24 <- pbp_rp %>%
  filter(season == 2024) %>%
  group_by(posteam) %>%
  summarize(epa_24 = mean(epa))

offenses_25 <- pbp_rp %>%
  filter(season == 2025) %>%
  group_by(posteam) %>%
  summarize(epa_25 = mean(epa))

# Joining the two years together and adding the team logos

offenses_all <- offenses_24 %>%
  left_join(offenses_25, by = "posteam")

# Open teams_colors_logos
View(teams_colors_logos)

offenses_all <- offenses_all %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

offenses_all %>%
  ggplot(aes(x = epa_24, y = epa_25)) +
  geom_hline(yintercept = mean(offenses_all$epa_25), linetype = "dashed") +
  geom_vline(xintercept = mean(offenses_all$epa_24), linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_bw() +
  labs(x = "Offensive EPA/Play in 2024",
       y = "Offensive EPA/Play in 2025",
       title = "Offensive EPA/Play in 2024 Compared to 2025",
       caption = "data by nflfastR | @lavaket") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) 

# Saving the plot
ggsave('off-epa-24-25.png', width = 14, height = 10, dpi = "retina")


# Let's evaluate quarterback aggressiveness now

# Using mutate() to create a 'yards_past_sticks' column
pbp_rp <- pbp_rp %>%
  mutate(yards_past_sticks = air_yards - ydstogo)

# Using group_by() and summarize() to evaluate quarterbacks
qb_agg <- pbp_rp %>%
  filter(!is.na(yards_past_sticks)) %>%
  filter(down %in% c(3, 4)) %>% #can just do down > 3 here but wanted to show how %in% works
  group_by(passer_player_name) %>%
  summarize(passes = n(),
            avg_yps = mean(yards_past_sticks),
            team_abbr = last(posteam)) %>%
  filter(passes >= 70) %>%
  left_join(teams_colors_logos, by = "team_abbr")

# Making a bar graph
qb_agg %>%
  ggplot(aes(x = avg_yps, y = fct_reorder(passer_player_name, avg_yps))) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_bw() +
  labs(x = "Average Late-Down Yards Past Sticks",
       y = "Passer Name",
       title = "How Aggressive Each Quarterback is on Late Downs",
       subtitle = "2024 & 2025 Seasons, minimum of 70 passes to be included") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        panel.grid.major.y = element_line(size = 0.1))


qb_gt <- qb_agg %>%
  mutate(avg_yps = round(avg_yps, 2)) %>%
  arrange(-avg_yps) %>%
  mutate(rank = row_number()) %>%
  select(rank, passer_player_name, team_wordmark, avg_yps) 

qb_gt %>%
  gt() %>%
  gtExtras::gt_img_rows(team_wordmark) %>%
  cols_label(passer_player_name = "Passer",
             team_wordmark = "",
             avg_yps = "Late Down Yards Past Sticks") %>%
  gtExtras::gt_theme_espn() %>%
  tab_header(title = "Late Down Yards Past Sticks")