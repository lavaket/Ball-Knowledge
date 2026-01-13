library(tidyverse)
library(nflfastR)
library(ggimage)
library(gganimate)
library(gifski)

# Load 2025 season data
pbp <- load_pbp(2025:2025)

# Filter for rush and pass plays
pbp_rp <- pbp %>%
  filter((rush == 1 | pass == 1), !is.na(epa), !is.na(posteam), !is.na(defteam), !is.na(week))

# Calculate cumulative EPA by week for each team
cumulative_epa <- pbp_rp %>%
  arrange(posteam, week) %>%
  group_by(posteam, week) %>%
  summarize(
    week_off_epa = mean(epa, na.rm = TRUE),
    week_plays = n(),
    .groups = "drop"
  ) %>%
  group_by(posteam) %>%
  arrange(week) %>%
  mutate(
    cumulative_off_epa = cummean(week_off_epa),
    total_plays = cumsum(week_plays)
  ) %>%
  ungroup()

# Calculate cumulative defensive EPA
cumulative_def_epa <- pbp_rp %>%
  arrange(defteam, week) %>%
  group_by(defteam, week) %>%
  summarize(
    week_def_epa = mean(-epa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(defteam) %>%
  arrange(week) %>%
  mutate(
    cumulative_def_epa = cummean(week_def_epa)
  ) %>%
  ungroup()

# Combine offensive and defensive data
team_evolution <- cumulative_epa %>%
  left_join(
    cumulative_def_epa %>% select(team = defteam, week, cumulative_def_epa),
    by = c("posteam" = "team", "week" = "week")
  ) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  filter(!is.na(team_logo_espn))

# Create the animated plot
anim_plot <- ggplot(team_evolution, 
                    aes(x = cumulative_off_epa, y = cumulative_def_epa)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", alpha = 0.5) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.06) +
  
  # Add quadrant labels
  annotate("text", x = 0.15, y = 0.15, label = "ELITE", 
           size = 8, color = "darkgreen", alpha = 0.3, fontface = "bold") +
  annotate("text", x = -0.15, y = 0.15, label = "DEFENSE\nWINS", 
           size = 7, color = "steelblue", alpha = 0.3, fontface = "bold") +
  annotate("text", x = 0.15, y = -0.15, label = "OFFENSE\nWINS", 
           size = 7, color = "orange3", alpha = 0.3, fontface = "bold") +
  annotate("text", x = -0.15, y = -0.15, label = "STRUGGLING", 
           size = 8, color = "darkred", alpha = 0.3, fontface = "bold") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0.5)
  ) +
  labs(
    x = "Offensive EPA per Play (Higher = Better Offense)",
    y = "Defensive EPA per Play (Higher = Better Defense)",
    title = "NFL Team Performance Evolution: 2025 Season",
    subtitle = "Week {closest_state}",
    caption = "data by nflfastR | animation by gganimate"
  ) +
  coord_cartesian(clip = "off") +
  
  # Animation settings
  transition_states(week, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out') +
  enter_fade() +
  exit_fade()

# Render and save animation
animate(
  anim_plot,
  nframes = 200,
  fps = 10,
  width = 1400,
  height = 1000,
  renderer = gifski_renderer("nfl_season_evolution_2025.gif")
)

# Alternative: Create static frame showing final standings
final_week <- team_evolution %>%
  group_by(posteam) %>%
  filter(week == max(week)) %>%
  ungroup()

final_plot <- ggplot(final_week, 
                     aes(x = cumulative_off_epa, y = cumulative_def_epa)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.06) +
  
  annotate("text", x = 0.15, y = 0.15, label = "ELITE", 
           size = 10, color = "darkgreen", alpha = 0.2, fontface = "bold") +
  annotate("text", x = -0.15, y = 0.15, label = "DEFENSE WINS", 
           size = 9, color = "steelblue", alpha = 0.2, fontface = "bold") +
  annotate("text", x = 0.15, y = -0.15, label = "OFFENSE WINS", 
           size = 9, color = "orange3", alpha = 0.2, fontface = "bold") +
  annotate("text", x = -0.15, y = -0.15, label = "STRUGGLING", 
           size = 10, color = "darkred", alpha = 0.2, fontface = "bold") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90")
  ) +
  labs(
    x = "Offensive EPA per Play",
    y = "Defensive EPA per Play",
    title = "NFL Team Performance: 2025 Season Final Standings",
    subtitle = paste("Through Week", max(final_week$week)),
    caption = "data by nflfastR"
  )

ggsave('nfl_final_standings_2025.png', final_plot, 
       width = 14, height = 10, dpi = "retina")

print("Animation complete! Check 'nfl_season_evolution_2025.gif'")
print("Static final standings saved as 'nfl_final_standings_2025.png'")