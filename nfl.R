# List of packages to check and install if needed
packages <- c("tidyverse", "nflfastR", "ggimage", "gt", "gtExtras", "ggrepel", "nflreadr", "nflplotR", "dplyr")

# Check if packages are already installed
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install packages if they are not already installed
if (length(to_install) > 0) {
  install.packages(to_install)
}

# Load Libraries 
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(dplyr)

#Load data from the 2020 through the 2023 seasons

pbp <- load_pbp(2020:2023)

#Filter by play type of rush, pass, special teams
#Rush
pbp_rush <- pbp |>
  filter(rush == 1) |>
  filter(!is.na(epa))

#Pass
pbp_pass <- pbp |>
  filter(pass == 1) |>
  filter(!is.na(epa))

#Special Teams
pbp_steams <- pbp |>
  filter(special_teams_play == 1) |>
  filter(!is.na(epa))

#sort into rushing by season, team and leading player

pbp_rush_summary <- pbp_rush %>%
  filter(rush == 1, !is.na(rusher_player_name)) %>%
  group_by(posteam, season, rusher_player_name) %>%
  summarize(rushes = n(), epa_rush = mean(epa)) %>%
  filter(rushes >= 10) %>%
  arrange(posteam, season, desc(epa_rush))

#Rush leaders by team, season and leading epa 

pbp_rush_leaders <- pbp_rush_summary %>%
  group_by(posteam, season) %>%
  slice_max(order_by = epa_rush, n = 1) %>%
  arrange(posteam, season)

#RUsing leaders by EPA
library(ggplot2)

ggplot(pbp_rush_leaders, aes(x = season, y = posteam, label = rusher_player_name)) +
  geom_tile(color = "white", fill = "white") +  # Set fill and color to white
  geom_text(size = 2.5, color = "black") +
  theme_minimal() +
  labs(x = "Season", y = "Team", title = "Rushing Leaders by Team and Season") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))  # Set panel background to white

#Rushing by Team Totals 
# Assuming pbp_rush_summary is already defined as the summary of rushing data

# Split the pbp_rush_summary table by posteam
team_table_rushing <- pbp_rush_summary %>%
  group_by(posteam) %>%
  group_split()

# View the tables for each team sorted by total yards
for (team_table in team_table_rushing) {
  team_name <- unique(team_table$posteam)
  cat("Team:", team_name, "\n")
  team_table_with_totals <- team_table %>%
    mutate(yards_gained = rushes * epa_rush) %>%
    group_by(rusher_player_name) %>%
    summarize(
      total_yards = sum(yards_gained),
      total_attempts = sum(rushes),
      total_epa = sum(epa_rush)
    ) %>%
    arrange(desc(total_yards))
  print(team_table_with_totals)
  cat("\n\n")
}

#Team Summaries Basicn (Number of passes and Runs by season)

# Summarize rushing data
rush_summary <- pbp %>%
  filter(rush == 1) %>%
  group_by(posteam, season) %>%
  summarize(total_runs = n())

# Summarize passing data
pass_summary <- pbp %>%
  filter(pass == 1) %>%
  group_by(posteam, season) %>%
  summarize(total_passes = n())

# Summarize rushing data
pbp_rush_summary <- pbp_rush %>%
  group_by(posteam, season) %>%
  summarize(total_rushes = n())

# Summarize passing data
pbp_pass_summary <- pbp_pass %>%
  group_by(posteam, season) %>%
  summarize(total_passes = n())

# Combine the summaries
team_summary <- full_join(pbp_rush_summary, pbp_pass_summary, by = c("posteam", "season"))

# Fill NA values with 0
team_summary[is.na(team_summary)] <- 0

#Team Summariesw basic
# Order the data for the pass plays table
team_summary_pass <- team_summary_with_names %>%
  arrange(desc(total_passes))

# Create a table for pass plays
pass_table <- gt(team_summary_pass) %>%
  tab_header(title = "Number of Passes by Team (Ordered by Passes)") %>%
  cols_label(team_name = "Team", total_passes = "Number of Pass Plays") %>%
  fmt_number(columns = c("total_passes"), decimals = 0)

# Order the data for the rush plays table
team_summary_rush <- team_summary_with_names %>%
  arrange(desc(total_rushes))

# Create a table for rush plays
rush_table <- gt(team_summary_rush) %>%
  tab_header(title = "Number of Rushes by Team (Ordered by Rushes)") %>%
  cols_label(team_name = "Team", total_rushes = "Number of Rush Plays") %>%
  fmt_number(columns = c("total_rushes"), decimals = 0)

# Print the tables
pass_table
rush_table

# Total Rush and pass plays by team and Season
library(dplyr)
library(gt)

# Create a table for the summary
summary_table_by_team <- team_summary_with_names %>%
  gt() %>%
  tab_header(title = "Total Rush and Pass Plays by Team and Season") %>%
  cols_label(team_name = "Team",
             season = "Season",
             total_rushes = "Total Rush Plays",
             total_passes = "Total Pass Plays") %>%
  fmt_number(columns = c("total_rushes", "total_passes"), decimals = 0)

# Print the summary table
summary_table_by_team

