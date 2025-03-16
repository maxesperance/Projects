
# Install and load necessary libraries
install.packages("Lahman")
install.packages("tidyverse")

library(Lahman)
library(tidyverse)

# Load key datasets from Lahman
data("Batting")  # Player batting statistics
data("Pitching") # Player pitching statistics
data("Teams")    # Team statistics 

# --- Data Exploration and Preprocessing ---

# 1. Batting Data: Filter and select key columns
batting <- Batting %>%
  filter(!is.na(AB), !is.na(H), !is.na(HR)) %>%  # Remove rows with missing at-bats, hits, or home runs
  select(playerID, yearID, teamID, AB, H, HR, RBI)  # At-bats, hits, home runs, runs batted in

# Summary statistics for batting
summary(batting)


# 2. Pitching Data: Filter and select key columns
pitching <- Pitching %>%
  filter(!is.na(W), !is.na(L), !is.na(ERA)) %>%  # Remove rows with missing wins, losses, or ERA
  select(playerID, yearID, teamID, W, L, ERA, SO)  # Wins, losses, earned run average, strikeouts

# Summary statistics for pitching
summary(pitching)

# 3. Teams Data: Filter for modern era (post-2000) and select key columns
teams <- Teams %>%
  filter(yearID >= 2000) %>%
  select(yearID, teamID, name, W, L, R, RA)  # Wins, losses, runs scored, runs allowed

# Summary statistics for teams
summary(teams)

# --- Visualizations ---

# 1. Batting: Average Home Runs per Year
batting_by_year <- batting %>%
  group_by(yearID) %>%
  summarize(avg_hr = mean(HR, na.rm = TRUE))

ggplot(batting_by_year, aes(x = yearID, y = avg_hr)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Home Runs per Year", x = "Year", y = "Average Home Runs") +
  theme_minimal()

# 2. Pitching: Average ERA by Year
pitching_by_year <- pitching %>%
  group_by(yearID) %>%
  summarize(avg_era = mean(ERA, na.rm = TRUE))

ggplot(pitching_by_year, aes(x = yearID, y = avg_era)) +
  geom_line(color = "red") +
  labs(title = "Average ERA per Year", x = "Year", y = "Average ERA") +
  theme_minimal()

# 3. Teams: Wins vs. Runs Scored (post-2000)
ggplot(teams, aes(x = R, y = W)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Team Wins vs. Runs Scored (2000-)", x = "Runs Scored", y = "Wins") +
  theme_minimal()

# 4. Boxplot: Home Runs by Team (Top 10 Teams by HR Total)
top_teams_hr <- batting %>%
  group_by(teamID) %>%
  summarize(total_hr = sum(HR)) %>%
  top_n(10, total_hr)

batting_top <- batting %>%
  filter(teamID %in% top_teams_hr$teamID)

ggplot(batting_top, aes(x = teamID, y = HR)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Home Runs Distribution by Top 10 HR Teams", x = "Team", y = "Home Runs") +
  theme_minimal()

# 5. Scatter Plot: Wins vs. Losses by Team (post-2000)
ggplot(teams, aes(x = W, y = L)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Team Wins vs. Losses (2000-)", x = "Wins", y = "Losses") +
  theme_minimal()

# --- Additional Insights ---
# Join Batting and Teams for a specific year (e.g., 2020)
batting_teams_2020 <- batting %>%
  filter(yearID == 2020) %>%
  left_join(teams %>% filter(yearID == 2020), by = "teamID")

# Check correlation between hits (H) and runs scored (R)
cor(batting_teams_2020$H, batting_teams_2020$R, use = "complete.obs")




