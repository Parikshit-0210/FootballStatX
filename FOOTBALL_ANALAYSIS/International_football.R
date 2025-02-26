# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
# Read the CSV file
results <- read_csv("E:/SEM3/R Proagramming/R Project/Football/results1.csv",show_col_types = FALSE)

# Basic data exploration
summary(results)
# Display the number of unique values per column
unique_counts <- sapply(results, function(x) length(unique(x)))
print(unique_counts)


away_goals = results$away_score
# Calculate total wins for each team
team_stats <- results %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE ~ "Draw"
  )) %>%
  group_by(winner) %>%
  summarize(total_wins = n(), 
            total_goals = sum(ifelse(winner == home_team, home_score, away_score), na.rm = TRUE),
            .groups = 'drop') %>%
  filter(winner != "Draw") %>%
  arrange(desc(total_wins))

# Making a new column to denote a 'win', 'lose', or 'tie'
results <- results %>%
  mutate(result = case_when(
    home_score > away_score ~ 'win',
    away_score > home_score ~ 'lost',
    TRUE ~ 'tie'
  ))

# Display the best team based on total wins
best_team <- head(team_stats, 5)
plot0=ggplot(best_team, aes(x = reorder(winner, total_wins), y = total_wins, fill = winner)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Top 5 Teams Based on Total Wins", x = "Team", y = "Total Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = total_wins), vjust = -0.5) +
  scale_fill_brewer(palette = "Set3")
print(plot0)
print(best_team)
# Calculate total goals scored by each team
total_goals <- results %>%
  mutate(home_goals = home_score, away_goals = away_score) %>%
  group_by(home_team) %>%
  summarize(total_home_goals = sum(home_goals)) %>%
  left_join(
    results %>% 
      group_by(away_team) %>%
      summarize(total_away_goals = sum(away_goals)),
    by = c("home_team" = "away_team")
  ) %>%
  mutate(total_goals = coalesce(total_home_goals, 0) + coalesce(total_away_goals, 0))

# Display total goals scored by each team
print(total_goals,n=309)

# Analyze home advantage
home_advantage <- results %>%
  summarize(home_wins = sum(home_score > away_score),
            away_wins = sum(away_score > home_score),
            draws = sum(home_score == away_score))

print(home_advantage)

# Identify the most active teams in friendly matches
friendly_matches <- results %>%
  filter(tournament == "Friendly") %>%
  group_by(home_team) %>%
  summarize(friendly_games = n()) %>%
  arrange(desc(friendly_games))
print(friendly_matches,n=252)
# Analyze performance in friendly matches
friendly_performance <- results %>%
  filter(tournament == "Friendly") %>%
  group_by(home_team) %>%
  summarize(
    total_friendly_games = n(),
    wins = sum(result == "win"),
    losses = sum(result == "lost"),
    ties = sum(result == "tie"),
    win_percentage = round((wins / total_friendly_games) * 100, 2),
    loss_percentage = round((losses / total_friendly_games) * 100, 2),
    tie_percentage = round((ties / total_friendly_games) * 100, 2)
  ) %>%
  arrange(desc(total_friendly_games))
friendly_performance = head(friendly_performance,20)# Display the results
print(friendly_performance, n = 20)
# Create a bar plot for wins, losses, and ties
plot9 = ggplot(friendly_performance, aes(x = reorder(home_team, total_friendly_games))) +
  geom_bar(aes(y = wins, fill = "Wins"), stat = "identity", position = "stack") +
  geom_bar(aes(y = losses, fill = "Losses"), stat = "identity", position = "stack") +
  geom_bar(aes(y = ties, fill = "Ties"), stat = "identity", position = "stack") +
  labs(
    title = "Performance of Most Active Teams in Friendly Matches",
    x = "Team",
    y = "Number of Matches",
    fill = "Outcome"
  ) +
  scale_fill_manual(values = c("Wins" = "green", "Losses" = "red", "Ties" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot9)



# Attempt to parse dates, coercing errors to NA
results$date <- as.Date(results$date, format="%Y-%m-%d", tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y"))
# Check for NA values after parsing
if (any(is.na(results$date))) {
  warning("There are NA values in the date column after parsing. Please check your data.")
}
# Extract Year, Month, Day, and Weekday
results$Year <- year(results$date)
results$month <- month(results$date)
results$day <- day(results$date)
results$weekday <- weekdays(results$date)
# Print data types and first few rows to verify changes
print(str(results))
print(head(results))
# Making a new column just for the purpose of analysis
results <- results %>%
  mutate(era = case_when(
    Year >= 1872 & Year < 1900 ~ '18s',
    Year >= 1900 & Year < 2000 ~ '19s',
    Year >= 2000 ~ '20s',
    TRUE ~ NA_character_
  ))

# Most TROPHIES won in each tournament by each team
# We will focus on the best home teams
best_teams <- results %>%
  filter(result == 'win') %>%
  group_by(era, home_team) %>%
  summarize(matches_won = n()) %>%
  arrange(desc(matches_won))
# Get the top 5 teams for each era
top_teams_by_era <- best_teams %>%
  group_by(era) %>%
  top_n(5, matches_won) %>%
  arrange(era, desc(matches_won))
print(top_teams_by_era,n = 20)

# Plotting using ggplot2
plot <- ggplot(best_teams, aes(x = reorder(home_team, -matches_won), y = matches_won, fill = era)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total matches won by teams in each era", x = "Home Team", y = "Matches Won") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()
# Explicitly print the plot
print(plot)



# Count matches hosted by each country where they are not participating
non_participating_hosts <- results %>%
  filter(country != home_team & country != away_team) %>%
  group_by(country) %>%
  summarize(total_matches = n(), .groups = 'drop') %>%
  arrange(desc(total_matches))
# Display the top countries hosting non-participating matches
print("Which countries host the most matches where they themselves are not participating in ")
print(non_participating_hosts)
# Finding the most matches played together
most_matches_together <- results %>%
  group_by(home_team, away_team) %>%
  summarize(matches_played_together = n()) %>%
  arrange(desc(matches_played_together))

# Display the top 10 pairs
print(head(most_matches_together, 10))

# Assuming the column for year is named 'Year', update it accordingly
country_participation <- results %>%
  group_by(Year) %>%  # Adjust if necessary
  summarise(unique_countries = n_distinct(home_team))

# Plotting
plot1<-ggplot(country_participation, aes(x = Year, y = unique_countries)) +  # Adjust if necessary
  geom_line() +
  labs(title = "Number of Unique Countries Participating Over Time",
       x = "Year", y = "Unique Countries") +
  theme_minimal()
print(plot1)

# Count matches played between teams
matchups <- results %>%
  group_by(home_team, away_team) %>%
  summarise(matches_played = n(), .groups = 'drop') %>%
  arrange(desc(matches_played))
# Top matchups
top_matchups <- head(matchups, 10)
# Plotting top matchups
plot2<-ggplot(top_matchups, aes(x = reorder(home_team, -matches_played), y = matches_played, fill = away_team)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Team Matchups",
       x = "Home Team", y = "Matches Played") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(plot2)


# Count matches hosted by country
matches_hosted <- results %>%
  group_by(home_team) %>%
  summarise(matches_hosted = n(), .groups = 'drop') %>%
  arrange(desc(matches_hosted))
print(head(matches_hosted,20))
# Plotting geographical distribution
plot3<-ggplot(head(non_participating_hosts, 20), aes(x = reorder(country, non_participating_hosts), y = total_matches)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Top 20 Countries by Matches Hosted",
       x = "Country", y = "Number of Matches Hosted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(plot3)


# Group by year and count the number of matches played each year
matches_per_year <- results %>%
  group_by(Year) %>%
  summarize(matches = n())
# Adjust plot size
options(repr.plot.width = 20, repr.plot.height = 5)
# Line plot using ggplot2
plot5 = ggplot(data = matches_per_year, aes(x = Year, y = matches)) +
  geom_line(color = 'blue', size = 1) +
  labs(
    title = "Number of matches played per year",
    x = "Year",
    y = "Number of matches"
  ) +
  scale_x_continuous(breaks = seq(min(matches_per_year$Year), max(matches_per_year$Year), by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(panel.grid.major = element_line(color = "gray", size = 0.5)) +
  geom_point()
print(plot5)


# Create a list of unique teams from both home and away teams
teams <- unique(c(results$home_team, results$away_team))
# List initialization for storing team match data
team_matches <- list()
# Add records for each team
for (team in teams) {
  home_matches <- sum(results$home_team == team, na.rm = TRUE)
  away_matches <- sum(results$away_team == team, na.rm = TRUE)
  total_matches <- home_matches + away_matches
  team_matches <- append(team_matches, list(data.frame(team = team, matches_played = total_matches)))
}
# Combine all the team match data into a single data frame
team_matches_df <- bind_rows(team_matches)
# Sort the data frame by the number of matches played in descending order
team_matches_df <- team_matches_df %>%
  arrange(desc(matches_played)) %>%
  head(10) # Select the top 10 teams
# Adjust plot size and create the barplot
plot6 = ggplot(data = team_matches_df, aes(x = matches_played, y = reorder(team, matches_played))) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = matches_played), hjust = -0.2) + # Add labels to bars
  labs(
    title = "Matches played by country",
    subtitle = "Top 10 countries by number of matches played",
    x = "Total matches played",
    y = "Team/Country"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10)) +
  coord_flip() # Flip the coordinates for better visualization
print(plot6)



# Define the list of major tournaments
major_tournaments_list <- c('FIFA World Cup', 'UEFA Euro', 'Copa América', 'African Cup of Nations', 'Gold Cup', 'AFC Asian Cup')
# Filter the dataset to include only the specified major tournaments
major_tournaments <- results %>%
  filter(tournament %in% major_tournaments_list)
# Identify host countries (assuming host country is known for each match in 'country')
major_tournaments <- major_tournaments %>%
  mutate(is_host = ifelse(home_team == country, TRUE, FALSE))
# Calculate performance metrics
mt_performance_metrics <- major_tournaments %>%
  group_by(is_host, home_team) %>%
  summarize(
    home_score = sum(home_score, na.rm = TRUE),
    away_score = sum(away_score, na.rm = TRUE)
  ) %>%
  mutate(
    win_rate = ifelse(home_score > away_score, 1, 0),
    draw_rate = ifelse(home_score == away_score, 1, 0),
    loss_rate = ifelse(home_score < away_score, 1, 0))
# Create a summary for plotting
mt_summary <- mt_performance_metrics %>%
  group_by(is_host) %>%
  summarize(
    win_rate = mean(win_rate),
    draw_rate = mean(draw_rate),
    loss_rate = mean(loss_rate))
# Map 'is_host' boolean to more descriptive labels
mt_summary$is_host <- ifelse(mt_summary$is_host, "Host", "Non-Host")
# Melt the DataFrame to long format for easier plotting
mt_summary_melted <- mt_summary %>%
  pivot_longer(cols = c(win_rate, draw_rate, loss_rate), names_to = "Metric", values_to = "Rate")
# Plotting with host appearing first
plot7 = ggplot(mt_summary_melted, aes(x = Metric, y = Rate, fill = is_host)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Host" = "blue", "Non-Host" = "gray")) +
  labs(
    title = "Performance of host vs non-host countries in major tournaments",
    x = "Metric",
    y = "Average Rate",
    fill = "Host Status"
  ) + theme_minimal() + theme(plot.title = element_text(face = "bold"))
print(plot7)


# Get the top 10 tournaments by the number of matches played
top_10_tournament_counts <- results %>%
  count(tournament) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

# Filter the dataset to include only the top 10 tournaments
top_10_tournaments <- results %>%
  filter(tournament %in% top_10_tournament_counts$tournament)

# Merge in the match counts for each tournament and sort by the number of matches played
top_10_tournaments <- top_10_tournaments %>%
  mutate(matches_played = tournament %>% factor(levels = top_10_tournament_counts$tournament)) %>%
  arrange(desc(matches_played))

# Adjust plot size and create the barplot
plot8 = ggplot(data = top_10_tournaments, aes(x = matches_played)) +
  geom_bar(fill = "gold") +
  labs(
    title = "Matches played by tournament",
    x = "Total matches played",
    y = "Tournament"
  ) +
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.2) +
  theme_minimal() +
  coord_flip()  # Flip coordinates for better visualization
print(plot8)