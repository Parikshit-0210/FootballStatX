# FootballStatX : International Football Results Analysis

## 📌 Project Overview

This project analyzes football match data using R to explore team performance, match statistics, and trends over time. The dataset includes match results from different teams and tournaments, enabling insights into winning patterns, goal distributions, and historical trends.

## 📂 Dataset

The dataset consists of **47,599 football matches** (from 1872 to 2024) and includes the following columns:

- **Date**: The date of the match.
- **Home Team**: The team playing at home.
- **Away Team**: The visiting team.
- **Home Score**: Goals scored by the home team.
- **Away Score**: Goals scored by the away team.
- **Tournament**: The type of tournament (e.g., Friendly, World Cup, etc.).
- **City**: The city where the match was played.
- **Country**: The country where the match took place.
- **Neutral**: Whether the match was played at a neutral venue (True/False).

## 📊 Features & Analysis

The project performs the following analyses:

### ✅ Data Exploration

- Summary statistics of the dataset
- Count of unique teams and matches played

### ✅ Team Performance Analysis

- Number of wins per team
- Goals scored by each team
- Matches categorized as friendly vs. competitive
- Performance trends over time

### ✅ Match Trends

- Distribution of home vs. away wins
- Tournament performance analysis
- Historical participation of teams in matches

### ✅ Visualizations

- Bar plots for wins, goals, and match trends
- Line graphs for performance trends
- Histograms for goal distributions

## 🛠 Technologies Used

- R
- ggplot2 (for visualizations)
- dplyr (for data manipulation)
- readr (for data loading and preprocessing)

## 🚀 How to Run the Project

1. Clone this repository:
   ```sh
   git clone https://github.com/Parikshit-0210/FootballStatx.git
   ```
2. Open RStudio or run an R script in your preferred environment.
3. Install required packages (if not installed):
   ```r
   install.packages("ggplot2")
   install.packages("dplyr")
   install.packages("readr")
   ```
4. Load and execute the script:
   ```r
   source("football_analysis.R")
   ```

## 📌 Future Enhancements

- Predictive modeling for match outcomes
- Advanced statistical analysis of goal distributions
- Interactive visualizations using Shiny

## 📜 License

This project is open-source under the MIT License.

## 🙌 Contributing

Feel free to fork this repository and submit pull requests to enhance the analysis!

## 👥 Team Members

- Dipankar TV
- Parikshit V
- Shree Nithesh R
