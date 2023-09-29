# Soccer-Game-Predictions-using-Binomial-Poisson
Soccer Game Predictions using Binomial Poisson
Dataset
Your data.csv should contain:

home_team: Name of the home team.
away_team: Name of the away team.
homeGoalCount: Goals scored by the home team.
awayGoalCount: Goals scored by the away team.
date: Match date in the format "%d/%m/%Y %H:%M".
How to Use
Clone the repository and navigate to the directory.
Set the working directory to where your data file is located.
Run the script.
Key Functions Overview
DCweights(): Computes weights for matches based on date differences, giving lower weights to older matches.
rankProbScore(): Calculates the RPS based on observed outcomes and predicted probabilities.
calculate_joint_probabilities(): Calculates joint probabilities for match outcomes.
Output
A modified dataframe that includes:
RPS for each match
Joint probabilities for home wins, draws, and away wins
Lambda values for home and away teams
Finally, the script will output the mean RPS across all matches.
Error Handling
The script handles potential errors in data processing and model fitting, providing warnings for unexpected outcomes and insufficient historical data for specific teams.

Contributions & Issues
For contributions or to report any issues, please raise a pull request or create an issue on GitHub.
