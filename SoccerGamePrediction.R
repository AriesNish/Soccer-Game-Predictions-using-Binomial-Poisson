# Set your working directory


# Load library
library(bivpois)

# Read data
df <- read.csv("data.csv")

# Handle missing values
df <- na.omit(df)

# Formula for modeling of lambda1 and lambda2
form1 <- ~c(home_team,away_team) + c(away_team,home_team)

# Initialize empty columns for RPS, joint probabilities, and lambda values
df$rps <- NA
df$joint_probs <- list()
df$home_lambda <- NA
df$away_lambda <- NA

# Define parameters
horizon_matches <- 390
min_matches <- 15

# Define the DCweights function

DCweights <- function(dates, currentDate = Sys.Date(), xi = 0) {
  cat("Attempting to convert:", dates, "\n")
  
  # Convert the dates using the specified format
  datediffs <- as.Date(dates, format="%d/%m/%Y %H:%M") - as.Date(currentDate)
  datediffs <- as.numeric(datediffs * -1)
  
  # Compute the weights
  w <- exp(-1 * xi * datediffs)
  w[datediffs <= 0] <- 0 # Future dates should have zero weights
  
  return(w)
}



# Define the rankProbScore function
rankProbScore <- function(observed, predictions) {
  ncat <- length(predictions)
  cumulative <- 0
  for (i in 1:ncat) {
    diff_square <- (sum(predictions[1:i]) - sum(observed[1:i]))^2
    cumulative <- cumulative + diff_square
  }
  rps <- (1 / (ncat - 1)) * cumulative
  return(rps)
}

calculate_joint_probabilities <- function(pred_home_win, pred_draw, pred_away_win) {
  
  joint_probabilities <- list(
    home_win = pred_home_win,
    draw = pred_draw,
    away_win = pred_away_win
  )
  return(joint_probabilities)
}

df$joint_probs <- vector("list", nrow(df))


# Loop over the data, leaving out the first 'horizon_matches' rows
for (i in (horizon_matches + 1):nrow(df)) {
  train_data <- df[(i - horizon_matches):(i - 1), ]
  home_team <- df$home_team[i]
  away_team <- df$away_team[i]
  
  home_data <- train_data[train_data$home_team == home_team, ]
  away_data <- train_data[train_data$away_team == away_team, ]
  
  if (nrow(home_data) < min_matches | nrow(away_data) < min_matches) {
    cat("Skipped match at row", i, "due to insufficient historical data for teams.\n")
    next
  }
  
  # fit the Bivariate Poisson model and handle potential errors
  tryCatch({
    m <- lm.dibp(homeGoalCount ~ 1, awayGoalCount ~ 1, l1l2 = form1, data = train_data, jmax = 1)
  }, error = function(e) {
    cat("Error fitting model for match at row", i, ". Message:", e$message, "\n")
    next
  })
  
  # Extract model parameters and compute expected goals
  beta1 <- m$beta1
  beta2 <- m$beta2
  
  # Map team names to their coefficients
  home_team_effects <- setNames(beta1[2:(length(beta1))], names(beta1)[2:(length(beta1))])
  away_team_effects <- setNames(beta2[2:(length(beta2))], names(beta2)[2:(length(beta2))])
  
  # Compute expected goals for each team
  expected_goals_home_team <- exp(beta1[1] + as.numeric(sapply(home_team, function(team) home_team_effects[team])))
  expected_goals_away_team <- exp(beta2[1] + as.numeric(sapply(away_team, function(team) away_team_effects[team])))
  
  # Calculate predicted probabilities
  
  pred_home_win <- expected_goals_home_team / (expected_goals_home_team + expected_goals_away_team)
  pred_away_win <- expected_goals_away_team / (expected_goals_home_team + expected_goals_away_team)
  pred_draw <- 1 - pred_home_win - pred_away_win
  
  # Extract observed outcomes from your dataset
  observed_home_goals <- df$homeGoalCount[i]
  observed_away_goals <- df$awayGoalCount[i]
  
  # Calculate observed_outcome with error handling
  cat("Row:", i, "Home Goals:", observed_home_goals, "Away Goals:", observed_away_goals, "\n")
  if (observed_home_goals > observed_away_goals) {
    observed_outcome <- c(1, 0, 0)  # Home Team Win
  } else if (observed_home_goals < observed_away_goals) {
    observed_outcome <- c(0, 0, 1)  # Away Team Win
  } else if (observed_home_goals == observed_away_goals) {
    observed_outcome <- c(0, 1, 0)  # Draw
  } else {
    # Handle unexpected values
    cat("Warning: Unexpected outcome in row", i, "\n")
    observed_outcome <- c(NA, NA, NA)
  }
  
  # Calculate and update joint probabilities
  
  joint_probs <- calculate_joint_probabilities(pred_home_win, pred_draw, pred_away_win)
  df$joint_probs[[i]] <- calculate_joint_probabilities(pred_home_win, pred_draw, pred_away_win)
  
  
  # Compute the RPS for match 
  pred_probs <- c(pred_home_win, pred_draw, pred_away_win)
  rps <- rankProbScore(observed_outcome, pred_probs)
  df$rps[i] <- rps
  
  # Update lambda values for each team
  lambda_home <- sum(DCweights(train_data$date)) 
  lambda_away <- sum(DCweights(train_data$date)) 
  df$home_lambda[i] <- lambda_home
  df$away_lambda[i] <- lambda_away
}

# Compute the mean RPS
mean_rps <- mean(df$rps, na.rm = TRUE)
print(mean_rps)
