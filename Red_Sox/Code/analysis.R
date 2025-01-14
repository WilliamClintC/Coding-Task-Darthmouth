# Load necessary libraries
library(readr)
library(stargazer)
library(rstudioapi)

# Set the working directory to the location of the current R script
current_path <- rstudioapi::getActiveDocumentContext()$path
if (length(current_path) == 1 && !is.na(current_path)) {
  setwd(dirname(current_path))
} else {
  stop("Unable to set working directory. Please ensure the script is run in RStudio.")
}

# Load the dataset
df <- read_csv("C:/Users/clint/Desktop/Coding Task Darthmouth/Red_Sox/compiled_df.csv")

# Convert day_game, weekend_game, sectiontype, gamemonth, team, and year to factors if they are not already
df$day_game <- as.factor(df$day_game)
df$weekend_game <- as.factor(df$weekend_game)
df$sectiontype <- as.factor(df$sectiontype)
df$gamemonth <- as.factor(df$gamemonth)
df$team <- as.factor(df$team)
df$year <- as.factor(df$year)

# Define the features and target variable, removing duplicates
features <- c('days_from_transaction_until_game', 'day_game', 'weekend_game', 'sectiontype', 'number_of_tickets', 'gamemonth', 'team', 'year')
X <- df[features]
y <- df$price_per_ticket

# Combine features and target variable into one dataframe
data <- df[, c(features, 'price_per_ticket')]

# Fit the regression model with number_of_tickets
model_with_tickets <- lm(price_per_ticket ~ ., data = data)

# Fit the regression model without number_of_tickets
features_without_tickets <- setdiff(features, 'number_of_tickets')
data_without_tickets <- df[, c(features_without_tickets, 'price_per_ticket')]
model_without_tickets <- lm(price_per_ticket ~ ., data = data_without_tickets)

# Combine features and log-transformed target variable into one dataframe
data_log <- df[, c(features, 'logprice')]

# Fit the regression model with number_of_tickets using log price
model_with_tickets_log <- lm(logprice ~ ., data = data_log)

# Fit the regression model without number_of_tickets using log price
data_without_tickets_log <- df[, c(features_without_tickets, 'logprice')]
model_without_tickets_log <- lm(logprice ~ ., data = data_without_tickets_log)

# Display the summary of the selected models using stargazer and save to an HTML file
stargazer(model_with_tickets, model_without_tickets, model_with_tickets_log, model_without_tickets_log,
          type = "html",
          out = "regression_results.html",
          omit = c("day_game", "weekend_game", "sectiontype", "gamemonth", "team", "year"),
          add.lines = list(c("Day Game Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Weekend Game Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Section Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Game Month Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Team Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Year Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Number of Tickets Controls", "Yes", "No", "Yes", "No"))
)