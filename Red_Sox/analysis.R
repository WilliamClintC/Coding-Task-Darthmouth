# Load necessary libraries
library(readr)
library(stargazer)
library(rstudioapi)

# Set the working directory to the location of the current R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the dataset
df <- read_csv("C:/Users/clint/Desktop/Coding Task Darthmouth/Red_Sox/compiled_df.csv")

# Convert day_game, weekend_game, sectiontype, gamemonth, and team to factors if they are not already
df$day_game <- as.factor(df$day_game)
df$weekend_game <- as.factor(df$weekend_game)
df$sectiontype <- as.factor(df$sectiontype)
df$gamemonth <- as.factor(df$gamemonth)
df$team <- as.factor(df$team)

# Define the features and target variable, removing duplicates
features <- c('days_from_transaction_until_game', 'day_game', 'weekend_game', 'sectiontype', 'number_of_tickets', 'gamemonth', 'team')
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

# Display the summary of both models using stargazer and save to an HTML file
stargazer(model_with_tickets, model_without_tickets,
          type = "html",
          out = "regression_results.html",
          omit = c("day_game", "weekend_game", "sectiontype", "gamemonth", "team"),
          add.lines = list(c("Day Game Controls", "Yes", "Yes"),
                           c("Weekend Game Controls", "Yes", "Yes"),
                           c("Section Controls", "Yes", "Yes"),
                           c("Game Month Controls", "Yes", "Yes"),
                           c("Team Controls", "Yes", "Yes"),
                           c("Number of Tickets Controls", "Yes", "No"))
)