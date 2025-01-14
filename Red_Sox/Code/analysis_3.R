# Load necessary libraries
library(readr)
library(stargazer)
library(rstudioapi)
library(dplyr)

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

# Create dummy variables for each range of days before the game using a loop
ranges <- list(
  c(0, 7), c(8, 14), c(15, 21), c(22, 28), c(29, 35), 
  c(36, 60), c(61, 90), c(91, 120), c(121, 150), c(151, 180), 
  c(181, 210), c(211, 240), c(241, 250)
)

for (i in seq_along(ranges)) {
  range <- ranges[[i]]
  df[[paste0("D_", i)]] <- ifelse(df$days_from_transaction_until_game >= range[1] & df$days_from_transaction_until_game <= range[2], 1, 0)
}

# Define the features and target variable, removing duplicates
features <- c(paste0("D_", 1:length(ranges)), 'day_game', 'weekend_game', 'sectiontype', 'gamemonth', 'team', 'year', 'number_of_tickets')
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


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to extract and plot coefficients
plot_model_coefficients <- function(model, model_name) {
  # Extract coefficients from the model
  coefficients <- summary(model)$coefficients
  
  # Filter coefficients to include only those related to the dummy variables D_1, D_2, ..., D_31
  D_coefficients <- coefficients[grep("^D_", rownames(coefficients)), ]
  
  # Create a data frame with the coefficients and their corresponding dummy variable names
  D_coefficients_df <- data.frame(
    Dummy = rownames(D_coefficients),
    Coefficient = D_coefficients[, "Estimate"],
    StdError = D_coefficients[, "Std. Error"],
    Model = model_name
  )
  
  # Convert Dummy to a factor with levels ordered accordingly and then reverse the levels
  D_coefficients_df$Dummy <- factor(D_coefficients_df$Dummy, levels = rev(paste0("D_", 1:31)))
  
  return(D_coefficients_df)
}

# Extract coefficients for all models
coefficients_with_tickets <- plot_model_coefficients(model_with_tickets, "With Tickets")
coefficients_without_tickets <- plot_model_coefficients(model_without_tickets, "Without Tickets")
coefficients_with_tickets_log <- plot_model_coefficients(model_with_tickets_log, "With Tickets (Log Price)")
coefficients_without_tickets_log <- plot_model_coefficients(model_without_tickets_log, "Without Tickets (Log Price)")

# Combine all coefficients into one data frame
all_coefficients_df <- bind_rows(coefficients_with_tickets, coefficients_without_tickets, coefficients_with_tickets_log, coefficients_without_tickets_log)

# Plot the coefficients using ggplot2
ggplot(all_coefficients_df, aes(x = Dummy, y = Coefficient, color = Model)) +
  geom_point() +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * StdError, ymax = Coefficient + 1.96 * StdError), width = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Coefficients of Dummy Variables (Days from Transaction Until Game) on Ticket Price", x = "Dummy Variable", y = "Coefficient") +
  facet_wrap(~ Model, scales = "free_y")

