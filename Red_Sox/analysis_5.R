# Load necessary libraries
library(readr)
library(stargazer)
library(rstudioapi)
library(dplyr)
library(ggplot2)

# Set the working directory to the location of the current R script
current_path <- rstudioapi::getActiveDocumentContext()$path
if (length(current_path) == 1 && !is.na(current_path)) {
  setwd(dirname(current_path))
} else {
  stop("Unable to set working directory. Please ensure the script is run in RStudio.")
}

# Load the datasets for each year
df_2009 <- read_csv("C:/Users/clint/Desktop/Coding Task Darthmouth/Red_Sox/Red_Sox/red_sox_2009.csv")
df_2010 <- read_csv("C:/Users/clint/Desktop/Coding Task Darthmouth/Red_Sox/Red_Sox/red_sox_2010.csv")
df_2011 <- read_csv("C:/Users/clint/Desktop/Coding Task Darthmouth/Red_Sox/Red_Sox/red_sox_2011.csv")
df_2012 <- read_csv("C:/Users/clint/Desktop/Coding Task Darthmouth/Red_Sox/Red_Sox/red_sox_2012.csv")

# Convert day_game, weekend_game, sectiontype, gamemonth, and team to factors if they are not already
convert_factors <- function(df) {
  df$day_game <- as.factor(df$day_game)
  df$weekend_game <- as.factor(df$weekend_game)
  df$sectiontype <- as.factor(df$sectiontype)
  df$gamemonth <- as.factor(df$gamemonth)
  df$team <- as.factor(df$team)
  return(df)
}

df_2009 <- convert_factors(df_2009)
df_2010 <- convert_factors(df_2010)
df_2011 <- convert_factors(df_2011)
df_2012 <- convert_factors(df_2012)

# Create dummy variables for each range of days before the game using a loop
create_dummy_variables <- function(df) {
  ranges <- list(
    c(0, 7), c(8, 14), c(15, 21), c(22, 28), c(29, 35), 
    c(36, 60), c(61, 90), c(91, 120), c(121, 150), c(151, 180), 
    c(181, 210), c(211, 240), c(241, 250)
  )
  
  for (i in seq_along(ranges)) {
    range <- ranges[[i]]
    df[[paste0("D_", i)]] <- ifelse(df$days_from_transaction_until_game >= range[1] & df$days_from_transaction_until_game <= range[2], 1, 0)
  }
  
  return(df)
}

df_2009 <- create_dummy_variables(df_2009)
df_2010 <- create_dummy_variables(df_2010)
df_2011 <- create_dummy_variables(df_2011)
df_2012 <- create_dummy_variables(df_2012)

# Define the features and target variable, removing duplicates
features <- c(paste0("D_", 1:13), 'day_game', 'weekend_game', 'sectiontype', 'gamemonth', 'team', 'number_of_tickets')

# Function to fit the model for a specific year
fit_model_for_year <- function(df) {
  data <- df[, c(features, 'price_per_ticket')]
  model <- lm(price_per_ticket ~ ., data = data)
  return(model)
}

# Fit the models for each year
model_2009 <- fit_model_for_year(df_2009)
model_2010 <- fit_model_for_year(df_2010)
model_2011 <- fit_model_for_year(df_2011)
model_2012 <- fit_model_for_year(df_2012)

# Display the summary of the selected models using stargazer and save to an HTML file
stargazer(model_2009, model_2010, model_2011, model_2012,
          type = "html",
          out = "regression_results.html",
          omit = c("day_game", "weekend_game", "sectiontype", "gamemonth", "team"),
          add.lines = list(c("Day Game Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Weekend Game Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Section Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Game Month Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Team Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Number of Tickets Controls", "Yes", "Yes", "Yes", "Yes"))
)

# Function to extract and plot coefficients
plot_model_coefficients <- function(model, model_name) {
  # Extract coefficients from the model
  coefficients <- summary(model)$coefficients
  
  # Filter coefficients to include only those related to the dummy variables D_1, D_2, ..., D_13
  D_coefficients <- coefficients[grep("^D_", rownames(coefficients)), ]
  
  # Create a data frame with the coefficients and their corresponding dummy variable names
  D_coefficients_df <- data.frame(
    Dummy = rownames(D_coefficients),
    Coefficient = D_coefficients[, "Estimate"],
    StdError = D_coefficients[, "Std. Error"],
    Model = model_name
  )
  
  # Convert Dummy to a factor with levels ordered accordingly and then reverse the levels
  D_coefficients_df$Dummy <- factor(D_coefficients_df$Dummy, levels = rev(paste0("D_", 1:13)))
  
  return(D_coefficients_df)
}

# Extract coefficients for the models
coefficients_2009 <- plot_model_coefficients(model_2009, "2009")
coefficients_2010 <- plot_model_coefficients(model_2010, "2010")
coefficients_2011 <- plot_model_coefficients(model_2011, "2011")
coefficients_2012 <- plot_model_coefficients(model_2012, "2012")

# Combine all coefficients into one data frame
all_coefficients_df <- bind_rows(coefficients_2009, coefficients_2010, coefficients_2011, coefficients_2012)

# Plot the coefficients using ggplot2
ggplot(all_coefficients_df, aes(x = Dummy, y = Coefficient, color = Model)) +
  geom_point() +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * StdError, ymax = Coefficient + 1.96 * StdError), width = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Coefficients of Dummy Variables (Days from Transaction Until Game) on Ticket Price", x = "Dummy Variable", y = "Coefficient") +
  facet_wrap(~ Model, scales = "free_y")