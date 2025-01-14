# Load necessary libraries
library(readr)
library(stargazer)

# Read the CSV file
data <- read_csv("C:/Users/clint/Desktop/Coding Task Darthmouth/lottery_study/lottery_study/lottery_study_updated.csv")

# Check if the dataset is loaded correctly
if (nrow(data) == 0) {
  stop("The dataset is empty. Please check the file path and ensure the CSV file is not empty.")
}

# Convert categorical variables to factors if they are not already
data$black <- as.factor(data$black)
data$hispanic <- as.factor(data$hispanic)
data$white <- as.factor(data$white)
data$gender <- as.factor(data$gender)
data$urban <- as.factor(data$urban)
data$employment <- as.factor(data$employment)
data$religion <- as.factor(data$religion)
data$state <- as.factor(data$state)
data$marital <- as.factor(data$marital)  # Convert marital status to factor

# Check the levels of each factor variable
factor_vars <- c("black", "hispanic", "white", "gender", "urban", "employment", "religion", "state", "marital")

for (var in factor_vars) {
  levels_var <- levels(data[[var]])
  if (length(levels_var) < 2) {
    cat("Variable", var, "has fewer than 2 levels:", levels_var, "\n")
  }
}

# Define the regression model without problematic variables
model_full <- lm(
  expend_total ~ income +
    black + hispanic + white + 
    gender + marital + 
    urban + employment + 
    religion + education + 
    ideology + state + 
    not_working_age + early_working_age + 
    late_working_age + retired,
  data = data
)

# Define the simple regression model with only income and expenditure
model_simple <- lm(expend_total ~ income, data = data)

# Define the third model with expenditures_share_income as the dependent variable
model_full_share <- lm(
  expenditures_share_income ~ income +
    black + hispanic + white + 
    gender + marital + 
    urban + employment + 
    religion + education + 
    ideology + state + 
    not_working_age + early_working_age + 
    late_working_age + retired,
  data = data
)

# Define the fourth model with only income and expenditures_share_income
model_simple_share <- lm(expenditures_share_income ~ income, data = data)

# Create HTML output using stargazer
stargazer(model_full, model_simple, model_full_share, model_simple_share, type = "html", 
          title = "Regression Results", 
          out = "C:/Users/clint/Desktop/Coding Task Darthmouth/lottery_study/lottery_regression_results.html",
          align = TRUE, 
          no.space = TRUE,
          omit = "state",  # Omit state controls for the full models
          add.lines = list(c("State controls", "Yes", "No", "Yes", "No"))  # Indicate state controls
)