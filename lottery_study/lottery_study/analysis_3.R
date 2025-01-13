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
  expend_total ~ self_control + PCA_Lottery_Understanding + PCA_Lottery_Interest + income +
    black + hispanic + white + 
    gender + marital + 
    urban + employment + 
    religion + education + 
    ideology + state + 
    not_working_age + early_working_age + 
    late_working_age + retired,
  data = data
)

# Define the third model with expenditures_share_income as the dependent variable
model_full_share <- lm(
  expenditures_share_income ~ self_control + PCA_Lottery_Understanding + PCA_Lottery_Interest + income +
    black + hispanic + white + 
    gender + marital + 
    urban + employment + 
    religion + education + 
    ideology + state + 
    not_working_age + early_working_age + 
    late_working_age + retired,
  data = data
)

# Define the third model with additional variables
model_full_alt <- lm(
  expend_total ~ self_control + financial_literacy + financial_numeracy + gamblers_fallacy + non_belief_lln + ev_miscalculation +
    risk_seeking + risk_aversion + seems_fun + enjoy_thinking + overconfidence + happiness + income +
    black + hispanic + white + 
    gender + marital + 
    urban + employment + 
    religion + education + 
    ideology + state + 
    not_working_age + early_working_age + 
    late_working_age + retired,
  data = data
)

# Define the fourth model with expenditures_share_income as the dependent variable and additional variables
model_full_share_alt <- lm(
  expenditures_share_income ~ self_control + financial_literacy + financial_numeracy + gamblers_fallacy + non_belief_lln + ev_miscalculation +
    risk_seeking + risk_aversion + seems_fun + enjoy_thinking + overconfidence + happiness + income +
    black + hispanic + white + 
    gender + marital + 
    urban + employment + 
    religion + education + 
    ideology + state + 
    not_working_age + early_working_age + 
    late_working_age + retired,
  data = data
)

# Create HTML output using stargazer
stargazer(model_full, model_full_share, model_full_alt, model_full_share_alt, type = "html", 
          title = "Regression Results", 
          out = "C:/Users/clint/Desktop/Coding Task Darthmouth/lottery_study/lottery_regression_results.html",
          align = TRUE, 
          no.space = TRUE,
          omit = c("state"),  # Omit only the state variable
          add.lines = list(
            c("State controls", "Yes", "Yes", "Yes", "Yes")
          )
)