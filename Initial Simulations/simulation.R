# Load necessary libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of cases
n <- 1000

# Generate sensitive variables
gender <- sample(c("Male", "Female"), n, replace = TRUE)
ethnicity <- sample(c("Majority", "Minority"), n, replace = TRUE, prob = c(0.7, 0.3))
disability <- sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.15, 0.85))

# Generate 10 non-sensitive variables with added noise
non_sensitive_vars <- matrix(rnorm(n * 10), nrow = n, ncol = 10)
non_sensitive_noise <- matrix(rnorm(n * 10, mean = 0, sd = 0.5), nrow = n, ncol = 10)
non_sensitive_vars <- non_sensitive_vars + non_sensitive_noise

# Create effect of sensitive variables on proxy variables
gender_effect <- ifelse(gender == "Female", -1, 0)
ethnicity_effect <- ifelse(ethnicity == "Minority", -1, 0)
disability_effect <- ifelse(disability == "Yes", -1, 0)

# Generate 5 proxy variables with added noise
proxy_vars <- matrix(rnorm(n * 5), nrow = n, ncol = 5)
proxy_noise <- matrix(rnorm(n * 5, mean = 0, sd = 0.5), nrow = n, ncol = 5)
proxy_vars <- sweep(proxy_vars, 1, gender_effect + ethnicity_effect + disability_effect, FUN = "+") + proxy_noise

# Combine all variables into a dataframe
data <- data.frame(
  gender = gender,
  ethnicity = ethnicity,
  disability = disability,
  non_sensitive_vars,
  proxy_vars
)

# Create column names for non-sensitive and proxy variables
colnames(data)[4:13] <- paste0("NS_", 1:10)
colnames(data)[14:18] <- paste0("Proxy_", 1:5)

# Generate the outcome variable
# All variables are positively related to the outcome
outcome_prob <- 1 / (1 + exp(-(0.5 + rowSums(data[, 4:18]))))
outcome <- rbinom(n, 1, outcome_prob)

# Add the outcome variable to the dataframe
data$outcome <- outcome

# View the first few rows of the dataframe
head(data)

####### TRY MODELS #######
train <- sample(1:1000, 800)
data_train <- data[train,]
data_test <- data[-train,]

# Fit logistic regression model without sensitive attributes
model <- glm(outcome ~ ., data = data_train %>% select(-gender, -ethnicity, -disability), family = binomial)
predicted_outcome <- predict(model, data_test, type = "response")
test_outcome <- ifelse(outcome > 0.5, 1, 0)
summary(model)

# Model performance
data$test_outcome <- test_outcome
confusion_matrix_g <- table(data$outcome, data$test_outcome)
(confusion_matrix_g[1,1] + confusion_matrix_g[2,2]) / n # accuracy
confusion_matrix_g[2,1] / n # p false negative
confusion_matrix_g[1,2] / n # p false positive

# Accuracy for subgroups
female_data = data[gender == "Female",]
confusion_matrix_female <- table(female_data$outcome, female_data$test_outcome)
(confusion_matrix_female[1,1] + confusion_matrix_female[2,2]) / n # accuracy
confusion_matrix_female[2,1] / n # p false negative
confusion_matrix_female[1,2] / n # p false positive

minority_data = data[ethnicity == "Minority",]
confusion_matrix_minority <- table(minority_data$outcome, minority_data$test_outcome)
(confusion_matrix_minority[1,1] + confusion_matrix_minority[2,2]) / n # accuracy
confusion_matrix_minority[2,1] / n # p false negative
confusion_matrix_minority[1,2] / n # p false positive

disability_data = data[disability == "Yes",]
confusion_matrix_disability <- table(disability_data$outcome, disability_data$test_outcome)
(confusion_matrix_disability[1,1] + confusion_matrix_disability[2,2]) / n # accuracy
confusion_matrix_disability[2,1] / n # p false negative
confusion_matrix_disability[1,2] / n # p false positive

female_minority_data <- data[gender == "Female" & ethnicity == "Minority",]
confusion_matrix_female_minority <- table(female_minority_data$outcome, female_minority_data$test_outcome)
(confusion_matrix_female_minority[1,1] + confusion_matrix_female_minority[2,2]) / n # accuracy
confusion_matrix_female_minority[2,1] / n # p false negative
confusion_matrix_female_minority[1,2] / n # p false positive

female_disabled <- data[gender == "Female" & disability == "Yes",]
confusion_matrix_female_disabled <- table(female_disabled$outcome, female_disabled$test_outcome)
(confusion_matrix_female_disabled[1,1] + confusion_matrix_female_disabled[2,2]) / n # accuracy
confusion_matrix_female_disabled[2,1] / n # p false negative
confusion_matrix_female_disabled[1,2] / n # p false positive