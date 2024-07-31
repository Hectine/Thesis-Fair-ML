threshold <- seq(0.05, 0.95, 0.05)
bench_FPR <- c()
bench_FNR <- c()
vulnerable_FPR <- c()
vulnerable_FNR <- c()
privileged_FPR <- c()
privileged_FNR <- c()

for(i in 1:length(threshold)){
  # convert probabilities into binary predictions based on decision threshold
  prediction <- ifelse(probs_rf > threshold[i], 1, 0)
  
  ## Benchmark ##
  # confusion matrix
  confusion_matrix_benchmark <- table(compas_test$is_recid, prediction)
  # calculate false positive rate
  FPR_bench <- tryCatch({
    confusion_matrix_benchmark[1,2] / (confusion_matrix_benchmark[1,2] + confusion_matrix_benchmark[1,1])
  }, warning = function(w) { NA }, error = function(e) { NA })
  # print result
  bench_FPR[i] <- FPR_bench
  # calculate false negative rate
  FNR_bench <- tryCatch({
    confusion_matrix_benchmark[2,1] / (confusion_matrix_benchmark[2,1] + confusion_matrix_benchmark[2,2])
  }, warning = function(w) { NA }, error = function(e) { NA })
  # print result
  bench_FNR[i] <- FNR_bench
  
  ## Young, African-American Man (VULNERABLE GROUP) ##
  # filter cases
  vulnerable_group <- compas_test[sex == "Male" & age_cat == "Less than 25" & race == "African-American", ]
  # find indices to filter predictions
  indices <- which(compas_test$sex == "Male" & compas_test$age_cat == "Less than 25" & compas_test$race == "African-American")
  # confusion matrix
  confusion_matrix_vulnerable <- table(vulnerable_group$is_recid, prediction[indices])
  # calculate false positive rate
  FPR <- tryCatch({
    confusion_matrix_vulnerable[1,2] / (confusion_matrix_vulnerable[1,2] + confusion_matrix_vulnerable[1,1])
  }, warning = function(w) { NA }, error = function(e) { NA })
  # print result
  vulnerable_FPR[i] <- FPR
  # calculate false negative rate
  FNR <- tryCatch({
    confusion_matrix_vulnerable[2,1] / (confusion_matrix_vulnerable[2,1] + confusion_matrix_vulnerable[2,2])
  }, warning = function(w) { NA }, error = function(e) { NA })
  # print result
  vulnerable_FNR[i] <- FNR
  
  ## Old, Caucasian Women (PRIVILEGED GROUP) ##
  # filter cases
  privileged_group <- compas_test[sex == "Female" & age_cat == "Greater than 45" & race == "Caucasian", ]
  # find indices to filter predictions
  indices <- which(compas_test$sex == "Female" & compas_test$age_cat == "Greater than 45" & compas_test$race == "Caucasian")
  # confusion matrix
  confusion_matrix_privileged <- table(privileged_group$is_recid, prediction[indices])
  # calculate false positive rate
  FPR <- tryCatch({
    confusion_matrix_privileged[1,2] / (confusion_matrix_privileged[1,2] + confusion_matrix_privileged[1,1])
  }, warning = function(w) { NA }, error = function(e) { NA })
  # print result
  privileged_FPR[i] <- FPR
  # calculate false negative rate
  FNR <- tryCatch({
    confusion_matrix_privileged[2,1] / (confusion_matrix_privileged[2,1] + confusion_matrix_privileged[2,2])
  }, warning = function(w) { NA }, error = function(e) { NA })
  # print result
  privileged_FNR[i] <- FNR
}

## Plot
library(ggplot2)
library(tidyr)
library(dplyr)

# Combine the vectors into a data frame
data <- data.frame(
  threshold = rep(threshold, times = 6),
  value = c(bench_FPR, bench_FNR, vulnerable_FPR, vulnerable_FNR, privileged_FPR, privileged_FNR),
  metric = rep(rep(c("FPR", "FNR"), each = length(threshold)), times = 3),
  group = rep(c("Benchmark", "Vulnerable", "Privileged"), each = length(threshold) * 2)
)

# Create the plot
ggplot(data, aes(x = threshold, y = value, color = group, linetype = metric)) +
  geom_line() +
  scale_linetype_manual(values = c("FPR" = "solid", "FNR" = "dashed")) +
  labs(title = "Boosting - FPR and FNR by Threshold for Different Groups",
       x = "Threshold",
       y = "Rate",
       color = "Group",
       linetype = "Metric") +
  theme_minimal()

## Plot 2
# Calculate the ratios of FPR and FNR to the benchmark
vulnerable_FPR_ratio <- vulnerable_FPR / bench_FPR
vulnerable_FNR_ratio <- vulnerable_FNR / bench_FNR
privileged_FPR_ratio <- privileged_FPR / bench_FPR
privileged_FNR_ratio <- privileged_FNR / bench_FNR

# Combine the vectors into a data frame
data_ratio <- data.frame(
  threshold = rep(threshold, times = 4),
  value = c(vulnerable_FPR_ratio, vulnerable_FNR_ratio, privileged_FPR_ratio, privileged_FNR_ratio),
  metric = rep(rep(c("FPR", "FNR"), each = length(threshold)), times = 2),
  group = rep(c("Vulnerable", "Privileged"), each = length(threshold) * 2)
)

# Create the plot
ggplot(data_ratio, aes(x = threshold, y = value, color = group, linetype = metric)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
  scale_linetype_manual(values = c("FPR" = "solid", "FNR" = "dashed")) +
  labs(title = "Random Forest - FPR and FNR Ratios to Benchmark by Threshold",
       x = "Threshold",
       y = "Ratio",
       color = "Group",
       linetype = "Metric") +
  theme_minimal()
