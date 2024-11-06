# Erforderliche Pakete laden
library(ggplot2)
library(tidyr)

# Beispiel-Daten erstellen
data <- data.frame(
  Algorithm = c("Logistic Regression", "Random Forest", "XGBoost"),
  AUC = c(0.81, 0.88, 0.89),
  FNR = c(0.22, 0.28, 0.27),
  FPR = c(0.36, 0.13, 0.134),
  AUC_SD = c(0.03, 0.2, 0.33),
  FNR_SD = c(0.02, 0.13, 0.07),
  FPR_SD = c(0.015, 0.09, 0.09)
)

# Daten in Long-Format umwandeln
data_long <- tidyr::pivot_longer(data, 
                                 cols = c("AUC", "FNR", "FPR"), 
                                 names_to = "Metric", 
                                 values_to = "Mean")

# Standardabweichungen entsprechend der Metrik zuordnen
data_long$SD <- c(data$AUC_SD, data$FNR_SD, data$FPR_SD)

# Barplot mit Fehlerbalken
ggplot(data_long, aes(x = Algorithm, y = Mean, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.2, 
                position = position_dodge(0.7)) +
  labs(title = "Mean Values of AUC, FNR, FPR by Algorithm Choice",
       x = "Algorithm",
       y = "Mean Change") +
  theme_minimal() +
  scale_fill_manual(values = c("AUC" = "darkgrey",  "FNR" = "darkcyan", "FPR" = "darkred"),
                    labels = c("AUC", "FNR", "FPR")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

#############


data <- data.frame(
  Metric = c("AUC", "FNR", "FPR"),
  Mean = c(0.046, 0.12, 0.278),
  SD = c(0.025, 0.015, 0.3)
)

# Barplot mit Fehlerbalken
ggplot(data, aes(x = Metric, y = Mean, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.2, 
                position = position_dodge(0.7)) +
  labs(title = "Mean Values with SD for AUC, FNR, FPR",
       x = "Metric",
       y = "Mean Change") +
  theme_minimal() +
  scale_fill_manual(values = c("AUC" = "darkgrey",  "FNR" = "darkcyan", "FPR" = "darkred"),
                    labels = c("AUC", "FNR", "FPR")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

