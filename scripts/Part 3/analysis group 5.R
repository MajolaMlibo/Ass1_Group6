
library(readxl)
data <- read_excel("C:/Users/Fhulufhelo/Downloads/Group5_observational.xlsx")


# Ensure Rainfall is numeric
data$Rainfall <- as.numeric(data$Rainfall)


# Multi-predictor model (all predictors, no interactions)
naive_mdel <- lm(AQI ~ Traffic + Factories + Rainfall  + Season + Treatment, data = data)
summary(naive_mdel)

# Refined model (all predictors + interactions)
refined_model <- lm(AQI ~ Traffic + Factories + Rainfall + Season + Treatment +
                      Traffic*Season + Factories*Rainfall, data = data)
summary(refined_model)



