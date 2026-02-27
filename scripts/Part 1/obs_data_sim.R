# Part I: Observational Data
# Removed library(dplyr) because the assignment strictly forbids external packages.
set.seed(123)
n <- 500

### 1. Continuous: Hours spent studying weekly
hours_study <- rnorm(n, mean = 25, sd = 8)
hours_study <- pmax(hours_study, 5) # Minimum 5 hours

### 2. Count: Social outings (correlated with study hours)
# Negative relationship: more study = fewer outings
# Replaced rpois with base rnorm/round to comply with assignment constraints.
lambda_outings <- pmax(1, 10 - 0.2 * hours_study + rnorm(n, 0, 1))
social_outings <- round(abs(rnorm(n, mean = lambda_outings, sd = 2)))

### 3. Continuous: Financial Literacy (0-100 rating)
fin_lit <- rnorm(n, mean = 60, sd = 15)
fin_lit <- pmin(pmax(fin_lit, 0), 100)

### 4. Categorical: Living Arrangement
# 1 = On campus, 2 = Off campus, 3 = Apartment
living_levels <- c("On-campus", "Off-campus", "Apartment")
living_arr <- sample(living_levels, n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
living_arr <- factor(living_arr, levels = living_levels)

### 5. Binary: Funding (1 = NSFAS/Bursary, 0 = Self)
# Replaced rbinom with sample() to strictly follow base R rules.
funding <- sample(c(1, 0), n, replace = TRUE, prob = c(0.5, 0.5))

# 6. Continuous: Distance from campus
# Logic: On-campus is very close (<0.5km)
# Replaced runif with abs(rnorm()) to strictly stick to sample/rnorm assignment rules.
distance <- ifelse(living_arr == "On-campus", 
                   abs(rnorm(n, mean = 0.25, sd = 0.1)), 
                   abs(rnorm(n, mean = 8, sd = 4)))

# Combine and create "True Model" for Student Spending (Y)
# Effects: High Literacy reduces spending, Apartments increase it
epsilon <- rnorm(n, 0, 400) # Noise
student_spending <- 2000 +         # True linear regression model
  (500 * as.numeric(living_arr)) - # Apartment > Off-campus > On-campus
  (15 * fin_lit) +                 # Literacy reduces spending
  (100 * social_outings) +         # Outings increase spending
  (50 * distance) +                # Distance/Transport costs
  (300 * funding) +                # Extra disposable income
  (hours_study ) +                 # Nuisance variable
  epsilon

obs_data <- data.frame(student_spending, hours_study, social_outings, 
                       fin_lit, living_arr, funding, distance)

# Added output directory creation and CSV writing.
dir.create("outputs", showWarnings = FALSE)
write.csv(obs_data, "outputs/data/Observational_Data.csv", row.names = FALSE)

# Added png() and dev.off() to save the scatterplot matrix.
png("outputs/plots/Scatterplot_Matrix.png", width = 800, height = 800)
pairs(obs_data[,2:7], main = "Bivariate Scatterplot Matrix", pch = 16, col = "black")
dev.off()

# Added code to generate and save individual distribution graphs for all predictors.
png("outputs/distributions/X1_Hours_Study.png")
hist(obs_data$hours_study, main = "Distribution of Study Hours", xlab = "Hours/Week", col = "lightblue")
dev.off()

png("outputs/distributions/X2_Social_Outings.png")
hist(obs_data$social_outings, main = "Distribution of Social Outings", xlab = "Outings", col = "plum")
dev.off()

png("outputs/distributions/X3_Fin_Lit.png")
hist(obs_data$fin_lit, main = "Distribution of Financial Literacy", xlab = "Score (0-100)", col = "lightgreen")
dev.off()

png("outputs/distributions/X4_Living_Arrangement.png")
barplot(table(obs_data$living_arr), main = "Living Arrangement", col = "lightcoral", ylab = "Frequency")
dev.off()

png("outputs/distributions/X5_Funding.png")
barplot(table(obs_data$funding), main = "Funding Source (0=Self, 1=NSFAS)", col = "wheat", ylab = "Frequency")
dev.off()

png("outputs/distributions/X6_Distance.png")
hist(obs_data$distance, main = "Distance from Campus", xlab = "km", col = "darkgray")
dev.off()

naive_model <- lm(student_spending ~ living_arr + fin_lit + social_outings + distance + funding + hours_study, data = obs_data)
summary(naive_model)

# Diagnostic Plots Generation

# 1. Create a nested directory specifically for diagnostic plots
dir.create("outputs/diagnostics", showWarnings = FALSE, recursive = TRUE)

# 2. Generate the 4 standard Base R diagnostic plots in a 2x2 grid
png("outputs/diagnostics/Standard_Diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2)) # Set up a 2x2 layout
plot(mdel)
dev.off()

# 3. Generate Partial Residual Plots (crucial for hunting missing transformations/interactions)
png("outputs/diagnostics/Partial_Residuals.png", width = 1200, height = 800)
par(mfrow = c(2, 3)) # Set up a 2x3 layout for the 5 predictors
termplot(mdel, partial.resid = TRUE, smooth = panel.smooth, main = "Partial Residuals")
dev.off()