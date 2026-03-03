# ==========================================
# Part I: Observational Data
# ==========================================

# Set seed for reproducibility (Updated from Markdown)
set.seed(2026) 
n <- 4860      # Updated sample size

# ==========================================
# Predictors
# ==========================================

# 1. Binary: Gender (New Variable)
# 0 = Male, 1 = Female
x_gender <- sample(c(0, 1), n, replace = TRUE) 

# 2. Binary: Funding Source
# 0 = Self, 1 = NSFAS (Updated Probability: 40/60)
x_funding <- sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6))

# 3. Categorical: Accommodation / Living Arrangement
# 1 = Res, 2 = Off-campus, 3 = Apartment
x_accommodation <- sample(c(1, 2, 3), n, replace = TRUE, prob = c(0.3, 0.5, 0.2))

# 4. Continuous: Distance from Campus (km)
# Logic: Res (1) is close (0.01-0.49km), others are further (1.5-15km)
x_distance <- ifelse(x_accommodation == 1, runif(n, 0.01, 0.49), runif(n, 1.5, 15))
x_distance <- round(x_distance, 2)

# 5. Continuous: Financial Literacy (0-100)
# Rounded and constrained between 0 and 100
x_fin_lit <- round(pmax(pmin(rnorm(n, 55, 12), 100), 0), 2)

# 6. Continuous: Study Hours (Weekly)
x_study_hrs <- round(runif(n, 9, 42) * 4, 2)

# 7. Count: Social Outings
# Logic: Literacy and Study Hours reduce outings
lambda_outings <- pmax(1, 14 - (0.05 * x_study_hrs) - (0.06 * x_fin_lit))
x_outings <- rpois(n, lambda = lambda_outings)

# ==========================================
# True Model for Student Spending (Y)
# ==========================================

# Base Parameters
beta_0 <- 2200 

# Accommodation Adjustment (Non-linear effect)
acc_adj <- ifelse(x_accommodation == 2, 850, ifelse(x_accommodation == 3, 3200, 0))

# Individual Coefficients (Betas)
beta_funding  <- 500   # NSFAS increase
beta_dist     <- 25    # Cost per KM
beta_lit      <- -12   # Savings per literacy point
beta_outings  <- 180   # Cost per social outing

# Noise (High noise for 35-50% R-squared)
epsilon <- rnorm(n, mean = 0, sd = 1650) 

# Spending Equation
y_spending <- beta_0 + 
              (beta_funding * x_funding) + 
              acc_adj + 
              (beta_dist * x_distance) + 
              (beta_outings * x_outings) + 
              (beta_lit * x_fin_lit) + 
              epsilon

# Rounding and realistic constraints (R1200 - R13000)
y_spending <- round(pmax(pmin(y_spending, 13000), 1200), 2)

# Create DataFrame
obs_data <- data.frame(Spending = y_spending, 
                       Funding = x_funding, 
                       Accommodation = x_accommodation, 
                       Distance = x_distance, 
                       FinLit = x_fin_lit, 
                       StudyHrs = x_study_hrs, 
                       Outings = x_outings, 
                       Gender = x_gender)

# Save Data
dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)
write.csv(obs_data, "outputs/data/Observational_Data.csv", row.names = FALSE)

# ==========================================
# Visualization: Histograms
# ==========================================

png("outputs/plots/Observational_Histograms.png", width = 1000, height = 1000)
# Set up a grid of 3 rows and 3 columns
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

hist(obs_data$Spending, col = "skyblue", main = "Spending (Y)", xlab = "Rands")
hist(obs_data$Funding, col = "lightgreen", main = "Funding Status", xlab = "0 = No, 1 = Yes")
hist(obs_data$Accommodation, col = "salmon", main = "Accommodation Type", xlab = "1=Res, 2=Off, 3=Apt")
hist(obs_data$Distance, col = "gold", main = "Distance to Campus", xlab = "Kilometers")
hist(obs_data$FinLit, col = "plum", main = "Fin. Literacy Score", xlab = "Score (0-100)")
hist(obs_data$StudyHrs, col = "lightblue", main = "Study Hours", xlab = "Weekly Hours")
hist(obs_data$Outings, col = "orange", main = "Social Outings", xlab = "Number of Outings")
hist(obs_data$Gender, col = "grey", main = "Gender", xlab = "0 = Male, 1 = Female")

par(mfrow = c(1, 1)) # Reset
dev.off()

# ==========================================
# Visualization: Scatterplot Matrix
# ==========================================

png("outputs/plots/Scatterplot_Matrix.png", width = 800, height = 800)
# Select key variables
plot_data <- obs_data[, c("Spending", "Distance", "FinLit", "StudyHrs", "Outings")]
pairs(plot_data, 
      main = "Scatter Plot Matrix: Student Spending Drivers",
      pch = 16, 
      cex = 0.5, 
      col = rgb(0.2, 0.4, 0.6, 0.1), # Transparent blue
      lower.panel = panel.smooth)
dev.off()

# ==========================================
# Analysis: Naive Model
# ==========================================

model_obs <- lm(Spending ~ Funding + factor(Accommodation) + Distance + 
                FinLit + StudyHrs + Outings + Gender, data = obs_data)

# Print Summary
summary(model_obs)
cat("Observational R-Squared:", round(summary(model_obs)$r.squared, 2))

# ==========================================
# Diagnostic Plots
# ==========================================

dir.create("outputs/diagnostics", showWarnings = FALSE, recursive = TRUE)

# Standard Diagnostic Plots
png("outputs/diagnostics/Standard_Diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(model_obs)
dev.off()

# Partial Residual Plots
png("outputs/diagnostics/Partial_Residuals.png", width = 1200, height = 800)
par(mfrow = c(2, 4)) # Adjusted for more predictors
termplot(model_obs, partial.resid = TRUE, smooth = panel.smooth, main = "Partial Residuals")
dev.off()