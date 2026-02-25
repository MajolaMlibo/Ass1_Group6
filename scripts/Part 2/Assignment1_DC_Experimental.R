rm(list= ls())
# --- 1. Setup for Part II ---
set.seed(2026)
n_per_cell <- 40  # Ensuring ~40 students per specific combination

# --- 2. Define the Levels (Categorical Factors) ---
levels_funding <- c(0, 1)                      # 0 = Private, 1 = NSFAS/Bursary
levels_living  <- c(1, 2, 3)                   # 1=Res, 2=Off-campus, 3=Private
levels_outings <- c("Low", "High")             # Categorized for balance

# --- 3. Create the Balanced Experimental Grid ---
# This ensures every combination (e.g., Funded student in Res with Low Outings) exists
exp_grid <- expand.grid(
  funding_source = levels_funding,
  living_arrangement = levels_living,
  social_outings_cat = levels_outings
)

# Repeat the grid to reach a large, balanced sample size
student_data_exp <- exp_grid[rep(1:nrow(exp_grid), each = n_per_cell), ]

# --- 4. Re-Simulating the "True Model" Expenditures ---
# We use your hidden weights from Part 1 to see if they "Recover" better here
n_total <- nrow(student_data_exp)

# Assign numeric effects based on your Part 1 logic
# Living Effect: Res=0, Off-campus=300, Private=1500
living_eff <- ifelse(student_data_exp$living_arrangement == 1, 0, 
                     ifelse(student_data_exp$living_arrangement == 2, 300, 1500))

# Outings Effect: Low=400 (base), High=1600 (approx 4 outings difference)
outings_num <- ifelse(student_data_exp$social_outings_cat == "High", 4, 1)
outings_eff <- 400 * outings_num

# Interaction: Funded students spend R200 MORE per outing
interact_eff <- ifelse(student_data_exp$funding_source == 1, 200 * outings_num, 0)

# Add your original Noise (Standard Deviation = 500)
error_exp <- rnorm(n_total, mean = 0, sd = 500)

# Final Expenditure Formula (True Model)
student_data_exp$monthly_expenditure_Y <- 1700 + (1200 * student_data_exp$funding_source) + 
  outings_eff + living_eff + interact_eff + error_exp

# --- 5. Verify Orthogonality ---
# If this table shows equal numbers, your variables are perfectly independent!
table(student_data_exp$funding_source, student_data_exp$living_arrangement)


## now we are running the analysis
# Convert to factors for the model
student_data_exp$living_arrangement <- as.factor(student_data_exp$living_arrangement)

# Fit the experimental model
model_exp_student <- lm(monthly_expenditure_Y ~ funding_source * social_outings_cat + living_arrangement, 
                      data = student_data_exp)
summary(model_exp_student)

# Compare diagnostics to Part 1
par(mfrow = c(2, 2))
plot(model_exp_student)