########data cleaning##########
##Missing Data Handling
library(haven)
read_sav("D:/my project/WCGS.sav")
data<-read_sav("D:/my project/WCGS1.sav")

# Count number of missing values per selected variable
vars <- c("behpat","age", "sbp", "dbp", "chol", "height", "weight")
missing_counts <- sapply(data[vars], function(x) sum(is.na(x)))
data_subset <- data[, vars]

# Convert to data frame for table output
missing_df <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = missing_counts,
  Percent = round(missing_counts / nrow(data) * 100, 2)
)

# Display table
library(kableExtra)
kbl(missing_df, caption = "Missing Values per Variable",
    col.names = c("Variable", "Count", "Percent (%)")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# Remove rows with any missing values
data_clean <- na.omit(data_subset)

##Outlier Removal
# Function to detect outliers using 1.5 x IQR rule
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(x < lower_bound | x > upper_bound))
}

# List of continuous variables to check
vars <- c("sbp", "dbp", "chol", "height", "weight", "age")

# Detect and count outliers for each variable
outlier_counts <- sapply(vars, function(var) {
  idx <- detect_outliers(data_clean[[var]])
  length(idx)
})

# Show the number of outliers per variable
outlier_counts

# Function to detect outliers using 1.5 x IQR rule
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(x < lower_bound | x > upper_bound))
}

# List of continuous variables to check
vars <- c("sbp", "dbp", "chol", "height", "weight", "age")

# Detect and count outliers for each variable
outlier_counts <- sapply(vars, function(var) {
  idx <- detect_outliers(data_clean[[var]])
  length(idx)
})

# Show the number of outliers per variable
outlier_counts

# Calculate the percentage of outliers for each variable
outlier_percentages <- sapply(vars, function(var) {
  total_non_missing <- sum(!is.na(data_clean[[var]]))
  outlier_count <- length(detect_outliers(data_clean[[var]]))
  round((outlier_count / total_non_missing) * 100, 2)
})

# Display percentages
outlier_percentages

##jitter plots
library(ggplot2)
library(tidyr)
library(dplyr)

# Select relevant numeric variables
vars <- c("age", "sbp", "dbp", "chol", "height", "weight")

# Reshape data to long format for faceted plots
data_long <- data %>%
  select(behpat, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value")

# Create jitter plots for each variable across behpat categories
ggplot(data_long, aes(x = behpat, y = value)) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "steelblue") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +
  stat_summary(fun = mean, geom = "line", group = 1, color = "red") +
  labs(title = "Jitter Plots of Numeric Variables by BEHPAT Group",
       x = "Behavioral Pattern",
       y = "Value")

##############Model Building##################
##backward Selection
library(nnet)
model_full <- multinom(behpat ~ age + sbp + dbp 
                       + chol + height + weight, data = data_clean)

coefs <- summary(model_full)$coefficients
ses <- summary(model_full)$standard.errors
z_values <- coefs / ses
p_values <- 2 * (1 - pnorm(abs(z_values)))
pvals_rounded <- round(p_values, 5)  

# p-alues
library(tibble)
library(dplyr)
pval_table_only <- pvals_rounded %>%
  as.data.frame() %>%
  rownames_to_column("Category") %>%
  select(Category, age, sbp, dbp, chol, height, weight)
library(kableExtra)
kbl(pval_table_only, caption = "P-values for Predictors in Each Outcome Group",
    col.names = c("Group", "Age", "SBP", "DBP", "Chol", "Height", "Weight")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

##Forward Selection Based on AIC

library(nnet)
# Null model (intercept only)
model_null <- multinom(behpat ~ 1, data = data_clean)

model_age   <- multinom(behpat ~ age, data = data_clean)
model_height <- multinom(behpat ~ height, data = data_clean)
model_weight <- multinom(behpat ~ weight, data = data_clean)
model_sbp   <- multinom(behpat ~ sbp, data = data_clean)
model_dbp   <- multinom(behpat ~ dbp, data = data_clean)
model_chol  <- multinom(behpat ~ chol, data = data_clean)

AIC(model_null, model_age, model_height, model_weight,
    model_sbp, model_dbp, model_chol)
# Model 1: add age
model_fwd1 <- multinom(behpat ~ age, data = data_clean)

model_age_height <- multinom(behpat ~ age + height, data = data_clean)
model_age_weight <- multinom(behpat ~ age + weight, data = data_clean)
model_age_sbp    <- multinom(behpat ~ age + sbp, data = data_clean)
model_age_dbp    <- multinom(behpat ~ age + dbp, data = data_clean)
model_age_chol   <- multinom(behpat ~ age + chol, data = data_clean)

AIC(model_fwd1, model_age_height, model_age_weight, model_age_sbp,
    model_age_dbp, model_age_chol)
# Model 2: add sbp
model_fwd2 <- multinom(behpat ~ age + sbp, data = data_clean)

model_age_sbp_height <- multinom(behpat ~ age + sbp + height, data = data_clean)
model_age_sbp_weight <- multinom(behpat ~ age + sbp + weight, data = data_clean)
model_age_sbp_dbp    <- multinom(behpat ~ age + sbp + dbp,    data = data_clean)
model_age_sbp_chol   <- multinom(behpat ~ age + sbp + chol,   data = data_clean)

AIC(model_fwd2, model_age_sbp_height, model_age_sbp_weight,
    model_age_sbp_dbp, model_age_sbp_chol)
# Model 3: add chol
model_fwd3 <- multinom(behpat ~ age + sbp + chol, data = data_clean)

model_age_sbp_chol_height <- multinom(behpat ~ age + sbp + chol + height, data = data_clean)
model_age_sbp_chol_weight <- multinom(behpat ~ age + sbp + chol + weight, data = data_clean)
model_age_sbp_chol_dbp    <- multinom(behpat ~ age + sbp + chol + dbp, data = data_clean)

AIC(model_fwd3,
    model_age_sbp_chol_height,
    model_age_sbp_chol_weight,
    model_age_sbp_chol_dbp)
# Model 4: add height
model_fwd4 <- multinom(behpat ~ age + sbp + chol + height , data = data_clean)

model_age_sbp_chol_height_weight <- multinom(behpat ~ age + sbp + chol + height + weight, data = data_clean)
model_age_sbp_chol_height_dbp    <- multinom(behpat ~ age + sbp + chol + height + dbp,    data = data_clean)

AIC(model_fwd4,
    model_age_sbp_chol_height_weight,
    model_age_sbp_chol_height_dbp)
# Model 5: add dbp
model_fwd5 <- multinom(behpat ~ age + sbp + chol + height +dbp
                       , data = data_clean)

#Model 5: add weight
model_fwd6 <- multinom(behpat ~ age + sbp+ dbp + chol 
                       + height + weight, data = data_clean)

##Multicollinearity
# model_final <- multinom(behpat ~ age + sbp + dbp + chol + height + weight, data = data_clean)
library(car)

# Create a linear model with same predictors
vif_model <- lm(age ~ sbp + dbp + chol + height + weight, data = data_clean)

# Compute VIFs
vif_values <- vif(vif_model)

library(knitr)
library(kableExtra)

# Turn VIF values into data frame
vif_table <- data.frame(VIF = round(vif_values, 2))

# Create formatted table
kable(vif_table, caption = "VIF Values in the Full Model") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

################Model Comparison#############
##Model Comparison Table
# p-value table
library(knitr)

pval_table <- data.frame(
  Variable = c("age", "height", "weight", "sbp", "dbp", "chol"),
  A2 = c(1.180427e-02, 0.1115953, 0.4711732, 0.007255131, 0.01501273, 0.19427261),
  B3 = c(9.587185e-06, 0.4365072, 0.4526394, 0.313565470, 0.07777905, 0.01870619),
  B4 = c(5.633520e-05, 0.1792555, 0.2511721, 0.291681584, 0.08157319, 0.01435446)
)

kable(pval_table, digits = 4, caption = "P-values for each predictor across outcome levels")

##
# Fit different models for comparison
model_full <- multinom(behpat ~ age + sbp + dbp + chol + height + weight, data = data_clean)
model_no_weight <- multinom(behpat ~ age + sbp + dbp + chol + height, data = data_clean)
model_no_height <- multinom(behpat ~ age + sbp + dbp + chol + weight, data = data_clean)
model_no_both <- multinom(behpat ~ age + sbp + dbp + chol, data = data_clean)

# Final selected model (including both weight and height)
model_final <- model_full

# Predict outcomes using each model
pred_full <- predict(model_full)
pred_nowt <- predict(model_no_weight)
pred_noht <- predict(model_no_height)
pred_noboth <- predict(model_no_both)
pred_final <- predict(model_final)

# Build a data frame to compare AIC, accuracy, and -2 log-likelihood
model_comp <- data.frame(
  Model = c("Full Model", "No Weight", "No Height", "No Weight & Height", "Final Model"),
  AIC = c(AIC(model_full), AIC(model_no_weight), AIC(model_no_height), AIC(model_no_both), AIC(model_final)),
  Accuracy = c(
    mean(pred_full == data_clean$behpat),
    mean(pred_nowt == data_clean$behpat),
    mean(pred_noht == data_clean$behpat),
    mean(pred_noboth == data_clean$behpat),
    mean(pred_final == data_clean$behpat)
  ),
  Neg2LogLikelihood = c(
    -2 * logLik(model_full),
    -2 * logLik(model_no_weight),
    -2 * logLik(model_no_height),
    -2 * logLik(model_no_both),
    -2 * logLik(model_final)
  )
)

# Create a nicely formatted table with the final model highlighted
model_comp %>%
  kbl(caption = "-2 Log-Likelihood, AIC, and Accuracy for Model Comparison", digits = 3) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(5, background = "#d4edda")  # Highlight the Final Model row in light green


################Final Model Interpretation#############

#final model
model_final <- multinom(behpat ~ age + sbp+ dbp + chol 
                        + height + weight, data = data_clean)

# Extract coefficients and standard errors from model
coefs <- summary(model_final)$coefficients
ses <- summary(model_final)$standard.errors

# Compute z-values and p-values
z_values <- coefs / ses
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Round numbers for better display
coefs_rounded <- round(coefs, 4)
pvals_rounded <- round(p_values, 5)

# Combine into a single tidy table
library(tibble)
library(dplyr)
library(knitr)
library(kableExtra)

coef_table <- coefs_rounded %>%
  as.data.frame() %>%
  rownames_to_column("Category") %>%
  mutate(p_age   = pvals_rounded[, "age"],
         p_sbp   = pvals_rounded[, "sbp"],
         p_dbp   = pvals_rounded[, "dbp"],
         p_chol  = pvals_rounded[, "chol"],
         p_height= pvals_rounded[, "height"],
         p_weight= pvals_rounded[, "weight"]) %>%
  select(Category, age, p_age,
         sbp, p_sbp,
         dbp, p_dbp,
         chol, p_chol,
         height, p_height,
         weight, p_weight)

# Create table
kbl(coef_table, caption = "Estimates and P-values for the Final Multinomial Logistic Model",
    col.names = c("Group", "Age", "p-value", "SBP", "p-value", "DBP", "p-value",
                  "Chol", "p-value", "Height", "p-value", "Weight", "p-value")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))

## Group A2 vs. A1
library(nnet)
library(knitr)

# Fit the multinomial logistic regression model
model <- multinom(behpat ~ age + sbp + chol + height + dbp + weight, data = data_clean)

# Extract coefficients and standard errors for A2 vs A1
coefs_A2 <- summary(model)$coefficients["A2", ]
ses_A2 <- summary(model)$standard.errors["A2", ]

# Calculate Odds Ratios and 95% Confidence Intervals
OR_A2 <- exp(coefs_A2)
lower_CI_A2 <- exp(coefs_A2 - 1.96 * ses_A2)
upper_CI_A2 <- exp(coefs_A2 + 1.96 * ses_A2)

# Create a summary table - corrected version
table_A2 <- data.frame(
  Variable = c("Intercept", "age", "sbp", "chol", "height", "dbp", "weight"),  
  # Explicitly name variables
  Odds_Ratio = round(OR_A2, 3),
  CI_Lower = round(lower_CI_A2, 3),
  CI_Upper = round(upper_CI_A2, 3),
  row.names = NULL  # Prevents duplicate row names
)

# Display the table
kable(table_A2, align = "c", caption = "Odds Ratios and 95% CI for A2 vs A1")

## Group B3 vs. A1
library(nnet)
library(knitr)

# Fit the multinomial logistic regression model
model <- multinom(behpat ~ age + sbp + chol + height + dbp + weight, data = data_clean)

# Extract coefficients and standard errors for A2 vs A1
coefs_B3 <- summary(model)$coefficients["B3", ]
ses_B3 <- summary(model)$standard.errors["B3", ]

# Calculate Odds Ratios and 95% Confidence Intervals
OR_B3 <- exp(coefs_B3)
lower_CI_B3 <- exp(coefs_B3 - 1.96 * ses_B3)
upper_CI_B3 <- exp(coefs_B3 + 1.96 * ses_B3)

# Create a summary table - corrected version
table_B3 <- data.frame(
  Variable = c("Intercept", "age", "sbp", "chol", "height", "dbp", "weight"),  
  # Explicitly name variables
  Odds_Ratio = round(OR_B3, 3),
  CI_Lower = round(lower_CI_B3, 3),
  CI_Upper = round(upper_CI_B3, 3),
  row.names = NULL  # Prevents duplicate row names
)

# Display the table
kable(table_B3, align = "c", caption = "Odds Ratios and 95% CI for B3 vs A1")

## Group B4 vs. A1

library(nnet)
library(knitr)

# Fit the multinomial logistic regression model
model <- multinom(behpat ~ age + sbp + chol + height + dbp + weight, data = data_clean)

# Extract coefficients and standard errors for A2 vs A1
coefs_B4 <- summary(model)$coefficients["B4", ]
ses_B4 <- summary(model)$standard.errors["B4", ]

# Calculate Odds Ratios and 95% Confidence Intervals
OR_B4 <- exp(coefs_B4)
lower_CI_B4 <- exp(coefs_B4 - 1.96 * ses_B4)
upper_CI_B4 <- exp(coefs_B4 + 1.96 * ses_B4)

# Create a summary table - corrected version
table_B4 <- data.frame(
  Variable = c("Intercept", "age", "sbp", "chol", "height", "dbp", "weight"),  
  # Explicitly name variables
  Odds_Ratio = round(OR_B4, 3),
  CI_Lower = round(lower_CI_B4, 3),
  CI_Upper = round(upper_CI_B4, 3),
  row.names = NULL  # Prevents duplicate row names
)

# Display the table
kable(table_B4, align = "c", caption = "Odds Ratios and 95% CI for B4 vs A1")


############ Model Evaluation#############
# Confusion matrix heatmap
conf_table <- table(Predicted = pred_final, Actual = data_clean$behpat)
conf_df <- as.data.frame(conf_table)

library(ggplot2)
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Actual Class",
       y = "Predicted Class",
       fill = "Count") +
  theme_minimal()

table(data_clean$behpat)





