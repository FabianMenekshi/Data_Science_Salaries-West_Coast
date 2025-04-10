library(dplyr)
library(caret)
library(readxl)
library(skimr)
library(glue)
library(ggplot2)
library(glmnet)
pacman::p_load(pacman, ggthemes, httr, plotly, 
               rio, rmarkdown, shiny, stringr, tidyr, GGally)
#################################### Importing the dataset ########################################

file_path <- file.choose()
dataset <- read_excel(file_path)
skim(dataset)

################################### Data Cleaning ################################

#remove rows where the variable to be predicted, SALARY is missing
cat("Rows before removing missing salary:", nrow(dataset), "\n")
dataset <- dataset %>% filter(!is.na(SALARY))
cat("Rows after removing missing salary:", nrow(dataset))

#extract the highest education level substituting the four binary
#columns for education with the highest education required 
#and None if all are zero 

education_vars <- c("Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctorate", "Degree")
education_vars_final <- c("Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctorate", "Degree", "None")

encoding <- c("Associate's Degree" = 1, "Bachelor's Degree" = 2, "Master's Degree" = 3, "Degree" =3, "Doctorate"= 4)

education_df <- dataset[, education_vars]

dataset$HighestEducation <- apply(education_df, 1, function(row) {
  if (all(row == 0)) {
    return(0)
  }
  # Get the names of columns where the value is 1
  columns_with_1 <- names(row)[row == 1]
  
  # If multiple columns have 1, take the highest encoding
  max_encoding <- max(encoding[columns_with_1])
  
  # Return the max encoding
  return(max_encoding)
})

dataset$HighestEducation <- factor(dataset$HighestEducation, 
                                   levels = c(0, 1, 2, 3, 4), 
                                   labels = c("None", "Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctorate"), 
                                   ordered = TRUE)
#remove old education variables
dataset <- dataset %>% select(-all_of(education_vars))

#remove level for associate degree since it has zero entries
summary(dataset$HighestEducation)
dataset$HighestEducation <- droplevels(dataset$HighestEducation)

# Check updated levels and count of each level
levels(dataset$HighestEducation) 
summary(dataset$HighestEducation)
skim(dataset)

#convert locations from characters to factor variables
Locations <- unique(dataset$`searchInput/location`)
dataset$Location <- factor(dataset$`searchInput/location`, levels = Locations)
dataset <- dataset %>% select(-'searchInput/location')
skim(dataset)

#convert jobtype from characters to factor variables
jobtypes <- unique(dataset$Jobtype)
dataset$Jobtype <- factor(dataset$Jobtype, levels = jobtypes)
skim(dataset)

#convert seniority levels from characters to factor variables
seniority_levs <- unique(dataset$Seniority)
dataset$Seniority <- factor(dataset$Seniority, levels = seniority_levs)
skim(dataset)

#remove id column since the number of unique items matches the dataset size
#no duplicates present
dataset <- dataset %>% select(-'id')

# Conversion of binary variables to factors  

for (col in names(dataset)) {
  if (is.numeric(dataset[[col]]) & !col %in% c("SALARY", "EXPERIENCE", "Location", "HighestEducation", "Seniority", "Jobtype")) {  # Check if the column is numeric
    dataset[[col]] <- factor(dataset[[col]])  # Convert to factor
  }
}

skim(dataset)

#remove columns that only have zero entries 
for (col_name in colnames(dataset)) {
  column <- dataset[[col_name]]
  if (all(column %in% c(0, 1))) {
    num_ones <- sum(column == 1)
    if (num_ones < 1) {
      print(col_name)
      dataset <- dataset %>% select(-col_name)
}}}

skim(dataset)

########### EPLORATORY DATA ANALYSIS ############################################

#boxplots for salary
boxplot(dataset$SALARY, horizontal = TRUE, main = "Boxplot of Salaries", col = "lightgreen")

#salary by education
ggplot(dataset, aes(x = HighestEducation, y = SALARY, fill = HighestEducation)) +
  geom_boxplot() +
  geom_violin(trim = FALSE, alpha = 0.6) +
  labs(title = "Salary by Education", x = "HighestEducation", y = "Salary") +
  theme_minimal()

ggplot(dataset, aes(x = Jobtype, y = SALARY, fill = Jobtype)) +
  geom_boxplot() +
  geom_violin(trim = FALSE, alpha = 0.6) +
  labs(title = "Salary by Jobtype", x = "Jobtype", y = "Salary") +
  theme_minimal()

ggplot(dataset, aes(x = Location, y = SALARY, fill = Location)) +
  geom_boxplot() +
  geom_violin(trim = FALSE, alpha = 0.6) +
  labs(title = "Salary by Location", x = "Location", y = "Salary") +
  theme_minimal()

#histograms
#histogrm and KDE of EXPERIENCE
ggplot(dataset, aes(x = EXPERIENCE)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, 
                 fill = "orange", 
                 alpha = 0.5,   
                 color = "black") +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Salary Distribution with KDE and Histogram", 
       x = "Salary", 
       y = "Density") +
  theme_minimal()

#scatterplot of alary vs experience
plot(dataset$EXPERIENCE, dataset$SALARY, 
     cex = 0.5, 
     pch = 19,
     main = "Experience vs Salary (Color-coded by Doctorate)",
     xlab = "Experience",
     ylab = "Salary")

#bar plot - distribution of jobs in different states
ggplot(dataset, aes(x = Location)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Job Listings by Location", x = "Location", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#experience vs salary divided by state 
ggplot(dataset, aes(x = EXPERIENCE, y = SALARY, color = Location)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Location) +
  theme_minimal() +
  labs(title = "Experience vs Salary by Location", x = "Experience", y = "Salary")

library(patchwork)
#kde of salary with boxplot, scaled histogram and gaussian plot
par(mfrow = c(2, 1)) 

mean_salary <- mean(dataset$SALARY, na.rm = TRUE)
sd_salary <- sd(dataset$SALARY, na.rm = TRUE)

x_vals <- seq(min(dataset$SALARY, na.rm = TRUE), max(dataset$SALARY, na.rm = TRUE), length.out = 100)
gaussian_curve <- data.frame(
  x = x_vals,
  y = dnorm(x_vals, mean = mean_salary, sd = sd_salary),
  type = "Gaussian Curve"
)

histplot <- ggplot(dataset, aes(x = SALARY)) +
  geom_histogram(aes(y = after_stat(density), fill = "Histogram"), bins = 30, alpha = 0.5, color = "black") +
  geom_density(aes(color = "KDE (Gaussian kernel)"), linewidth = 1, linetype = "solid", alpha = 0.8) +
  geom_line(data = gaussian_curve, aes(x = x, y = y, color = "Fitted Gaussian density"), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Overlay of Scaled Histogram, KDE, and Gaussian Curve", #we can potentially change this
    x = "Salary",
    y = "Density",
    fill = "Legend",
    color = "Legend"
  ) +
  scale_fill_manual(
    values = c("Histogram" = "skyblue"),
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.75, 0.8),
    legend.title = element_text(hjust = 0.5, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"), 
    axis.text.y = element_blank()
  )+   guides(fill = "none", color = guide_legend(override.aes = list(shape = 20, linetype = 1)))  

boxplot_plot <- ggplot(dataset, aes(y = SALARY)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Boxplot",
    x = NULL,
    y = "Salary"
  ) +  theme_minimal() + theme(
    axis.text.y = element_blank())

# Combine the two plots
combined_plot <- boxplot_plot + histplot + plot_layout(widths = c(1, 2)) 

# Display the combined plot
combined_plot

#################################################################################
########## save the cleaned dataset to a new file excel and use it later ######################
# Get the current working directory 
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Save the dataframe to an Excel file
library(writexl)
write_xlsx(dataset, file.path(current_dir, "data_cleaned.xlsx"))

################################# HANDLING OUTLIERS  ############################
#qqplot of salary variable
par(mfrow = c(1, 1))
qqnorm(dataset$SALARY, main = "Normal Q-Q Plot of Salary", pch = 20, col = "blue")
qqline(dataset$SALARY, col = "red", lwd = 2)

#because of violation of normality assumption as seen in QQ-plot and extrapolation reasons
#remove all outliers as usually defined and analyse them to evaluate whether they belong 
#to a given class/have special features/ that render them valuable data points
#since the latter cannot be found, drop them

Q1 <- quantile(dataset$SALARY, 0.25)
Q3 <- quantile(dataset$SALARY, 0.75) 
IQR <- Q3 - Q1     

# Define the upper bound
upper_bound <- Q3 + 1.5 * IQR

# Filter the data to remove outliers
data_no_outliers <- dataset[dataset$SALARY <= upper_bound, ]
data_outliers <- dataset[dataset$SALARY > upper_bound, ]
skim(data_no_outliers)
skim(data_outliers)

########### !!DISCARDED!! dataset splitting for training and evaluation ####################################
############### REASON explained in discussion section of paper########################

set.seed(123) #set seed for replicability 
# split the data with 80% training 
train_indices <- createDataPartition(data_no_outliers$SALARY, p = 0.8, list = FALSE)
train_data <- data_no_outliers[train_indices, ]
test_data <- data_no_outliers[-train_indices, ]
skim(train_data)
skim(test_data)

naive_reg <- lm(SALARY ~ ., data = train_data)
summary(naive_reg) #evaluate regression on training dataset to extract
#parameters "importance" based on t-tests

cat("\nFeature Importance (p-values):\n")
print(summary(naive_reg)$coefficients)
p_values <- summary(naive_reg)$coefficients[, 4]  
significant_features <- rownames(summary(naive_reg)$coefficients)[p_values < 0.05]
# Print the significant features
cat("Significant features with p-value < 0.05:\n")
cat(significant_features)
cat(length(significant_features), " out of ", length(coef(naive_reg))-1)

#evalaute the model on the test set using RMSE, R^2 and R^2 adj.
predictions <- predict(naive_reg, newdata = test_data)
y <- test_data$SALARY
rmse <- sqrt(mean((y - predictions)^2)) 
cat("The RMSE of the linear regression on the test set is", rmse)
sst <- sum((y - mean(y))^2)  # Total sum of squares
sse <- sum((y - predictions)^2)       # Sum of squared errors
r2 <- 1 - (sse / sst)
n <- nrow(test_data) 
p <- length(coef(naive_reg))-1 
r2_adjusted <- 1 - ((1 - r2) * (n - 1) / (n - p-1))
cat("The R^2 and R^2_adj of the linear regression
    on the test set are", r2, " and ", r2_adjusted)

##################################################################################
################################## NAIVE ANALYSIS ###############################
#run linear regression on whole dataset to analyse the importance of different 
#features and evalaute if a linear fit of these columns is suitable

naive_reg <- lm(SALARY ~ ., data = data_no_outliers)
summary(naive_reg) #evaluate regression on training dataset to extract
#parameters "importance" based on t-tests

cat("\nFeature Importance (p-values):\n")
print(summary(naive_reg)$coefficients)
p_values <- summary(naive_reg)$coefficients[, 4]  
significant_features <- rownames(summary(naive_reg)$coefficients)[p_values < 0.05]
# Print the significant features
cat("Significant features with p-value < 0.05:\n")
print(significant_features)
cat(length(significant_features), " out of ", length(coef(naive_reg))-1)

# Calculate residuals and fitted values on training data checking assumptions
predictions <- naive_reg$fitted.values
residuals <- naive_reg$residuals
#evaluate the model on the test set using RMSE, R^2 and R^2 adj.
y <- data_no_outliers$SALARY
rmse <- sqrt(mean((y - predictions)^2)) 
cat("The RMSE of the linear regression on the dataset is", rmse)

# Add fitted values to the dataset
data_no_outliers$fitted <- predictions

# Scatter plot of fitted vs true values
ggplot(data_no_outliers, aes(x = SALARY, y = fitted , color = HighestEducation)) +
  geom_point( alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Fitted Values vs True Values colour-coded by EDU",
    x = "True Values (y)",
    y = "Fitted Values"
  ) +
  theme_minimal()
data_no_outliers <- data_no_outliers %>% select(-fitted)

#check the normality and homoscedasticity assumptions 

#residual diagnostics
cat("\nResidual Diagnostics:\n")
par(mfrow = c(1, 2))
plot(naive_reg)

# Create scatter plot of residuals vs fitted values
# Create a Normal Q-Q plot of the Salary variable
par(mfrow = c(1, 1))
qqnorm(data_no_outliers$SALARY, main = "Normal Q-Q Plot of Salary", pch = 20, col = "blue")
qqline(data_no_outliers$SALARY, col = "red", lwd = 2)

par(mfrow = c(1, 3))
plot(predictions, residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20,
     col = "blue")
abline(h = 0, col = "red", lwd = 2) # Add reference line at 0
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals, main = "Normal Q-Q Plot of Residuals", pch = 20, col = "blue")
qqline(residuals, col = "red", lwd = 2)

# Create scatter plot of residuals vs the only numerical predictor variable 
#to further check homoscedasticity
par(mfrow = c(1, 1))
plot(data_no_outliers$EXPERIENCE, residuals,
     main = "Residuals vs Experience",
     xlab = "EXPERIENCE",
     ylab = "Residuals",
     pch = 20,
     col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Perform Shapiro-Wilk test on Salary
shapiro_test <- shapiro.test(data_no_outliers$SALARY)
shapiro_test

# Perform Shapiro-Wilk test on residuals
shapiro_test <- shapiro.test(residuals)
shapiro_test

##################### BOX-COX TRANSFORMATION #########################################
#since salary spans multiple orders of magnitude implement a Box-cox transformation
#R^2 increases ONLY by 1% point - 
# not worth the loss in interpretability of results
#create a sqeuence of lambda values for which to perform the transformation
lambda_seq <- seq(0, 5, by = 0.1)

# Initialize variables to store results
best_R2_lambda <- NULL
max_r2 <- -Inf
results <- data.frame(Lambda = numeric(), R2 = numeric(), R2_adj = numeric())

# Iterate over lambda values
for (lambda in lambda_seq) {
  # Apply the Box-Cox transformation
  if (lambda == 0) {
    data_no_outliers$SALARY_trans <- log(data_no_outliers$SALARY)  } 
  else {
      data_no_outliers$SALARY_trans <- (data_no_outliers$SALARY^lambda - 1) / lambda
  }
  
  # Fit the linear regression model
  model <- lm(SALARY_trans ~ . - SALARY, data_no_outliers)
  # retrieve R^2 
  r2 <- summary(model)$r.squared
  r2_adjusted <- summary(model)$adj.r.squared
  # Store results
  results <- rbind(results, data.frame(Lambda = lambda, R2 = r2, R2_adj = r2_adjusted))
  
  # Update best lambda if necessary
  if (r2 > max_r2) {
    max_r2 <- r2
    best_R2_lambda <- lambda
  }
}

cat("the best R^2 on test data is ", max_r2, "for lambda = ", best_R2_lambda)

#plot the results of R^2 for different lambda 
ggplot(results, aes(x = Lambda, y = R2)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = best_R2_lambda, linetype = "dashed", color = "red") +
  labs(title = "Box-Cox Transformation: R^2 vs Lambda",
       x = "Lambda (Transformation Parameter)",
       y = "R^2")

data_no_outliers <- data_no_outliers %>% select(-c( "SALARY_trans"))
skim(data_no_outliers)
###################### Feature selection #######################################
############ LASSO and Elastic-net REGRESSION ######################################
# Convert categorical variables to dummy variables
dummy_vars <- dummyVars(SALARY ~ ., data = data_no_outliers, fullRank = TRUE)
data_encoded <- predict(dummy_vars, newdata = data_no_outliers)

# Separate features and target variable
x <- as.matrix(data_encoded)
y <- data_no_outliers$SALARY

#train lasso with 10-fold cross-validation
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "gaussian")
lasso_model_best <- glmnet(x, y, alpha = 1, lambda = lasso_model$lambda.min)

# predict salary on dataset using best lambda in training 
lasso_prediction <- predict(lasso_model_best, newx = x)

# analyse which variable coefficients have been set to zero by LASSO
lasso_coefficients <- as.matrix(coef(lasso_model_best))
coefficients_df <- as.data.frame(as.matrix(lasso_coefficients))
colnames(coefficients_df) <- c("Coefficient")

print(coefficients_df)

removed_features <- rownames(lasso_coefficients)[abs(lasso_coefficients[, 1]) < 0.01]
# Print the selected features
print(removed_features)

selected_features <- rownames(lasso_coefficients)[abs(lasso_coefficients[, 1]) > 0.01]
# Print the selected features
print(selected_features)

################# Evalaute the LASSO model using different metrics ###########################
# Calculate RMSE (Root Mean Square Error)
rmse <- sqrt(mean((y - lasso_prediction)^2))

# Calculate R^2 and R^2 adjusted
sst <- sum((y - mean(y))^2)  # Total sum of squares
sse <- sum((y - lasso_prediction)^2)       # Sum of squared errors
r2 <- 1 - (sse / sst)
n <- nrow(data_no_outliers) 
p <- sum(coef(lasso_model_best) != 0) - 1 #size of non-zero coefficients minus intercept
r2_adjusted <- 1 - ((1 - r2) * (n - 1) / (n - p-1))

# Print the metrics
cat("RMSE:", rmse, "\n")
cat("R^2:", r2, "\n")
cat("Adjusted R^2:", r2_adjusted, "\n")

#fit all possible models between ridge(alpha=0), elastic-net (with 0<alpha<1)
# and lasso to see which achieves lowest RMSE, highest R^2 adjusted 
set.seed(123)
alpha_seq <- seq(0, 0.95, by = 0.05) # Sequence of alpha values 

# Initialize variables to store results
results2 <- data.frame(alpha = numeric(), lambda.min = numeric(),
                      RMSE = numeric(), R2_adj = numeric(), R2 = numeric())

for (alpha in alpha_seq) {
  # Fit Elastic Net with cross-validation for current alpha
  model <- cv.glmnet(x, y, alpha = alpha, family = "gaussian")
  
  # Predict using the model with lambda.min
  y_pred <- predict(model, s = model$lambda.min, newx = x)
  
  # Compute RMSE, R^2 and R^2 adjusted
  sst <- sum((y - mean(y))^2)  # Total sum of squares
  sse <- sum((y - y_pred)^2)       # Sum of squared errors
  r2 <- 1 - (sse / sst)
  n <- nrow(data_no_outliers) 
  p <- sum(coef(model, s = "lambda.min") != 0) - 1 #size of non-zero coefficients minus intercept
  r2_adjusted <- 1 - ((1 - r2) * (n - 1) / (n - p-1))
  rmse <- sqrt(mean((y - y_pred)^2))
  
  # Store the results
  results2 <- rbind(results2, data.frame(alpha = alpha, lambda.min = model$lambda.min,
                                       RMSE = rmse, R2_adj = r2_adjusted, R2 = r2))
}
best_result_rmse <- results2[which.min(results2$RMSE), ]
best_result_R2 <- results2[which.max(results2$R2), ]
best_result_R2_ADJ <- results2[which.max(results2$R2_adj), ]

cat("Best RMSE alpha:", best_result_rmse$alpha, "\n")
cat("Corresponding lambda.min:", best_result_rmse$lambda.min, "\n")
cat("Lowest RMSE:", best_result_rmse$RMSE, "\n")

cat("Best adjusted R^2 alpha:", best_result_R2_ADJ$alpha, "\n")
cat("Corresponding lambda.min:", best_result_R2_ADJ$lambda.min, "\n")
cat("Highest adjusted R^2:", best_result_R2_ADJ$R2_adj, "\n")

cat("Best R^2 alpha:", best_result_R2$alpha, "\n")
cat("Corresponding lambda.min:", best_result_R2$lambda.min, "\n")
cat("Highest R^2:", best_result_R2$R2, "\n")

ggplot(results2, aes(x = alpha, y = RMSE)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue", linetype = "dashed") +
  labs(title = "RMSE vs Alpha in Elastic Net Regression",
       x = "Alpha",
       y = "Root Mean Squared Error (RMSE)") +
  theme_minimal()

ggplot(results2, aes(x = alpha, y = R2_adj)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue", linetype = "dashed") +
  labs(title = "R^2 adjsuted vs Alpha in Elastic Net Regression",
       x = "Alpha",
       y = "R^2 adjsuted") +
  theme_minimal()

ggplot(results2, aes(x = alpha, y = R2)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue", linetype = "dashed") +
  labs(title = "R^2 vs Alpha in Elastic Net Regression",
       x = "Alpha",
       y = "R^2") +
  theme_minimal()

############################### INTERACTION TERMS ################################
####################### STEP-UP MODEL for INTERACTION TERMS ###############################
#add sensible interaction terms which improve R^2 adjusted
int_model <- lm(SALARY ~ . + Seniority : EXPERIENCE, data=data_no_outliers)
summary(int_model)
#performance improvement is quite significant
predictions <- int_model$fitted.values
y <- data_no_outliers$SALARY
rmse <- sqrt(mean((y - predictions)^2))
# Calculate R^2 and R^2 adjusted
r2 <- summary(int_model)$r.squared
r2_adjusted <- summary(int_model)$adj.r.squared

# Print the metrics
cat("RMSE:", rmse, "\n")
cat("R^2:", r2, "\n")
cat("Adjusted R^2:", r2_adjusted, "\n")
cat("BIC of naive", BIC(naive_reg), "; BIC of interaction",BIC(int_model))
cat("AIC of naive", AIC(naive_reg), "; BIC of interaction",AIC(int_model))

#since improvement seems significant both for R^2 adjusted
#create a step-up model for interaction terms selection based on BIC computed on model 
variables <- colnames(data_no_outliers)
variables <- variables[variables != "SALARY"]
# Fit the initial model with main effects only
initial_model <- lm(SALARY ~ ., data = data_no_outliers)
best_model <- initial_model
best_interactions <- c()
best_bic <- BIC(initial_model) #comment if want to run with AIC
#best_bic <- AIC(initial_model) uncomment if want to run with BIC

# Step-up process for interaction terms
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    # Define the interaction term
    new_interaction_term <- paste0("`", variables[i], "`", ":", "`", variables[j], "`")
    #print(new_interaction_term)
    interaction_terms <- c(best_interactions, new_interaction_term)
    print(new_interaction_term)
    # Create the full interaction term string
    interaction_term_string <- paste(interaction_terms, collapse = " + ")
    # Fit a model with the interaction term added
    formula_with_interaction <- as.formula(
      paste("SALARY ~ . +", interaction_term_string)
    )
    #print(formula_with_interaction)
    model_with_interaction <- lm(formula_with_interaction, data = data_no_outliers)
    print(summary(model_with_interaction)$r.squared)
    # Compute BIC
    #new_bic <- AIC(model_with_interaction) #uncomment if want to see results with AIC 
    new_bic <- BIC(model_with_interaction) #comment if want to see results with BIC
    
    # Check if BIC improved
    if (new_bic < best_bic) {
      best_bic <- new_bic
      best_interactions <- interaction_terms
      #print(interaction_terms)
    }
  }
}

# Print the final model and selected interaction terms
cat("\nSelected Interaction Terms:\n")
print(best_interactions)
#due to missing values
interaction_to_remove <- c("`Jobtype`:`emotional intelligence`")

# Remove the specified interaction from the best_interactions vector
best_interactions <- setdiff(best_interactions, interaction_to_remove)
interaction_term_string <- paste(best_interactions, collapse = " + ")
formula_with_interaction <- as.formula(
  paste("SALARY ~ . +", interaction_term_string)
)
final_model <- lm(formula_with_interaction, data = data_no_outliers)
summary(final_model)
cat("BIC of final model", BIC(final_model))

predictions <- final_model$fitted.values
y <- data_no_outliers$SALARY
rmse <- sqrt(mean((y - predictions)^2))

# Print the metrics
cat("RMSE:", rmse, "\n")

#########################STEP-UP and STEP-DOWN MODEL WITH AIC/BIC ##################
set.seed(123)
#implement step-up/down based on AIC score
full_model <- lm(SALARY ~ ., data = data_no_outliers)

# Perform step-down (backward elimination) using AIC
#step_down_model <- step(full_model, direction = "backward", trace = 0)
#summary(step_down_model)
#rownames(summary(step_down_model)$coefficients)
# Start with an empty model
empty_model <- lm(SALARY ~ 1, data = data_no_outliers)

# Perform step-up (forward selection) using AIC
#step_up_model <- step(empty_model, direction = "forward", scope = formula(full_model), trace = 0)
#summary(step_up_model)
#rownames(summary(step_up_model)$coefficients)

#implement step_up\down based on BIC
step_down_model2 <- step(full_model, direction = "backward", 
                        k = log(nrow(data_no_outliers)), trace = 0)
summary(step_down_model2)
rownames(summary(step_down_model2)$coefficients)

step_up_model2 <- step(empty_model, direction = "forward", 
                      scope = formula(full_model),k = log(nrow(data_no_outliers)), trace = 0)
summary(step_up_model2)
rownames(summary(step_up_model2)$coefficients)

################################################################################
############################# HYPOTHESIS TESTING ###################################

colnames(data_no_outliers) <- make.names(colnames(data_no_outliers))

# Update skill_columns with the "sanitized" names
skill_columns <- c(
  "Compuer_vision", "NLP_software", "Deep_learning", "database_software", 
  "visualisation_software", "general_DS_software", "python", "r", "sql", 
  "scala", "java", "c..", "matlab", "sas", "javascript", "ruby", "php", 
  "perl", "Swift", "kotlin", "shell", "c", "rust",
  "communication", "teamwork", "problem.solving", "creativity", "adaptability", 
  "time.management", "critical.thinking", "leadership", "collaboration", 
  "interpersonal.skills", "conflict.resolution", "emotional.intelligence", 
  "organization", "decision.making", "active.listening", "flexibility", 
  "attention.to.detail", "self.motivation", "motivation"
)

t_test_results <- data.frame(var = character(), p_val = numeric(), significant = logical())
for (column in skill_columns){
  # Extract rows with class 0/1 and compute the mean salary
  class_0 <- data_no_outliers$SALARY[data_no_outliers[[column]] == 0]
  class_1 <- data_no_outliers$SALARY[data_no_outliers[[column]] == 1]
  t_test <- t.test(class_1, class_0, alternative = "greater", var.equal = FALSE)
  t_test_results <- rbind(t_test_results, 
                          data.frame(var = column, p_val = t_test$p.value,
                                     significant = t_test$p.value < 0.05))
}

print(t_test_results)

# Filtering significant skills
cat("\nSignificant Skills increasing Salary:\n")
significant_skills <- t_test_results %>% filter(significant == TRUE)
print(significant_skills)

####################### perform ANOVA with tests #######################
cat("Performing F-test for all covariates with ANOVA")
# Perform ANOVA
anova_reg <- aov(lm(SALARY ~ ., data = data_no_outliers))
# View the summary
summary(anova_reg)
############################################################################

####EDUCATION 
education_anova <- aov(SALARY ~ HighestEducation, data = data_no_outliers)
education_anova_summary <- summary(education_anova)
education_tukey <- TukeyHSD(education_anova)

cat("\nDoes Education Level Affect Salary?\n")
print(education_anova_summary)
cat("\nTukey's Post-Hoc Test Results for Education:\n")
print(education_tukey)

#study Doctorate against all other classes 
group1 <- "Doctorate"

group1_data <- dataset %>% filter(HighestEducation == group1) %>% pull(SALARY)
group2_data <- dataset %>% filter(HighestEducation != group1) %>% pull(SALARY)

# one-sided t-test (group1 > group2)
t_test_result <- t.test(group1_data, group2_data, alternative = "greater")

cat("\nT-Test: Mean Salary of", group1, "vs all classes (One-Sided, Greater):\n")
print(t_test_result)

# one-sided t-tests for specific group comparisons
edu_groups <- levels(dataset$HighestEducation)

cat("\nOne-Sided T-Tests: Mean Salary Comparisons Between Education Levels\n")
for (j in 1:length(edu_groups)) {
    group1 <- "Doctorate"
    group2 <- edu_groups[j]
    
    group1_data <- dataset %>% filter(HighestEducation == group1) %>% pull(SALARY)
    group2_data <- dataset %>% filter(HighestEducation == group2) %>% pull(SALARY)
    
    # one-sided t-test (group1 > group2)
    t_test_result <- t.test(group1_data, group2_data, alternative = "greater")
    
    cat("\nT-Test: Mean Salary of", group1, "vs", group2, "(One-Sided, Greater):\n")
    print(t_test_result)
  }


