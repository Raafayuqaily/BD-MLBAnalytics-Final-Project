# Reset workspace
if (dev.cur() > 1) dev.off()  # Close any pre-existing plots
rm(list = ls())  # Clear all existing variables

# Load necessary libraries
library(dplyr)        # For data manipulation
library(tidyr)        # For data cleansing
library(ggplot2)      # For plotting
library(corrplot)     # For visualizing multicollinearity
library(car)          # For detecting multicollinearity
library(plotly)       # For interactive plots
library(reshape2)     # For the melt function
library(e1071)        # For skewness calculation
library(glmnet)       # For Lasso regression
library(caret)        # For R^2 calculation
library(gamlr)        # For Lasso regression with AICc
library(stats)        # For PCA
library(knitr)
library(patchwork)
library(gridExtra)    # For arranging plots
library(tibble)       # For working with tibbles
library(parallel)     # For parallel processing
library(randomForest) # For Random Forest analysis
library(randomForestExplainer)
library(rpart)
library(rpart.plot)

#----
# Loading Data

# Read CSV
games_raw <- read.csv("odds_and_R_data_Condensed_Final!.csv")
games_raw$total_points <- games_raw$V_Final + games_raw$H_Final

# Convert character variables to factors
games_df <- games_raw %>%
  mutate(across(where(is.character), as.factor))

# Dealing with NA values
games_df[] <- lapply(games_df, function(x) {
  if (is.numeric(x)) {
    return(replace(x, is.na(x), 0))  # Replace NA with 0 for numeric columns
  } else if (is.character(x)) {
    x <- replace(x, is.na(x), "Unknown")  # Replace NA with "Unknown" for character columns
    return(factor(x))
  } else {
    return(x)
  }
})

# Identifying factor variables with more than 100 levels
high_level_factors <- sapply(games_df, function(x) if (is.factor(x)) nlevels(x) else 0)
high_level_factors <- high_level_factors[high_level_factors > 100]

# Removing variables with more than 100 factor levels
games_df_reduced <- games_df %>%
  select(-names(high_level_factors))

# Remove specific columns
games_df_reduced <- games_df_reduced %>%
  select(-Day, -Home_Team_Code, -Visiting_Team_Code)

#----
# Getting a sense of the existing data

# Correlation matrix and plot
numeric_data <- select(games_df_reduced, where(is.numeric))  # Only numeric data
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")  # Handle missing values

# Save the cleaned data
write.csv(numeric_data, file = "numeric_data.csv", row.names = FALSE)

# Correlation plot
cor_matrix_melted <- melt(cor_matrix)
p <- ggplot(data = cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  ggtitle("Correlation Matrix Heatmap")
ggsave("correlation_plot.png", plot = p, width = 10, height = 8)

#----
# FDR Analysis

# General regression
fit_games <- glm(run_dif ~ ., data = numeric_data, family = "gaussian")
fit_summary <- summary(fit_games)

# Extract coefficients, p-values, and variable names
coefficients <- fit_summary$coefficients
coef_df <- as.data.frame(coefficients)
colnames(coef_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Sort by p-value
sorted_coef_df <- coef_df %>%
  rownames_to_column(var = "Variable") %>%
  arrange(`Pr(>|t|)`)

# Select top and bottom 5 variables by significance
top_5 <- head(sorted_coef_df, 5)
bottom_5 <- tail(sorted_coef_df, 5)
selected_vars <- rbind(top_5, bottom_5)

# Print the selected variables to the console
print(selected_vars)

# Source the FDR function
source("fdr.R")

# P-value distribution
p_values <- fit_summary$coefficients[, 4]
p_values <- p_values[-1]  # Drop intercept
ordered_p_values <- sort(p_values, decreasing = FALSE)

# P-value histogram
p_values_df <- data.frame(p_values = p_values)
histogram_p_values <- ggplot(p_values_df, aes(x = p_values)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "P-value Distribution Histogram", x = "P-values", y = "Frequency") +
  theme_minimal()
ggsave("histogram_p_values.png", plot = histogram_p_values, width = 10, height = 6, dpi = 300)

# Create the cumulative density plot using ggplot2
cumulative_density_p_values <- ggplot(p_values_df, aes(x = p_values)) +
  stat_ecdf(geom = "step", color = "skyblue") +
  labs(title = "P-value Cumulative Density Plot", x = "P-values", y = "Cumulative Density") +
  theme_minimal()
ggsave("cumulative_density_p_values.png", plot = cumulative_density_p_values, width = 10, height = 6, dpi = 300)

# Skewness of P-values
cat("Skewness of P-values for run-dif is:", skewness(p_values), "\n")

# FDR cutoff 1%
alpha <- 0.01
fdr_cutoff <- fdr_cut(pvals = ordered_p_values, q = alpha, plotit = TRUE, title = "FDR Analysis (Run_dif), q = 0.1")
cat("The p-value cutoff associated with a 1% FDR rate is: ", fdr_cutoff, "\n")
print(sum(p_values <= fdr_cutoff))

# Extract the top 10 smallest p-values and their corresponding variable names
top_10_p_values <- head(sort(p_values), 10)
top_10_variables <- names(top_10_p_values)

# Print the top 10 variables and their p-values with 5 decimal places in kable format
top_10_df <- data.frame(Variable = top_10_variables, P_Value = round(top_10_p_values, 5))
print(top_10_df)

cat("Number of discoveries expected to be false at 1% FDR level: ", alpha * sum(p_values <= fdr_cutoff), "\n")

#----
# Summary Statistics and Plots

# Summary statistics for run_dif
run_dif_summary <- summary(numeric_data$run_dif)
print(run_dif_summary)

# Histogram for run_dif
histogram_run_dif <- ggplot(numeric_data, aes(x = run_dif)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Run Differences", x = "Run Difference", y = "Frequency") +
  theme_minimal()
ggsave("histogram_run_dif.png", plot = histogram_run_dif, width = 10, height = 6, dpi = 300)

# Boxplot for run_dif
boxplot_run_dif <- ggplot(numeric_data, aes(y = run_dif)) +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(title = "Boxplot of Run Differences", x = "Data", y = "Run Difference") +
  theme_minimal()
ggsave("boxplot_run_dif.png", plot = boxplot_run_dif, width = 10, height = 6, dpi = 300)

# Bar Plot for home_win
barplot_home_win <- ggplot(numeric_data, aes(x = factor(home_win), fill = factor(home_win))) +
  geom_bar() +
  scale_fill_manual(values = c("red", "green"), labels = c("Loss", "Win")) +
  labs(title = "Bar Plot of Home Wins", x = "Home Win (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()
ggsave("barplot_home_win.png", plot = barplot_home_win, width = 10, height = 6, dpi = 300)

# Compute the counts and percentage of home wins
home_win_counts <- table(numeric_data$home_win)
home_win_percentage <- 100 * home_win_counts["1"] / sum(home_win_counts)
print(home_win_counts)
print(paste("Percentage of times the home team wins:", home_win_percentage, "%"))

# Summary statistics for HmH
HmH_summary <- summary(numeric_data$HmH)
print(HmH_summary)

# Histogram for HmH
histogram_HmH <- ggplot(numeric_data, aes(x = HmH)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Histogram of HmH", x = "HmH", y = "Frequency") +
  theme_minimal()
ggsave("histogram_HmH.png", plot = histogram_HmH, width = 10, height = 6, dpi = 300)

# Boxplot for HmH
boxplot_HmH <- ggplot(numeric_data, aes(y = HmH)) +
  geom_boxplot(fill = "cyan", color = "black") +
  labs(title = "Boxplot of HmH", x = "Data", y = "HmH") +
  theme_minimal()
ggsave("boxplot_HmH.png", plot = boxplot_HmH, width = 10, height = 6, dpi = 300)

# Skewness of HmH
print(skewness(numeric_data$HmH))

# Summary statistics for H_Final
H_Final_summary <- summary(numeric_data$H_Final)
print(H_Final_summary)

# Histogram for H_Final
histogram_H_Final <- ggplot(numeric_data, aes(x = H_Final)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  labs(title = "Histogram of H_Final", x = "H_Final", y = "Frequency") +
  theme_minimal()
ggsave("histogram_H_Final.png", plot = histogram_H_Final, width = 10, height = 6, dpi = 300)

# Boxplot for H_Final
boxplot_H_Final <- ggplot(numeric_data, aes(y = H_Final)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot of H_Final", x = "Data", y = "H_Final") +
  theme_minimal()
ggsave("boxplot_H_Final.png", plot = boxplot_H_Final, width = 10, height = 6, dpi = 300)

# Skewness of H_Final
print(skewness(numeric_data$H_Final))

# Compute win percentage for each home team
team_win_percentage <- games_raw %>%
  group_by(Home_Team_Code) %>%
  summarize(total_games = n(), total_wins = sum(home_win), win_percentage = total_wins / total_games * 100) %>%
  arrange(desc(win_percentage))
print(team_win_percentage)

# Bar plot of win percentages by home team
win_percentage_plot <- ggplot(team_win_percentage, aes(x = reorder(Home_Team_Code, -win_percentage), y = win_percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Home Team Win Percentage", x = "Home Team", y = "Win Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("win_percentage_plot.png", plot = win_percentage_plot, width = 12, height = 8, dpi = 300)

# Compute average run difference for each home team
team_avg_run_dif <- games_raw %>%
  group_by(Home_Team_Code) %>%
  summarize(total_games = n(), avg_run_dif = mean(run_dif, na.rm = TRUE)) %>%
  arrange(desc(avg_run_dif))

print(team_avg_run_dif)

# Bar plot of average run difference by home team
avg_run_dif_plot <- ggplot(team_avg_run_dif, aes(x = reorder(Home_Team_Code, -avg_run_dif), y = avg_run_dif)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Run Difference by Home Team", x = "Home Team", y = "Average Run Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("avg_run_dif_plot.png", plot = avg_run_dif_plot, width = 12, height = 8, dpi = 300)

# Define a function to create scatter plots with correlation coefficient
create_scatter_plot <- function(data, x_var, y_var) {
  cor_coeff <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = paste("Scatterplot of", y_var, "vs", x_var),
         x = x_var, y = y_var) +
    annotate("text", x = Inf, y = Inf, label = paste("r =", round(cor_coeff, 2)), 
             hjust = 1.1, vjust = 2, size = 5, color = "red") +
    theme_minimal()
  return(plot)
}

# Create individual scatter plots
plot1 <- create_scatter_plot(games_raw, "Duration", "run_dif")
plot2 <- create_scatter_plot(games_raw, "HmH", "run_dif")
plot3 <- create_scatter_plot(games_raw, "HmHR", "run_dif")
plot4 <- create_scatter_plot(games_raw, "H_Final", "run_dif")

# Combine the plots into one image
combined_plot <- (plot1 | plot2) / (plot3 | plot4)

# Save the combined plot as a PNG file
ggsave("combined_scatter_plots.png", plot = combined_plot, width = 14, height = 10, dpi = 300)

#----
# Lasso Regression

# Prepare the data for Lasso regression
x <- as.matrix(numeric_data %>% select(-run_dif, -home_win))
y <- numeric_data$run_dif

# Run Lasso using the best lambda based on AICc
lasso1 <- gamlr(x, y, lambda.min.ratio = 1e-3)
print(lasso1)
plot(lasso1, main = "Lasso Path")

# Find the best lambda to minimize AICc
lasso1_sum <- summary(lasso1)
best_lambda_aicc <- lasso1_sum[which.min(lasso1_sum$aicc), "lambda"]
cat("The best lambda to minimize AICc is:", best_lambda_aicc, "\n")

source("deviance.R")

# Make predictions and compute in-sample R^2
lasso1_pred <- predict(lasso1, newdata = x, type = "response")
lasso1_r2 <- R2(y = y, pred = lasso1_pred, family = "gaussian")
cat("The in-sample R^2 is:", lasso1_r2, "\n")

coefficients <- coef(lasso1, s = lasso1$lambda.aicc)[, 1]
non_zero_coefficients <- coefficients[coefficients != 0]
significant_predictors <- non_zero_coefficients[order(abs(non_zero_coefficients), decreasing = TRUE)]

cat("There are ", length(significant_predictors), " predictors that have non-zero coefficients. \n")

top_10_predictors <- data.frame(Predictor = names(significant_predictors), Coefficient = significant_predictors)
top_10_predictors <- head(top_10_predictors, 10)

#----
# Bootstrap Lasso

# Prepare the outcome variable and the feature matrix
Outcome <- numeric_data$run_dif
spm2 <- as.matrix(numeric_data %>% select(-run_dif, -home_win, -H_Final, -V_Final)) # Feature matrix

# Create a cluster for parallel processing
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("spm2", "Outcome"))

# Define the bootstrap function
boot_function <- function(ib) {
  require(gamlr)
  fit <- gamlr(spm2[ib,], y = Outcome[ib], lambda.min.ratio = 1e-3)
  fit$lambda[which.min(AICc(fit))]
}

# Set the number of bootstrap samples
boots <- 100
n <- nrow(spm2)

# Generate bootstrap samples
resamp <- as.data.frame(matrix(sample(1:n, boots * n, replace = TRUE), ncol = boots))

# Perform parallel bootstrap sampling
lambda_samp <- unlist(parLapply(cl, resamp, boot_function))

# Summarize the bootstrap results
summary(lambda_samp)
std_dev <- sd(lambda_samp)
cat("The standard error for the selected lambda is: ", std_dev, '\n')

lower_bound <- quantile(lambda_samp, 0.025)
upper_bound <- quantile(lambda_samp, 0.975)
cat("The 95% Confidence Interval for lambda: [", lower_bound, ",", upper_bound, "]\n")

# Plot the bootstrapped lambda distribution histogram
hist(lambda_samp, col = 'green', main = "Bootstrapped Lambda Distribution", breaks = 25, xlab = "Lambda")
mtext("Fig 16. Bootstrapped Lambda Distribution Histogram.", side = 3, line = 1, cex = 0.75)
abline(v = lower_bound, col = 'red', lwd = 2)
abline(v = upper_bound, col = 'red', lwd = 2)
abline(v = mean(lambda_samp), col = 'blue', lwd = 2)

# Stop the cluster
stopCluster(cl)

#----
# Random Forest Analysis

x <- as.matrix(numeric_data %>% select(-run_dif, -home_win, -H_Final, -V_Final)) # run_dif is constructed from H_Final and V_Final
y <- numeric_data$run_dif

# Fit a random forest model for regression
rf_model <- randomForest(x, y, ntree = 100, importance = TRUE)

# Print the model summary
print(rf_model)

# Extract variable importance
importance_df <- as.data.frame(importance(rf_model))
importance_df <- rownames_to_column(importance_df, var = "Variable")

# Sort by importance
sorted_importance_df <- importance_df %>%
  arrange(desc(IncNodePurity))

# Select the top 20 most important variables
top20_importance <- sorted_importance_df[1:20, ]

# Print the top 20 variables
print(top20_importance)

# Create a bar plot for the top 20 most important variables
importance_plot <- ggplot(top20_importance, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 20 Most Important Variables in Random Forest Model",
       x = "Variable",
       y = "Importance (IncNodePurity)") +
  theme_minimal()
ggsave("top20_importance.png", plot = importance_plot, width = 10, height = 6, dpi = 300)

# Calculate the in-sample R-squared for Random Forest model
in_sample_preds <- predict(rf_model, x)
ss_total <- sum((y - mean(y))^2)
ss_residual <- sum((y - in_sample_preds)^2)
rf_r2 <- 1 - (ss_residual / ss_total)

cat("The in-sample R^2 for RF is:", rf_r2, "\n")

#--- 
#CV Lasso

# Prepare the data
d <- numeric_data$run_dif
spm <- as.matrix(numeric_data %>% select(-run_dif))

# Cross-validated Lasso regression using gamlr
cv.treat <- cv.gamlr(spm, d)

# Plot the cross-validation results
plot(cv.treat, main = "")
mtext("Fig 18. Lasso Mean Squared Error (Cross Validation).", side = 3, line = 1, cex = 0.75)

# Make predictions
dhat <- predict(cv.treat$gamlr, spm)
dhat <- drop(dhat)

# Calculate R-squared
D <- sum((d - dhat)^2)
D0 <- sum((d - mean(d))^2)
R2 <- 1 - D / D0
cat("The in-sample R^2 is: ", R2, '\n')


#---
# K-Means Clustering

# Define a function to perform k-means clustering and plot the results
perform_kmeans <- function(data, x_var, y_var, centers = 3) {
  # Prepare the data
  kmeans_data <- data %>% select(all_of(c(x_var, y_var)))
  kmeans_data <- na.omit(kmeans_data)  # Remove any NA values
  
  # Perform k-means clustering
  set.seed(123)  # For reproducibility
  kmeans_result <- kmeans(kmeans_data, centers = centers, nstart = 20)
  
  # Add cluster information to the data
  kmeans_data$cluster <- as.factor(kmeans_result$cluster)
  
  # Plot the results
  plot <- ggplot(kmeans_data, aes_string(x = x_var, y = y_var, color = "cluster")) +
    geom_point(alpha = 0.7, size = 3) +
    labs(title = paste("K-means Clustering (k =", centers, ")"),
         x = x_var, y = y_var) +
    theme_minimal()
  
  return(plot)
}

# Variables to be analyzed
independent_vars <- c("Duration", "HmH", "HmHR", "H_Final")
dependent_var <- "run_dif"

# Perform k-means clustering for each independent variable
plots <- list()
for (x_var in independent_vars) {
  plot <- perform_kmeans(numeric_data, x_var, dependent_var, centers = 3)
  plots[[x_var]] <- plot
}

# Combine the plots into one image
combined_plot <- (plots[["Duration"]] | plots[["HmH"]]) / (plots[["HmHR"]] | plots[["H_Final"]])

# Save the combined plot as a single PNG file
ggsave("combined_kmeans_plots.png", plot = combined_plot, width = 14, height = 10, dpi = 300)

# Display the combined plot
print(combined_plot)

#----
# Principal Component Analysis

# PCA Analysis
pca_result <- prcomp(numeric_data, scale. = TRUE)  # Use 'scale.' instead of 'scale'

# Summary of PCA results
pca_summary <- summary(pca_result)
print(pca_summary)

# Save the PCA screeplot
png("pca_screeplot.png", width = 800, height = 600)
plot(pca_result, type = "lines", main = "Screeplot of PCA")
dev.off()

# Display PCA rotation (loadings) for the first 3 principal components
pca_loadings <- as.data.frame(pca_result$rotation[, 1:3])
kable(pca_loadings, caption = "Principal Component Loadings")

# Calculate the variance explained by each principal component
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)  # Proportion of variance explained
cum_var_explained <- cumsum(var_explained)

# Save the cumulative variance explained plot
png("cumulative_variance_explained.png", width = 800, height = 600)
plot(cum_var_explained, type = "b", xlab = "Principal Components", ylab = "Cumulative Proportion of Variance Explained", main = "Cumulative Variance Explained")
abline(h = 0.50, col = "red", lty = 2)  # 50% threshold
dev.off()

# Print the number of components that explain at least 50% of the variance
num_components_50 <- which(cum_var_explained >= 0.50)[1]
cat("Number of components that explain at least 50% of the variance:", num_components_50, "\n")

# Save the PCA screeplot with cumulative variance explained
png("pca_screeplot_with_cumvar.png", width = 800, height = 600)
barplot(var_explained, main = "Scree Plot with Cumulative Variance Explained", xlab = "Principal Components",
        ylab = "Proportion of Variance Explained", ylim = c(0, 0.5), col = "blue")
lines(cum_var_explained, type = "b", pch = 20, col = "red", lwd = 2)
legend("topleft", legend = c("Variance Explained", "Cumulative Variance"), fill = c("blue", "red"), horiz = FALSE, cex = 0.7)
dev.off()

# Extract the scores for the selected principal components
scores <- pca_result$x[, 1:num_components_50]

# Create a new dataframe combining PCA scores with run_dif
games_df_pca <- data.frame(scores, run_dif = numeric_data$run_dif)

# Re-run the regression model using the principal components
fit_pca <- glm(run_dif ~ ., data = games_df_pca, family = "gaussian")
fit_pca_summary <- summary(fit_pca)
print(fit_pca_summary)

# Displaying significant variables using PCA results
alpha <- 0.05  # Significance level
significant_vars <- which(fit_pca_summary$coefficients[, 4] <= alpha)
sig_var_names_pca <- rownames(fit_pca_summary$coefficients)[significant_vars]
print("Significant variables using PCA results:")
print(sig_var_names_pca)



