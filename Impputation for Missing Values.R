#####Imputation for Missing Values-------
# Load necessary libraries
if (!require("mice")) install.packages("mice")
if (!require("VIM")) install.packages("VIM")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("gridExtra")) install.packages("gridExtra")

library(mice)
library(VIM)
library(ggplot2)
library(reshape2)
library(gridExtra)

# Start with the existing dataset dfd
df <- dfd
# Create a copy to simulate the "true" values for error calculation
# Note: In a real scenario, you would need actual true values for comparison
df_true <- df

# Method 1: Simple imputation with mean/median
df_imputed_simple <- df
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    # For numeric columns, impute with mean
    df_imputed_simple[[col]][is.na(df_imputed_simple[[col]])] <- mean(df_imputed_simple[[col]], na.rm = TRUE)
  } else {
    # For categorical/factor columns, impute with mode
    if (sum(is.na(df_imputed_simple[[col]])) > 0) {
      mode_value <- names(sort(table(df_imputed_simple[[col]]), decreasing = TRUE)[1])
      df_imputed_simple[[col]][is.na(df_imputed_simple[[col]])] <- mode_value
    }
  }
}

# Method 2: Multiple imputation with mice using ridge penalty to handle singularity
set.seed(123)
# First check for columns with zero variance which can cause issues
zero_var_cols <- sapply(df, function(x) var(x, na.rm = TRUE) == 0)
if(any(zero_var_cols)) {
  print("Warning: The following columns have zero variance and may cause problems:")
  print(names(df)[zero_var_cols])
}

# Try mice with a ridge penalty to handle the singularity
try({
  imputation <- mice(df, m = 5, method = "pmm", maxit = 50, printFlag = FALSE, ridge = 0.01)
  df_imputed_mice <- complete(imputation)
}, silent = TRUE)

# If the above still fails, try alternative approach
if(!exists("df_imputed_mice")) {
  print("MICE with ridge penalty failed. Trying alternative imputation method.")
  
  # Option 1: Use different methods for each variable
  methods_vector <- rep("pmm", ncol(df))
  
  # Option 2: Use a simpler imputation method as fallback
  imputation <- mice(df, m = 5, method = "sample", maxit = 20, printFlag = FALSE)
  df_imputed_mice <- complete(imputation)
  
  # If all else fails, create a copy of the simple imputation as a fallback
  if(!exists("df_imputed_mice")) {
    df_imputed_mice <- df_imputed_simple
    print("MICE imputation failed. Using simple imputation results as fallback for MICE.")
  }
}

#######
# Method 3: KNN imputation
df_imputed_knn <- kNN(df, k = 5)
df_imputed_knn <- df_imputed_knn[, 1:ncol(df)] # Remove indicator columns

# Modified error calculation function to work with missing data scenario
# Instead of comparing to "true" values (which we don't have), 
# this will evaluate based on the distribution characteristics
evaluate_imputation <- function(df_original, df_imputed, method_name) {
  results <- data.frame(variable = character(), 
                        mean_diff = numeric(), 
                        sd_diff = numeric(), 
                        method = character())
  
  for (col in names(df_original)) {
    if (is.numeric(df_original[[col]])) {
      # Calculate difference in mean and standard deviation
      orig_mean <- mean(df_original[[col]], na.rm = TRUE)
      orig_sd <- sd(df_original[[col]], na.rm = TRUE)
      
      imp_mean <- mean(df_imputed[[col]], na.rm = TRUE)
      imp_sd <- sd(df_imputed[[col]], na.rm = TRUE)
      
      # Add to results
      results <- rbind(results, data.frame(
        variable = col,
        mean_diff = abs(orig_mean - imp_mean) / orig_mean * 100, # % difference
        sd_diff = abs(orig_sd - imp_sd) / orig_sd * 100, # % difference
        method = method_name
      ))
    }
  }
  
  return(results)
}

# Calculate evaluation metrics for each method
eval_simple <- evaluate_imputation(df, df_imputed_simple, "Mean/Mode")
eval_mice <- evaluate_imputation(df, df_imputed_mice, "MICE")
eval_knn <- evaluate_imputation(df, df_imputed_knn, "KNN")

# Combine all evaluations
all_evals <- rbind(eval_simple, eval_mice, eval_knn)

# Create plots
# 1. Plot mean difference comparison
mean_diff_plot <- ggplot(all_evals, aes(x = variable, y = mean_diff, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Difference (%) Across Methods", x = "Variable", y = "Mean Difference (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Plot standard deviation difference comparison
sd_diff_plot <- ggplot(all_evals, aes(x = variable, y = sd_diff, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "SD Difference (%) Across Methods", x = "Variable", y = "SD Difference (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Density plots for comparison
plot_density_comparison <- function(variable_name) {
  if (is.numeric(df[[variable_name]])) {
    ggplot() +
      geom_density(data = df, aes(x = .data[[variable_name]], color = "Original Distribution (without NAs)"), alpha = 0.7, na.rm = TRUE) +
      geom_density(data = df_imputed_simple, aes(x = .data[[variable_name]], color = "Mean/Mode"), alpha = 0.7) +
      geom_density(data = df_imputed_mice, aes(x = .data[[variable_name]], color = "MICE"), alpha = 0.7) +
      geom_density(data = df_imputed_knn, aes(x = .data[[variable_name]], color = "KNN"), alpha = 0.7) +
      scale_color_manual(values = c("Original Distribution (without NAs)" = "black", 
                                    "Mean/Mode" = "red", 
                                    "MICE" = "blue", 
                                    "KNN" = "green")) +
      labs(title = paste("Density Plot for", variable_name), 
           x = variable_name, 
           y = "Density", 
           color = "Method") +
      theme_minimal()
  }
}

# Get only numeric columns for density plots
numeric_cols <- names(df)[sapply(df, is.numeric)]
density_plots <- lapply(numeric_cols, plot_density_comparison)

# Arrange and display plots
grid.arrange(mean_diff_plot, sd_diff_plot, ncol = 1)

# Display density plots separately for clarity
for (plot in density_plots) {
  if (!is.null(plot)) {
    print(plot)
  }
}

# 4. Create a summary table of overall performance
summary_table_alt <- aggregate(
  cbind(mean_diff, sd_diff) ~ method, 
  data = all_evals,
  FUN = function(x) mean(x, na.rm = TRUE)
)
colnames(summary_table_alt) <- c("method", "Mean_Difference", "SD_Difference")
summary_table_alt <- summary_table_alt[order(summary_table_alt$Mean_Difference), ]

print(summary_table_alt)
# 5. Correlation preservation plot
# Calculate correlations in the original and imputed data
# Using only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]
numeric_df_simple <- df_imputed_simple[, sapply(df_imputed_simple, is.numeric)]
numeric_df_mice <- df_imputed_mice[, sapply(df_imputed_mice, is.numeric)]
numeric_df_knn <- df_imputed_knn[, sapply(df_imputed_knn, is.numeric)]

# Check if there are numeric columns to calculate correlation
if (ncol(numeric_df) > 1) {
  # Calculate correlations
  cor_original <- cor(numeric_df, use = "pairwise.complete.obs")
  cor_simple <- cor(numeric_df_simple)
  cor_mice <- cor(numeric_df_mice)
  cor_knn <- cor(numeric_df_knn)
  
  # Melt correlation matrices for plotting
  melt_cor <- function(cor_matrix, method_name) {
    melted <- melt(cor_matrix)
    melted$method <- method_name
    return(melted)
  }
  
  cor_original_melted <- melt_cor(cor_original, "Original Data")
  cor_simple_melted <- melt_cor(cor_simple, "Mean/Mode")
  cor_mice_melted <- melt_cor(cor_mice, "MICE")
  cor_knn_melted <- melt_cor(cor_knn, "KNN")
  
  all_cors <- rbind(cor_original_melted, cor_simple_melted, cor_mice_melted, cor_knn_melted)
  
  # Plot correlation heatmaps
  ggplot(all_cors, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    labs(title = "Correlation Structure Preservation", x = "", y = "", fill = "Correlation") +
    theme_minimal() +
    facet_wrap(~ method) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 6. Missing data visualization
# Create a visualization of missing data patterns
if (!require("naniar")) install.packages("naniar")
library(naniar)

vis_miss(df, cluster = TRUE) +
  labs(title = "Missing Data Pattern in the Original Dataset")

# 7. Count of missing values per variable
miss_var_summary <- miss_var_summary(df)
ggplot(miss_var_summary, aes(x = reorder(variable, pct_miss), y = pct_miss)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Percentage of Missing Values by Variable", 
       x = "Variable", 
       y = "Missing Values (%)") +
  theme_minimal()
#-------------Select the best imputation model--
#Ensure required libraries are loaded
library(dplyr)
if(!require(dplyr)) {
  # Alternative approach using base R if dplyr is not available
  use_dplyr <- FALSE
} else {
  use_dplyr <- TRUE
}
# Function to select the best imputation model
select_best_imputation_model <- function(df_original, df_imputed_simple, df_imputed_mice, df_imputed_knn) {
  # Calculate evaluation metrics for each method
  evaluate_imputation <- function(df_original, df_imputed, method_name) {
    results <- data.frame(
      variable = character(), 
      mean_diff = numeric(), 
      sd_diff = numeric(),
      corr_diff = numeric(),
      method = character()
    )
    
    # Get numeric columns for calculations
    numeric_cols <- names(df_original)[sapply(df_original, is.numeric)]
    
    # Skip if no numeric columns
    if(length(numeric_cols) == 0) {
      return(data.frame(
        method = method_name,
        mean_diff_score = NA,
        sd_diff_score = NA,
        corr_diff_score = NA,
        total_score = NA
      ))
    }
    
    # Calculate correlation matrix for original data
    cor_orig <- cor(df_original[, numeric_cols], use = "pairwise.complete.obs")
    cor_imp <- cor(df_imputed[, numeric_cols])
    
    for(col in numeric_cols) {
      # Calculate difference in mean and standard deviation
      orig_mean <- mean(df_original[[col]], na.rm = TRUE)
      orig_sd <- sd(df_original[[col]], na.rm = TRUE)
      
      imp_mean <- mean(df_imputed[[col]])
      imp_sd <- sd(df_imputed[[col]])
      
      # Calculate mean absolute difference in correlations with other variables
      corr_diffs <- numeric()
      for(other_col in numeric_cols) {
        if(other_col != col) {
          orig_corr <- cor_orig[col, other_col]
          imp_corr <- cor_imp[col, other_col]
          corr_diffs <- c(corr_diffs, abs(orig_corr - imp_corr))
        }
      }
      
      mean_corr_diff <- if(length(corr_diffs) > 0) mean(corr_diffs) else 0
      
      # Add to results
      results <- rbind(results, data.frame(
        variable = col,
        mean_diff = abs(orig_mean - imp_mean) / max(abs(orig_mean), 1e-10) * 100,  # % difference
        sd_diff = abs(orig_sd - imp_sd) / max(abs(orig_sd), 1e-10) * 100,  # % difference
        corr_diff = mean_corr_diff * 100,  # Convert to percentage scale
        method = method_name
      ))
    }
    
    return(results)
  }
  
  # Evaluate each method
  eval_simple <- evaluate_imputation(df_original, df_imputed_simple, "Mean/Mode")
  eval_mice <- evaluate_imputation(df_original, df_imputed_mice, "MICE")
  eval_knn <- evaluate_imputation(df_original, df_imputed_knn, "KNN")
  
  # Combine all evaluations
  all_evals <- rbind(eval_simple, eval_mice, eval_knn)
  
  # Calculate summary statistics
  if(use_dplyr) {
    summary_stats <- all_evals %>%
      group_by(method) %>%
      summarize(
        mean_diff_score = mean(mean_diff, na.rm = TRUE),
        sd_diff_score = mean(sd_diff, na.rm = TRUE),
        corr_diff_score = mean(corr_diff, na.rm = TRUE),
        total_score = mean_diff_score + sd_diff_score + corr_diff_score
      ) %>%
      arrange(total_score)
  } else {
    # Base R alternative
    summary_stats <- aggregate(
      cbind(mean_diff, sd_diff, corr_diff) ~ method, 
      data = all_evals,
      FUN = function(x) mean(x, na.rm = TRUE)
    )
    colnames(summary_stats) <- c("method", "mean_diff_score", "sd_diff_score", "corr_diff_score")
    summary_stats$total_score <- summary_stats$mean_diff_score + 
      summary_stats$sd_diff_score + 
      summary_stats$corr_diff_score
    summary_stats <- summary_stats[order(summary_stats$total_score), ]
  }
  
  # Select the best method (lowest total score)
  best_method <- summary_stats$method[1]
  
  # Return the best model based on method name
  if(best_method == "Mean/Mode") {
    return(list(
      model = df_imputed_simple,
      method = "Mean/Mode",
      summary_stats = summary_stats
    ))
  } else if(best_method == "MICE") {
    return(list(
      model = df_imputed_mice,
      method = "MICE",
      summary_stats = summary_stats
    ))
  } else {
    return(list(
      model = df_imputed_knn,
      method = "KNN",
      summary_stats = summary_stats
    ))
  }
}

# Use the function to select the best model
best_imputation <- select_best_imputation_model(df, df_imputed_simple, df_imputed_mice, df_imputed_knn)

# Print results
cat("Best imputation method:", best_imputation$method, "\n\n")
print(best_imputation$summary_stats)

# The selected model is now available in best_imputation$model
df_best <- best_imputation$model
