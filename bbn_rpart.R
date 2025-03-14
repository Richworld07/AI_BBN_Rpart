#########Clear data########
rm(list = ls())  # Removes all objects from environment

# Clear packages

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # Mimics ctrl+L

#########
# Install packages --------------------------------------------------------
#.libPaths(c("C:/R/Library"))##library path
packages1 <- c("magrittr","caTools","corrplot","missForest","readxl","graphNEL","Rgraphviz","bnlearn","readr","missForest","bnviewer","caTools")
pkgs2inst <- !(packages1 %in% (.packages(all.available=T)))
if (any(pkgs2inst)) install.packages(packages1[pkgs2inst])
lapply(packages1, require, character.only=T)
rm(packages1,pkgs2inst)

#if (!require("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")
#BiocManager::install("Rgraphviz")

library(Rgraphviz)
# Loading Data ---------------------------------
rawdata <-read_excel("dataset.xlsx")
head(rawdata)

df <- data.frame(rawdata)

#View(df)
#Period = df$D1
##Droping the Period Column
dfd<- df[, !(names(df) %in% c("Period"))]

# Impute the missing values using m
#library(missForest)
set.seed(123)
#df_imp <- missForest(dfd)
#view(df_imp)
#------------Imputation for Missing Values-------
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

#write.csv(df_im, "cleanDataMain.csv", row.names = FALSE)

#------------Feature Scaling-------
library(caTools)
df_im<- df_best

df_s = scale(df_im)
View(df_s)
df_sdf= data.frame(df_s)
str(df_sdf)

#-----------Feature Engineering Selection------------
#Transforming the Data
#Pre-process data to better learn Bayesian networks
#Removing highly-correlated variables
  #The dedup() function takes a data frame containing (only) 
  #continuous variables and looks for pairs of variables with strong
  #correlation, regardless of the sign. It then removes one of variable 
  #in each such pair. The end goal is to avoid learning Gaussian 
  #Bayesian networks which clusters of highly-connected nodes,
  #for both speed and interpretability.
# screen continuous data for highly correlated pairs of variables.
threshold = 0.5
df_c = dedup(df_sdf, threshold, debug = FALSE)
# Visualisation the correlation matrix
{cor_matrix <- cor(df_c, use = "complete.obs")
  
  # Option 1: Using corrplot package (more customizable)
  corrplot(cor_matrix, 
           method = "circle",     # Type of plot (circle, color, number)
           type = "upper",        # Show only upper triangle
           order = "hclust",      # Order variables by hierarchical clustering
           tl.col = "black",      # Text label color
           tl.srt = 45,           # Text label rotation
           addCoef.col = "black", # Add coefficient values
           col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
           diag = FALSE)          # Hide diagonal cells
  
  # Option 2: Using ggplot2 for a heatmap style visualization
  # Reshape the correlation matrix for ggplot
  library(reshape2)
  cor_melted <- melt(cor_matrix)
  names(cor_melted) <- c("Var1", "Var2", "Correlation")
  
  # Create the heatmap
  ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "#6D9EC1", high = "#E46726", mid = "white", 
                         midpoint = 0, limit = c(-1,1), name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
    coord_fixed()
}
#--------------Discretise with Hartemink’s Method
df_sd = discretize(data.frame(df_c), method = 'hartemink', breaks = 3, ibreaks = 60,
                   idisc="quantile", debug = TRUE)
View(df_sd)
# Save the discretized data to a CSV file
#write_excel_csv(data.frame(lapply(df_sd, as.numeric)), file = "df_sd.csv", row.names = FALSE)

    #Data are first marginalised in 60 intervals, which are subsequently
    #collapsed while reducing the mutual information between the variables 
    #as little as possible,he process stops when each variable has 3 levels
    #(i.e. low, average and high)

#--------------------Learning the BBN------
##Model Averaging BBN Boot strength------------
    #The results of both structure and parameter learning are noisy
    #in most real-world settings, due to limitations in the data &
    #in our knowledge of the processes that control them.Since parameters
    # are learned conditional on the results of structure learning,it’s a good
    # idea to use model averaging to obtain a stable network structure from the 
    #data. 
#Constraint-based algorithms:Use statistical tests to
  #learn conditional independence relationships from the data.
# In score-based algorithms, each candidate network is
  #assigned a goodness-of-fit score, which we want to maximise.
#Hybrid algorithms use conditional independence tests are
  #to restrict the search space for a subsequent score-based search.

#---------Structure Learning----
library(bnlearn)
library(bnviewer)
set.seed(123)
#---- Constraint-Based Algorithms
#iamb_bn = inter.iamb(df_sd, test = "mi", B = 100, alpha = 0.01)
#print(iamb_bn)

#---- Score-Based Algorithms
#hc_bn = hc(df_sd, score = "bic", iss = 3, restart = 5, perturb = 10)
#print(hc_bn)
#tabu_bn = tabu(df_sd, tabu = 15, max.iter = 500)
#print(tabu_bn)

#strength = arc.strength(hc_bn, df_sd, criterion = "x2")
#strength.plot(hc_bn, strength)

#----- Hybrid Algorithms
# Correct way to use rsmax2
#df_sd_numeric <- data.frame(lapply(df_sd, as.numeric))
# Then try rsmax2 again
#hb_bn = rsmax2(
 # df_sd_numeric,
  #restrict = "si.hiton.pc", 
  #maximize = "tabu",
  #estrict.args = list(test = "zf", alpha = 0.01),
  #maximize.args = list(score = "bic-g")
#)

#data.frame(lapply(df_sd, as.numeric))
# boot strength 

  boot <- boot.strength(df_sd, algorithm = "hc", R = 500, 
                      algorithm.args = list(score = "loglik", iss = 10), # Changed from "bic" to "bic-g"
  debug = TRUE)

boot[(boot$strength >= 0.85) & (boot$direction >= 0.5),]

#Setting the Threshold
#Plotting the distribution of arc strengths
  #The bagging and bayes.factors encode the complete distribution 
  #of the arc strengths, since they cover all possible arcs.
  #This makes it possible to plot the empirical cumulative 
  #distribution function (ECDF) of the confidence in the arcs'
  #presence (modulo their directions) using the values stored in the
  #strength column. 
  #This is what the default plot() method does for the bn.strength
  #objects produced by boot.strength() and bf.strength().
plot(boot) 

#Plotting the averaged network
avg.boot = averaged.network(boot, threshold = 0.5)
vs = vstructs(avg.boot)
graphviz.plot(avg.boot)

strength.plot(avg.boot, boot,shape = "ellipse")
avg.boot$nodes$D1$mb
#-----Markov Blanket
#The Markov Blanket of a node is the set of nodes that 
#makes the node conditionally independent of all other nodes 
#in the network. In simpler terms, it's the minimal set of nodes 
#that "shields" D1 from the rest of the network.This means that if you know 
#the values of the nodes in the Markov Blanket, knowing the values of any other
#nodes in the network won't give you any additional information about D1.
#In this case, D1 is directly or indirectly related to a large number of other nodes.

# Get the Markov blanket for node D1
markov_blanket_vars <- avg.boot$nodes$D1$mb

# Select only those variables from your data frame
selected_data <- df_sd[, markov_blanket_vars]

# Convert to data frame if it's not already one
selected_data <- as.data.frame(selected_data)

# Optional: See the structure of the new data frame
str(selected_data)

################## --Interactive Bayesian Network Strength Viewer############
strength.viewer(avg.boot,boot,
                bayesianNetwork.background = "white",
                bayesianNetwork.arc.strength.threshold.expression = c("@threshold > 0 & @threshold < 0.5",
                                                                      "@threshold >= 0.5 & @threshold <= 0.8",
                                                                      "@threshold > 0.8 & @threshold <= 1"),
                
                bayesianNetwork.arc.strength.threshold.expression.color  = c("red","gold", "green"),
                bayesianNetwork.arc.strength.threshold.alternative.color =  "white",
                
                bayesianNetwork.arc.strength.label = TRUE,
                bayesianNetwork.arc.strength.label.prefix = "",
                bayesianNetwork.arc.strength.label.color = " white",
                
                bayesianNetwork.arc.strength.tooltip = TRUE,
                
                bayesianNetwork.edge.scale.min = 1,
                bayesianNetwork.edge.scale.max = 3,
                
                bayesianNetwork.edge.scale.label.min = 14,
                bayesianNetwork.edge.scale.label.max = 14,
                
                bayesianNetwork.width = "100%",
                bayesianNetwork.height = "800px",
                bayesianNetwork.layout = "layout.sphere",
                node.colors = list(background = "black",
                                   border = "#2b7ce9",
                                   highlight = list(background = "",
                                                    border = "#2b7ce9")),
                
                node.font = list(color = "blue", face="Open Sons"),
                edges.dashes = FALSE)












#-------------------------RPART------------
# Required libraries
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(pROC)

#Train Test Split

df_sdf <- data.frame(lapply(df_sd, as.factor))
split = sample.split(df_sdf, SplitRatio = 0.80)
training_set = subset(df_sdf, split == TRUE)
test_set = subset(df_sdf, split == FALSE)

# Create a more robust model training framework
set.seed(1234) # For reproducibility

# Define custom tuning grid for more granular control
tuning_grid <- expand.grid(
  cp = seq(0.001, 0.05, by = 0.001)  # More granular complexity parameter values
)

# Enhanced cross-validation settings
controlObject <- trainControl(
  method = "repeatedcv",       # Repeated cross-validation
  number = 5,                 # 10-fold
  classProbs = TRUE,           # Calculate class probabilities
  summaryFunction = multiClassSummary, # Detailed metrics for multiclass problems
  savePredictions = "final",   # Save predictions for further analysis
  returnResamp = "all"         # Return all resampling results
)
print(levels(training_set$D1))
# Method 1: If you want to keep the original values but make them valid for modeling
training_set$D1 <- factor(training_set$D1)
levels(training_set$D1) <- make.names(levels(training_set$D1))
test_set$D1 <- factor(test_set$D1)
levels(test_set$D1) <- make.names(levels(training_set$D1))

# Verify the new levels
print(levels(training_set$D1))

# Train the model with enhanced parameters
library(MLmetrics)
set.seed(456) # Additional seed for training stability
rpartModel <- train(
  D1 ~ .,
  data = training_set,
  method = "rpart",
  trControl = controlObject,
  tuneGrid = tuning_grid,
  metric = "Accuracy",         # Primary optimization metric
  parms = list(split = "information") # Use information gain for splits
)

# Extract the final model
fit.final <- rpartModel$finalModel

# Plot the model with enhanced visualization
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
rpart.plot(fit.final)

#Evaluate model on test set
predictions <- predict(rpartModel, newdata = test_set, type = "raw")
confusion_matrix <- confusionMatrix(predictions, test_set$D1)

#Generate variable importance plot
var_importance <- varImp(rpartModel)
plot(var_importance, top = 10, main = "Top 10 Variable Importance")

# Generate ROC curves for each class (assuming three-factor classification)
# First get class probabilities
pred_probs <- predict(rpartModel, newdata = test_set, type = "prob")

# Create list to store ROC objects
roc_list <- list()
auc_values <- numeric()

# Get class levels
class_levels <- levels(test_set$D1)

# Calculate ROC for each class (one-vs-rest approach)
for (i in 1:length(class_levels)) {
  # Create binary outcome (1 for current class, 0 for others)
  binary_outcome <- ifelse(test_set$D1 == class_levels[i], 1, 0)
  
  # Calculate ROC
  roc_list[[i]] <- roc(binary_outcome, pred_probs[, i])
  auc_values[i] <- auc(roc_list[[i]])
}

# Plot all ROC curves
par(mfrow = c(1, 1))
plot(roc_list[[1]], col = "red", main = "ROC Curves by Class")
for (i in 2:length(class_levels)) {
  plot(roc_list[[i]], col = rainbow(length(class_levels))[i], add = TRUE)
}
legend("bottomright", 
       legend = paste0(class_levels, " (AUC = ", round(auc_values, 3), ")"),
       col = rainbow(length(class_levels)), 
       lwd = 2)

# Visualize cross-validation results
ggplot(rpartModel) + 
  ggtitle("Cross-Validation Results") +
  theme_bw()

# Print detailed model performance metrics
print(confusion_matrix)
print(paste("Accuracy:", round(confusion_matrix$overall["Accuracy"], 4)))
print(paste("Kappa:", round(confusion_matrix$overall["Kappa"], 4)))

# Optional: Save model for future use
saveRDS(rpartModel, "robust_rpart_model.rds")

# Create function for predicting new data
predict_new_data <- function(new_data, model = rpartModel) {
  predictions <- predict(model, newdata = new_data, type = "prob")
  class_preds <- predict(model, newdata = new_data, type = "raw")
  
  return(list(
    class_predictions = class_preds,
    probability_predictions = predictions
  ))
}
#------------- data frame of RPART important features----
# Extract the importance scores and convert to a data frame
importance_df <- data.frame(
  Variable = rownames(var_importance$importance),
  Importance = var_importance$importance[,1],
  stringsAsFactors = FALSE)

# Sort by importance (descending)
importance_df <- importance_df[order(importance_df$Importance, 
                                     decreasing = TRUE),]
# Determine how many top variables to select
# Option 1: Select top N variables (e.g., top 10)
n_top_vars <- 10
top_vars <- importance_df$Variable[1:min(n_top_vars, nrow(importance_df))]
# Print the selected variables
print("Selected variables based on importance:")
print(top_vars)

# This assumes the base variable names are before the first parenthesis
base_vars <- unique(gsub("\\(.*$", "", top_vars))
print("Base variable names:")
print(base_vars)

# Select the base variables from the original dataframe
selected_rpartdf <- df_sd[, base_vars, drop = FALSE]

# Add the target variable if needed (assuming D1 is your target)
if("D1" %in% colnames(df_sd) && !("D1" %in% top_vars)) {
  selected_rpartdf$D1 <- df_sd$D1
}

#----------ENSEMBLE BBN and RPART Important features----

# First, identify variables in selected_rpartdf that are not in selected_data
new_vars <- setdiff(names(selected_rpartdf), names(selected_data))

# Print the new variables that will be added
print("New variables to be added:")
print(new_vars)

# If there are new variables to add, add them to selected_data
if (length(new_vars) > 0) {
  # Create the merged dataframe
  merged_data <- cbind(selected_data, selected_rpartdf[, new_vars, drop = FALSE])
  
  # Print the dimensions of the new merged dataframe
  print("Dimensions of the merged dataframe:")
  print(dim(merged_data))
  
  # Print the column names of the merged dataframe
  print("Column names in the merged dataframe:")
  print(names(merged_data))
} else {
  # If no new variables, just keep the original dataframe
  merged_data <- selected_data
  print("No new variables to add. Original dataframe unchanged.")
}

# Verify the structure of the final merged dataframe (BBN & Rpart)
str(merged_data)

# indclude the target feature D1
Ensemble_Imp_features = merged_data
str(Ensemble_Imp_features)

# Check the structure of the new data frame
str(selected_rpartdf)
## Decision Tree Plots
rpart.plot(fit,shadow.col = "gray", digits = 4, 
           fallen.leaves = TRUE,
           roundint=FALSE,type = 4, extra = 104)

# Plot the tree using rpart.plot
rpart.plot(fit, type = 4, 
           fallen.leaves = TRUE, box.palette = "Blues", 
           col = "red", nn = TRUE)




###-----------Grid Search RPART------------
library(rpart)
library(caret)

# Define the hyper parameter grid
controlObject <- trainControl(method = "repeatedcv",repeats = 5,number = 10)

# Perform grid search using cross-validation
set.seed(123)
rpartModel <- caret::train(D1 ~.,
                           data = training_set,
                           method = "rpart",
                           tuneLength = 30,
                           trControl = controlObject)

# Plot the trained rpart model
fit.final = rpartModel$finalModel
#
par(mfrow = c(1, 1))  # Or par(mfcol = c(1, 1))
rpart.plot(fit.final,extra=104,
           shadow.col = "gray")

###Perfomance valuation
#Confusion matrix
cm <- as.matrix(table(truth=test_set$D1,prediction))
cm

#-Defining Variables
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

##Accuracy Calculation
accuracy = sum(diag)/n
accuracy 
####Per-class Precision, Recall, and F-1
#Precision is defined as the fraction of correct predictions for a certain class, 
#recall is the fraction of instances of a class that were correctly predicted
# F-1 score is also commonly reported. It is defined as the harmonic mean (or a weighted average) 
#of precision and recall
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 
###Macro-Averaged matrices-performance measures
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
data.frame(macroPrecision, macroRecall, macroF1)

###Multiclass ROC
packages1 <- c("multiROC","pROC","plotROC","ROCR")
pkgs2inst <- !(packages1 %in% (.packages(all.available=T)))
if (any(pkgs2inst)) install.packages(packages1[pkgs2inst])
lapply(packages1, require, character.only=T)
rm(packages1,pkgs2inst)

auc=multiclass.roc(response=test_set$D32,predictor= factor(prediction, ordered = TRUE),
                   plot=TRUE,print.auc=TRUE,legacy.axes=TRUE,percent=TRUE, 
                   xlab="False Positive Percentage",
                   ylab="True Postive Percentage",col="#377eb8",lwd=4)

predictor= factor(prediction, ordered = TRUE)


########ENSEMBLE XGBOOST##########
library(caret)
featurePlot(x = Cleandf[, "NPL_Ratio"],y = dsurvey$D40,
            ## Add some space between the panels
            between = list(x = 1, y = 1),
            ## Add a background grid ('g') and a smoother ('smooth')
            type = c("g", "p", "smooth"))
#splitting the dataset into training and test set
library(caTools)
set.seed(123)
dataset= dsurvey
split = sample.split(dataset$D1,SplitRatio = 0.80)
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split == FALSE)

#10-fold cross-validation
controlObject <- trainControl(method = "repeatedcv",repeats = 5,number = 10)

#RPART, RandomForest ,neural networks, and SVMs were created as follows
install.packages('earth')
library(earth)
set.seed(123)
earthModel <- train(D40 ~ ., data = training_set,method = "earth",
                    tuneGrid = expand.grid(.degree = 1,.nprune = 2:25),
                    trControl = controlObject)
set.seed(123)
svmRModel <- train(D40 ~ ., data = training_set,
                    method = "svmRadial",
                    tuneLength = 15,
                    trControl = controlObject)

nnetGrid <- expand.grid(.decay = c(0.001, .01, .1),
                          .size = seq(1, 27, by = 2),
                          .bag = FALSE)
#Neural network
library(NeuralNetTools)
install.packages('NeuralNetTools')
set.seed(123)
library(nnet)
i <-names(training_set)
form <-as.formula(paste("D40 ~", paste(i[!i %in% "dep"], collapse =" + ")))
nn <-nnet.formula(form,size=10,data=training_set)
plotnet(nn)
olden(nn)
##
nnetModel <- train(D40 ~ .,
                    data = training_set,
                    method = "avNNet",
                    tuneGrid = nnetGrid,
                    linout = TRUE,
                    trace = FALSE,
                    maxit = 1000,
                    trControl = controlObject)
set.seed(123)
rpartModel <- train(D40 ~ .,
                      data = training_set,
                      method = "rpart",
                      tuneLength = 30,
                      trControl = controlObject)
set.seed(123)
rfModel <- train(D40 ~ .,
                  data = training_set,
                  method = "rf",
                  tuneLength = 10,
                  ntrees = 1000,
                  importance = TRUE,
                  trControl = controlObject)

gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                                   n.trees = seq(100, 1000, by = 50),
                                   n.minobsinnode = 10,
                                   shrinkage = c(0.01, 0.1))
set.seed(123)
gbmModel <- train(D40 ~ .,
                    data = training_set,
                    method = "gbm",
                    tuneGrid = gbmGrid,verbose=FALSE,
                    trControl = controlObject)

#resampling results these models were collected into caret’s resamples function
allResamples <- resamples(list(MARS = earthModel,
                                SVM = svmRModel,
                                CART = rpartModel,
                                "Boosted Tree" = gbmModel,
                                "Random Forest" = rfModel))
# Plot the RMSE values
library(lattice)
lattice::parallelplot(allResamples,metric = "MAE")#Rsquared & RMSE
parallelPlot(allResamples)
## Summary stats
summary(allResamples)
##Accuracy plots
dotplot(allResamples)

densityplot(allResamples,
            auto.key = list(columns = 3),
            pch = "|")
bwplot(allResamples,
       metric = "RMSE")
xyplot(allResamples,
       models = c("CART", "MARS"),
       metric = "RMSE")

##Variable importance
# pre: assumes dependent variable is the last column
library(e1071)
model <- svm(Species ~ ., data = iris)
class(model)
predictors <- caret_featureSelection(training_set, test_set, n.pred=5)
predictors
plot(varImp(object=svmRModel),   main="GBM - Variable Importance")
###Predictions
svmPredictions <- predict(svmRModel, test_set)
plot(svmPredictions,test_set$D40)

## HIGHLIGHT NODE AND ITS MARKOV BLANKET
res <- hc(dqddata)
plot(res, highlight = c("ZTotalCost_income",mb(avg.boot1 , "ZTotalCost_income")))
fitted <- bn.fit(res,dqddata)
fitted$ZTotalCost_income
nbcl = naive.bayes(dqddata, training = "ZTotalCost_income")
nbcl.trained = bn.fit(nbcl, dqddata)
nbcl.trained
##Evaluating the Naive bayse Classifier with Cross Validation
cv.nb = bn.cv(nbcl, data = dqddata, runs = 10, method = "k-fold", folds = 10)
cv.b=bn.cv(nbcl, data = trainingData, runs = 10, method = "k-fold",
      folds = 10, loss = "pred", loss.args = list(target = "ZTotalCost_income"))
nbcl
######Tree Augmented  Naive bayes Classifier
tancl = tree.bayes(dqddata, training = "ZTotalCost_income")
tancl.trained = bn.fit(tancl, dqddata)
tancl.trained
cv.tan = bn.cv("tree.bayes", data = dqddata, runs = 10, method = "k-fold",
               folds = 10, algorithm.args = list(training = "Zafsi"))
cv.tan
plot(cv.nb, cv.tan, xlab = c("NAIVE BAYES", "TAN"))

###ENSEMBLES AND CROSS vALIDATION
#extract the BNs that were tted withdrawing each fold;
kfold = bn.cv(trainingData, "hc", k = 10)
ensemble = lapply(kfold, `[[`, "fitted")
##predict each new student from each model;
pred.ensemble = sapply(ensemble, predict, node = "Zafsi",
                       data = testData, method = "bayes-lw")

single= bn.fit(hc(trainingData,score = "bic")), trainingData)
pred.single = predict(single, "Zafsi", testData, method = "bayes-lw")
pred.single


output <- table(Actual=testData$Zafsi, pred.single)
output
cm <- as.matrix(output)
####################################
###########reating training set######

levels(dqddata$ZStatutory_reserves)
levels(dqddata$Zafsi)[1]<-"low"
levels(dqddata$Zafsi)[2]<-"Median"
levels(dqddata$Zafsi)[3]<-"High"
levels(dqddata$Zafsi)
#intrain=createDataPartition(dqddata$Zafsi, p=0.80, list = FALSE)
intrain <- runif(nrow(dsurvey)) < 0.80
trainingData<-dsurvey[intrain,]
testData <- dsurvey[-intrain,]
# Pull out the dependent variable


#####Random Forest Regression####
library(randomForest)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(MASS)
library(rpart)
library(rpart.plot)
library(caret)
library(class)
library(ipred)
library(caretEnsemble)
library(MASS)
library(kernlab)
library(klaR)
library(nnet)
library(C50)
require(devtools)
require(neuralnet)
require(lubridate)
library(nnfor)
require(forecast)
install.packages("h20")
library(h2o)   
set.seed(1234)
######Caret package()
##model building
#Tune algorithm parameters using an automatic grid search
# prepare training scheme
control <-trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
#train model
set.seed(123)
rf.mode = train(Znpls_tcap~.,data = trainingData,method = "rf",importance=TRUE,ntree=500,debug=TRUE,trControl=control,tuneLength=5)
# summarize the model
print(rf.mode)
varImp(rf.mode ,type = 1)
plot(varImp(rf.mode ,type = 1))#important variables

###voting Ensembles
#Bagged CART
set.seed(917)
fit.treebag = train(afsi ~.,data=trainingData,method = "knn'",metric=metric,trControl=control)
# Random Forest
fit.rf = train(x=trainX,y=trainingData$afsi,method = "rf",metric=metric,trControl=control,importance=TRUE)
plot(fit.rf,)
# summarize results
bagging_results <-resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
##plots
dotplot(bagging_results)
#######KNN and Rpart heteroginious ensemble####
# define training control
tcontrol <-trainControl(method="cv", number=4,savePredictions=TRUE)
# train a list of models
algorithmList <-c('rf', 'rpart','svmPoly','nnet')
set.seed(1233)

models= caretList(Zafsi~Znet_int_marg+Zreer+Zpassets_gdp+Zhh_indebt+Zterm_trade+Zwballcom_ind+Ztasset_gdp
    +Ztcost_to_inc+Zdeficit_gdp+Ztcredi_gdp+Zlarg_exp_gloans+Zdom_inf+Zdom_reer+Zsa_inflation+Zcurr_acc_gdp+Zint_spread
    +Zsa_rate+Zcorpcredi_tcredit+Zgor+Zjse.asi,trControl=tcontrol,data=trainingData,methodList=algorithmList)
##
models= caretList(Znpls_tcap~.,trControl=tcontrol,data=trainingData,methodList=algorithmList)


results <-resamples(models)
summary(results)
dotplot(results)
# correlation between results
mrc=modelCor(results)
splom(results)

# Example of prediction output
pred(models, features) %>% head()
 
predict_ens<-Predict(models,testData)#ensemble prediction

# Compute the accumulated local effects for all features
library(iml)
install.packages("patchwork")
library(patchwork)
eff <- FeatureEffects$new(predict_ens)
eff$plot(ncol=2)
####Feature Importance
# Compute feature importances as the performance drop in mean absolute error
imp <- FeatureImp$new(predict_ens, loss = "mae")
plot(imp)
######NEURAL NETWORK########
set.seed(123)

ntrain <- runif(nrow(ddsurvey)) < 0.80
trainData<-ddsurvey[intrain,]#continuous
testingData <- ddsurvey[-intrain,]#continuous
nn <- neuralnet(Zafsi~Znet_int_marg+Zreer+Zpassets_gdp+Zhh_indebt+Zterm_trade+Zwballcom_ind+Ztasset_gdp
                +Ztcost_to_inc+Zdeficit_gdp+Ztcredi_gdp+Zlarg_exp_gloans+Zdom_inf+Zdom_reer+Zsa_inflation+Zcurr_acc_gdp+Zint_spread
                +Zsa_rate+Zcorpcredi_tcredit+Zgor+Zjse.asi,trainData,hidden=c(5,3),linear.output=TRUE)

predict<- predict(nn,newdata=testingData)#NN prediction
######Feature effect
# Compute the accumulated local effects for all features
install.packages("iml")
library(iml)
eff <- FeatureEffects$ne

##times series
plot(dsurvey$Zafsi,col="blue",type="l",main="Actual vs Predicted curve",lwd=2)
lines(predict,type = "l",col="red",lwd=1)

###Creating dates
head(annp)

#########FORECASTING#############
prediction<- predict(models$nnet,newdata=testData,type="raw")
prediction
##Ploting
plot(models$nnet)

# Create confusion matrix
conf=confusionMatrix(prediction,testData$Zafsi,plot=TRUE)

output <- table(Actual=testData$Zafsi, prediction)
output
cm <- as.matrix(output)
cm

##########simple Random forest######
library(randomForest)
Rfdata = data.frame(CleandScaled)
fmodel=randomForest(NPL_Ratio~.,data=df_sd ,ntree = 1000,nodesize = 7,,importance=TRUE)
varImpPlot(fmodel,type = 1)
varImp(fmodel,type = 1)
importance(fmodel)
pred <- predict(fmodel,newdata=testData,type = 'class')
Conf.afsi <- table(truth = testData$afsi,pred)


