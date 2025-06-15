#-------------------------------------------
# Load libraries
#-------------------------------------------
# install.packages("openxlsx") 
# install.packages("factoextra")
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vcd)      
library(reshape2)
library(xtable)
library(tidyr)
library(factoextra)
# Setting colors
pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
#-------------------------------------------------------------------------------
# PART 1 Empirical Data
#-------------------------------------------------------------------------------
#-------------------------------------------
# Data preparation
#-------------------------------------------
# Load dataset as empirical data
em_df <- read.csv("Amazon Customer Behavior Survey.csv", stringsAsFactors = FALSE)

str(em_df)
head(em_df)
names(em_df)

# Check NA values
colSums(is.na(em_df))

# Decide to drop Timestamp column
# The timestamp variable, representing the date and time when each response was recorded (between June 4 and June 16), was excluded from the analysis. 
# This variable reflects only the moment of survey submission and does not encode any intrinsic cultural traits or behavior-related information. Therefore, it was deemed irrelevant for constructing cultural vectors or computing similarity matrices.
em_df <- em_df %>% 
  select(-Timestamp, -Purchase_Categories, -Service_Appreciation, -Improvement_Areas)

# Two variables using the same name “Personalized_Recommendation_Frequency”
# Rename the variables
names(em_df)
names(em_df)[4] <- "Made_Purchase_Based_On_Recommendation"   
names(em_df)[16] <- "Received_Recommendation_Frequency"         
names(em_df)

# Define key variables either as nominal or ordinal
# Identify text variables and their unique values
text_vars <- names(em_df)[sapply(em_df, function(x) is.character(x) | is.factor(x))]
unique_values <- lapply(em_df[text_vars], unique)
for (var in names(unique_values)) {
  cat("\n---", var, "---\n")
  print(unique_values[[var]])
}

# Variables that need to be transferred into ordinal variables
# Purchase_Frequency, Made_Purchase_Based_On_Recommendation
# Browsing_Frequency, Search_Result_Exploration
# Add_to_Cart_Browsing，Cart_Completion_Frequency, Saveforlater_Frequency
# Review_Reliability，Review_Helpfulness，Recommendation_Helpfulness
summary(em_df$age)
# Custom binning of age based on observed distribution
em_df$age_group <- cut(em_df$age,
                       breaks = c(0, 22, 25, 30, 40, Inf),
                       labels = c("≤22", "23–25", "26–30", "31–40", "41+"),
                       right = TRUE,
                       ordered_result = TRUE)

# Convert to numeric ordinal values for similarity calculation
em_df$age_group <- as.numeric(em_df$age_group)
table(em_df$age_group)

ordinal_levels_list <- list(
  Purchase_Frequency = c("Less than once a month", "Once a month", "Few times a month", "Once a week", "Multiple times a week"),
  Made_Purchase_Based_On_Recommendation = c("No", "Sometimes", "Yes"),
  Browsing_Frequency = c("Rarely", "Few times a month", "Few times a week", "Multiple times a day"),
  Search_Result_Exploration = c("First page", "Multiple pages"),
  Add_to_Cart_Browsing = c("No", "Maybe", "Yes"),
  Cart_Completion_Frequency = c("Never", "Rarely", "Sometimes", "Often", "Always"),
  Saveforlater_Frequency = c("Never", "Rarely", "Sometimes", "Often", "Always"),
  Review_Reliability = c("Never", "Rarely", "Occasionally", "Moderately", "Heavily"),
  Review_Helpfulness = c("No", "Sometimes", "Yes"),
  Recommendation_Helpfulness = c("No", "Sometimes", "Yes")
)

for (var in names(ordinal_levels_list)) {
  em_df[[var]] <- factor(em_df[[var]], levels = ordinal_levels_list[[var]], ordered = TRUE)
  em_df[[var]] <- as.numeric(em_df[[var]])
}

summary(em_df[names(ordinal_levels_list)])
print(names(ordinal_levels_list))

# Define Ordinal variables
ordinal_vars_auto <-c("age_group", "Received_Recommendation_Frequency","Customer_Reviews_Importance","Rating_Accuracy","Shopping_Satisfaction")
ordinal_vars_manual <- c("Purchase_Frequency", "Made_Purchase_Based_On_Recommendation", "Browsing_Frequency", "Search_Result_Exploration",
                         "Add_to_Cart_Browsing", "Cart_Completion_Frequency","Saveforlater_Frequency", "Review_Reliability", 
                         "Review_Helpfulness" , "Recommendation_Helpfulness")
ordinal_vars <- c(ordinal_vars_auto, ordinal_vars_manual)
print(ordinal_vars)
# Define Nominal variables
nominal_vars <- c("Gender", "Product_Search_Method", "Review_Left", "Cart_Abandonment_Factors")

for (var in nominal_vars) {
  if (var %in% names(em_df)) {
    em_df[[var]] <- as.factor(em_df[[var]])
  }
}
em_df[nominal_vars] <- lapply(em_df[nominal_vars], as.factor)

str(em_df)
summary(em_df)

#-------------------------------------------
# Calculate Similarity Matrix
#-------------------------------------------

# N individuals
N <- nrow(em_df)
N

# Initial N x N bull similarity matrix 
similarity_matrix <- matrix(0, nrow = N, ncol = N)

# Compute the similarity for each pair of individuals (i, j)
similarity_contributions <- matrix(0, nrow = N, ncol = N)

# Ordinal Var
for (var in ordinal_vars) {
  if (var %in% names(em_df)) {
    x <- em_df[[var]]
    qk <- length(na.omit(unique(x)))
    if (qk <= 1) next
# Nominal Var    
    for (i in 1:N) {
      for (j in i:N) {
        if (!is.na(x[i]) && !is.na(x[j])) {
          sim_ij <- 1 - abs(x[i] - x[j]) / (qk - 1)
          # Store the calculated similarity for pair (i, j) and (j, i)
          similarity_contributions[i, j] <- coalesce(similarity_contributions[i, j], 0) + sim_ij
          similarity_contributions[j, i] <- similarity_contributions[i, j]
        }
      }
    }
  }
}

# Number of Features
F_total <- length(ordinal_vars) + length(nominal_vars)

# Divide by feature number
similarity_matrix <- similarity_contributions / F_total
similarity_matrix[1:5, 1:5]

#-------------------------------------------
# Decomposition
#-------------------------------------------
eigen_result <- eigen(similarity_matrix)
# Eigenvalue
emp_value <- eigen_result$values
head(eigen_result$values, 10)
# Eigenvector
emp_vector <- eigen_result$vectors

#-------------------------------------------------------------------------------
# Part 2 Restricted Randomness - Null model
#-------------------------------------------------------------------------------
#-------------------------------------------
# Simulate Restricted Randomness
#-------------------------------------------
# Parameters
# Number of individuals
N <- nrow(em_df)  
num_simulations <- 500  
F_total <- length(ordinal_vars) + length(nominal_vars)  

# Initial Matrix for eigenvalues
eigenvalue_matrix_null <- matrix(NA, nrow = num_simulations, ncol = N)

# Main loop simulation
for (sim in 1:num_simulations) {
  # Step 1: Generate a synthetic dataset by sampling based on empirical marginal distributions
  r_random_df <- data.frame(matrix(nrow = N, ncol = 0))
  
  # Generate ordinal variables (numeric)
  for (var in ordinal_vars) {
    if (var %in% names(em_df)) {
      probs <- prop.table(table(em_df[[var]]))
      # Skip if no variation
      if (length(probs) <= 1) {
        cat("Skipping variable:", var, "\n")
        next
      }
      sampled <- sample(as.numeric(names(probs)), size = N, replace = TRUE, prob = probs)
      r_random_df[[var]] <- sampled
    }
  }
  
  # Generate nominal variables (character)
  for (var in nominal_vars) {
    if (var %in% names(em_df)) {
      probs <- prop.table(table(em_df[[var]]))
      if (length(probs) <= 1) next  # Skip if no variation
      
      sampled <- sample(names(probs), size = N, replace = TRUE, prob = probs)
      r_random_df[[var]] <- sampled
    }
  }
  
  # Step 2: Build similarity matrix from generated data
  sim_contrib <- matrix(0, nrow = N, ncol = N)
  
  # Ordinal features similarity
  for (var in ordinal_vars) {
    x <- r_random_df[[var]]
    qk <- length(na.omit(unique(x)))
    if (qk <= 1) next  # Skip constant variables
    
    for (i in 1:N) {
      for (j in i:N) {
        sim_ij <- 1 - abs(x[i] - x[j]) / (qk - 1)
        sim_contrib[i, j] <- sim_contrib[i, j] + sim_ij
        sim_contrib[j, i] <- sim_contrib[i, j]
      }
    }
  }
  
  # Nominal features similarity using Kronecker delta
  for (var in nominal_vars) {
    x <- r_random_df[[var]]
    qk <- length(na.omit(unique(x)))
    if (qk <= 1) {
      cat("Skipping variable:", var, "\n")
      next
    }
    for (i in 1:N) {
      for (j in i:N) {
        sim_ij <- as.numeric(x[i] == x[j])
        sim_contrib[i, j] <- sim_contrib[i, j] + sim_ij
        sim_contrib[j, i] <- sim_contrib[i, j]
      }
    }
  }
  
  # Step 3: Normalize similarity matrix
  sim_matrix <- sim_contrib / F_total
  
  # Step 4: Eigenvalue decomposition
  eig_res <- eigen(sim_matrix, only.values = TRUE)
  eigenvalue_matrix_null[sim, ] <- eig_res$values
}

#---------------------------------------
# Part 3 Explore eigenvalue distribution 
#---------------------------------------
#---------------------------------------
# Empirical Data
#---------------------------------------
# emp_value
# emp_vector

# Visualize Eigenvalues Distribution
emp_value
head(emp_value, 20)

# Histogram
hist(emp_value, breaks = 30, 
     main = "Histogram of Eigenvalue of Empirical Data",
     xlab = "Value", col = "lightblue")

# Plot
plot(emp_value, type = "o", 
     main = "Eigenvalue Spectrum of Empirical Data", 
     xlab = "Index", ylab = "Eigenvalue")
grid()

# 2. Eigenvalue spectra of cultural similarity matrices
# Set layout for main plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))  # Reset layout

# Plot main histogram: full spectrum
hist(emp_value[-1],
     breaks = 30,
     col = "lightblue",
     border = "white",
     main = expression("Empirical Eigenvalue Spectrum"),
     xlab = expression(lambda),
     ylab = "Frequency")

# Add inset: red line plot of λ (x) vs counts (y)
par(fig = c(0.5, 0.9, 0.5, 0.9), new = TRUE)
x_range <- c(emp_value[1]-100,emp_value[1]+100)
plot(NA, NA,
     xlim = x_range,
     ylim = c(0, 2),
     xlab = expression(lambda),
     ylab = "Counts",
     cex.main = 0.9,
     cex.axis = 0.8,
     cex.lab = 0.9)

# Add the red vertical line
lines(c(emp_value[1], emp_value[1]), c(0, 1),
      col = "red", lwd = 2)
box()
par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1))

#-------------------------------------------
# Restricted Randomness
#-------------------------------------------
# Choose one of the null model
null_values <- eigenvalue_matrix_null[1, ]
lambda1_null <- null_values[1]
lambda1_null

head(null_values, 10)
# eigenvalue_matrix_null[,2]

# Main plot: eigenvalue spectrum of null model
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

hist(null_values[-1],  
     breaks = 30,
     col = "lightgray",
     border = "white",
     main = expression("Null Model Eigenvalue Spectrum"),
     xlab = expression(lambda),
     ylab = "Frequency")

# Inset
par(fig = c(0.5, 0.9, 0.5, 0.9), new = TRUE)

# Setting the same range as the empirical figure
x_range <- c(emp_value[1]-100, emp_value[1]+100)

plot(NA, NA,
     xlim = x_range,
     ylim = c(0, 2),
     xlab = expression(lambda),
     ylab = "Count",
     cex.main = 0.9,
     cex.axis = 0.8,
     cex.lab = 0.9)

# Add red vertical line
lines(c(lambda1_null, lambda1_null), c(0, 1),
      col = pal[1], lwd = 2)

box()

par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1))


# Compare histograms of eigenvalues (excluding lambda1)
hist(emp_value[-c(1,5)], breaks = 30, col = rgb(0.2,0.4,0.8,0.5),
     xlab = expression(lambda), main = "Empirical vs Null Eigenvalue Spectrum",
     xlim = range(c(0, emp_value[6])))

hist(null_values[-c(1,5)], breaks = 30, col = rgb(1,0,0,0.4), add = TRUE)

legend("topright", legend = c("Empirical", "Null Model"),
       fill = c(rgb(0.2,0.4,0.8,0.5), rgb(1,0,0,0.4)))

# Check whether the sub-leading eigenvalue is significantly deviating 
# Comparing eigenvalue distribution of empirical data 
# to the distribution of the sub-leading eigenvalues from all the random models
lambda2_null <- eigenvalue_matrix_null[,2]

# Compute histogram of null model λ₂
hist_lambda2 <- hist(lambda2_null, breaks = 30, plot = FALSE)
hist_max <- max(hist_lambda2$counts)
xlim_range <- range(c(emp_value[-1], lambda2_null))
ylim_range <- c(0, hist_max + 10)

# Start blank plot
plot(NA, xlim = xlim_range, ylim = ylim_range,
     xlab = expression(lambda[2]), ylab = "counts",
     main = "", axes = FALSE)

# --- Bottom band: histogram ---
for (i in 1:length(hist_lambda2$counts)) {
  rect(hist_lambda2$breaks[i],
       0,
       hist_lambda2$breaks[i + 1],
       hist_lambda2$counts[i],
       col = pal[4],
       border = pal[7],
       density = 20,
       angle = 45)
}

# --- Top band: red vertical lines for empirical eigenvalues ---
for (val in emp_value[-1]) {
  lines(x = c(val, val), y = c(hist_max , hist_max + 5), col = pal[5], lwd = 1)
}

# Add Dash
abline(h = hist_max + 0, col = "gray40")
abline(h = hist_max + 5, col = "gray40")

box()

legend("topright", legend = c("r-random"), fill = NA, 
       border = pal[7], density = 20, angle = 45, cex = 0.8)

# X-axis, Y-axis
axis(1)
axis(2)

# Confirm that sub-leading eigenvalue suggest group structure

#------------------------------------------------------------
# Part 4 Eigenvector Analysis - EDA
#------------------------------------------------------------
# Look into  top2_eigenvector of empirical data
# Create a copy of the original dataset
top2_eigenvector <- eigen_result$vectors[, 2]
em_df_backup <- em_df
em_df$eigen2 <- top2_eigenvector

#-------------------------------------------------------------
# Part 4.1
#-------------------------------------------------------------
# 2nd Eigenvector visualization
# Scatter plot
ggplot(em_df, aes(x = eigen2, y = 1:nrow(em_df))) +
  geom_point(size = 2, alpha = 0.7, color = pal[2]) +
  labs(
    title = "2nd Eigenvector Value per Individual",
    x = "2nd Eigenvector Value",
    y = "Individual Index"
  ) +
  theme_bw()

# Sorted Scatter Plot
sorted_vals <- sort(top2_eigenvector)
plot(sorted_vals, type='b', pch=19, cex=0.7, 
     main="Sorted Second Eigenvector Values",
     xlab="Index (sorted)", ylab="Eigenvector value")
abline(h=0, col=pal[8], lty=2)

# Eigenvector Histogram
hist(em_df$eigen2, breaks = 30, 
     col = pal[2], 
     xlab = "Entry Value",
     main = "Histogram of 2nd Eigenvector from Empirical Data")


# ----------------------------------
# Part 4.2
# ----------------------------------

# Group individuals based on values of eigenvector entry 
# mean +/- 1 deviation
mu <- mean(em_df$eigen2)
sigma <- sd(em_df$eigen2)
low_threshold <- mu - 1 * sigma
high_threshold <- mu + 1 * sigma

em_df$eigen_group <- with(em_df, ifelse(
  eigen2 <= mu - 1 * sigma, "Group 1",
  ifelse(eigen2 >= mu + 1 * sigma, "Group 2", "Group Mild")
))
table(em_df$eigen_group)

ggplot(em_df, aes(x = eigen2, fill = eigen_group)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Group 1" = pal[7], "Group Mild" = pal[8], "Group 2" = pal[3])) +
  geom_vline(xintercept = c(low_threshold, high_threshold), 
             linetype = "dashed", color = pal[5], size = 1) +
  labs(
    #title = "Histogram of 2nd Eigenvector from Empirical Data",
    #subtitle = "Dashed lines mark ±1 standard deviations",
    x = "Entry Value",
    y = "Count",
    fill = "Eigen Group"
  ) +
  theme_bw()

# Also try Quantile
#qs <- quantile(em_df$eigen2, probs = c(0.2, 0.8))
#em_df$eigen_group <- with(em_df, ifelse(
#  eigen2 <= qs[1], "Low",
#  ifelse(eigen2 >= qs[2], "High", "Mid")
#))

# Check which variables contribute significantly to the groups
# Identify variable types
ordinal_vars
nominal_vars

df_sub <- em_df %>% 
  filter(eigen_group %in% c("Group 1", "Group 2"))
table(df_sub$eigen_group)

# t-value for ordinal variables
num_var_results <- lapply(ordinal_vars, function(v) {
  group1_vals <- df_sub[[v]][df_sub$eigen_group == "Group 1"]
  group2_vals <- df_sub[[v]][df_sub$eigen_group == "Group 2"]
  
  if(length(na.omit(group1_vals)) > 2 && length(na.omit(group2_vals)) > 2){
    t_res <- t.test(group1_vals, group2_vals)
    data.frame(
      Variable = v,
      Mean_Group1 = mean(group1_vals, na.rm=TRUE),
      Mean_Group2 = mean(group2_vals, na.rm=TRUE),
      t_statistic = as.numeric(t_res$statistic),
      p_value = t_res$p.value
    )
  } else {
    NULL
  }
})
num_var_results <- do.call(rbind, num_var_results)

# chisq test for nominal/categorical variables
cat_var_results <- lapply(nominal_vars, function(v) {
  tbl <- table(df_sub[[v]], df_sub$eigen_group)
  if(all(dim(tbl) >= 2)){
    chi_res <- chisq.test(tbl)
    data.frame(
      Variable = v,
      p_value = chi_res$p.value
    )
  } else {
    NULL
  }
})

cat_var_results <- do.call(rbind, cat_var_results)

# Print significant variables with（p < 0.05）
cat("Significant numeric variables (p < 0.05):\n")
sig_num <- num_var_results %>% filter(p_value < 0.05) %>%
  mutate(Diff = Mean_Group1 - Mean_Group2) %>%
  arrange(desc(Diff))

print(sig_num)
print(xtable(sig_num), include.rownames = FALSE)

cat("\nSignificant categorical variables (p < 0.05):\n")
sig_cat <- cat_var_results %>% filter(p_value < 0.05)
print(sig_cat)
print(xtable(sig_cat), include.rownames = FALSE)

# Visualize for numerical variables
sig_num <- sig_num %>%
  arrange(desc(Diff)) %>%
  mutate(Variable = factor(Variable, levels = Variable))

long_df <- pivot_longer(sig_num,
                        cols = c("Mean_Group1", "Mean_Group2"),
                        names_to = "Group",
                        values_to = "Mean")

long_df$Group <- factor(long_df$Group,
                        labels = c("Group 1", "Group 2"))

ggplot(long_df, aes(x = Variable, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Group 1" = pal[2], "Group 2" = pal[3])) +
  coord_flip() +
  labs(title = "",
       x = "Variable", y = "Mean Score") +
  theme_bw()

# Visualize for nominal/categorical variables
# Gender
ggplot(df_sub, aes(x = eigen_group, fill = Gender)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Female" = pal[2], "Male" = pal[7], "Others" = pal[3], "Prefer not to say"= pal[4])) +
  labs(title = "", y = "Proportion", x = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


# Review_Left
ggplot(df_sub, aes(x = eigen_group, fill = Review_Left)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = pal[1:5])+
  labs(title = "", y = "Proportion", x = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


# Cart_Abandonment_Factors
ggplot(df_sub, aes(x = eigen_group, fill = Cart_Abandonment_Factors)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = pal[4:8])+
  labs(title = "", y = "Proportion", x = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

#------------------------------------------
# Part 4.3 Sort out key individuals
#------------------------------------------
top2_eigenvector

# Sorting orders
df_v2 <- data.frame(ID=1:N,
                    em_df,
                    abs_v2 = abs(top2_eigenvector))

# Sorted based on absolute values
df_v2_sorted <- df_v2[order(-df_v2$abs_v2), ]

# Identify top-k contributor
top_k <- 10
cat("Top", top_k, "individuals by absolute contribution to v2:\n")
print(head(df_v2_sorted, top_k))

# Hightlight top-k contributor in the eigenvector entries
top_ids <- df_v2_sorted$ID[1:top_k]

em_df$highlight_top <- ifelse(1:nrow(em_df) %in% top_ids, "Top", "Other")

ggplot(em_df, aes(x = eigen2, y = 1:nrow(em_df))) +
  geom_point(aes(color = highlight_top), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Top" = "red", "Other" = pal[2])) +
  labs(
    title = "2nd Eigenvector Value per Individual",
    x = "2nd Eigenvector Value",
    y = "Individual Index",
    color = "Legend"
  ) +
  theme_bw()

#---------------------------------------
# Part 5 Clustering Analysis
#---------------------------------------
ordinal_vars
nominal_vars

# Prepare Numeric Variables
#dummy_vars <- model.matrix(~ Gender + Product_Search_Method + Review_Left + Cart_Abandonment_Factors - 1, data = em_df)
clustering_data <- em_df[, ordinal_vars]

# Standardize variables
clustering_data_scaled <- scale(clustering_data)
summary(clustering_data_scaled)
sapply(as.data.frame(clustering_data_scaled), sd)

# k-means clustering
# Try different k
#------------------------------------------------------
# k = 2
#------------------------------------------------------
set.seed(123)
km_k2 <- kmeans(clustering_data_scaled, centers = 2)
em_df$group_kmeans_k2 <- factor(km_k2$cluster)
table(em_df$group_kmeans_k2,em_df$eigen_group)

em_df$group_kmeans_k2<- recode(em_df$group_kmeans_k2,
                               `1` = "Group 1",
                               `2` = "Group 2")

em_df$group_kmeans_k2 <- factor(em_df$group_kmeans_k2, 
                                levels = c("Group 1", "Group 2"))

# Visualize the grouping along eigenvector entry
ggplot(em_df, aes(x = eigen2, fill = group_kmeans_k2)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Group 1" = pal[7], "Group 2" = pal[3])) +
  geom_vline(xintercept = c(low_threshold, high_threshold), 
             linetype = "dashed", color = pal[5], size = 1) +
  labs(
    #title = "Histogram of 2nd Eigenvector from Empirical Data",
    #subtitle = "k-means",
    x = "Entry Value",
    y = "Count",
    fill = "k-means Group"
  ) +
  theme_bw()

# Compare the grouping with RMT
# Checking un-matching
table(em_df$group_kmeans_k2, em_df$eigen_group)
em_df$group_combo_k2 <- interaction(em_df$eigen_group, em_df$group_kmeans_k2, sep = " ∩ ")
combo_levels_k2 <- levels(em_df$group_combo_k2)
combo_levels_k2

# Highlight their positions in the value histogram
combo_colors_k2 <- setNames(
  pal[1:6],
  combo_levels_k2
)

ggplot(em_df, aes(x = eigen2, fill = group_combo_k2)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.85) +
  scale_fill_manual(values = combo_colors_k2) +
  labs(
    title = "",
    x = "2nd Eigenvector Entry Value",
    y = "Count",
    fill = "Group Combination"
  ) +
  theme_minimal()

# # Setting colors
fill_colors_k2 <- c("Group 1" = "#1f78b4", "Group 2" = "#33a02c", "Group Mild" = "grey70")
stroke_colors_k2 <- c("Group 1" = "#1f78b4", "Group 2" = "#33a02c", "Group Mild" = "grey20")
# Plot
ggplot(em_df, aes(x = eigen2, y = 1:nrow(em_df))) +
  geom_point(aes(fill = eigen_group), shape = 21, size = 3, alpha = 0.85, color = "black") +
  geom_point(aes(color = group_kmeans_k2), shape = 1, size = 3, stroke = 1.1) +
  scale_fill_manual(values = fill_colors_k2, name = "RMT Group") +
  scale_color_manual(values = stroke_colors_k2, name = "K-means Group") +
  labs(
    title = "",
    x = "2nd Eigenvector Entry",
    y = "Respondent Index"
  ) +
  theme_minimal()

# Evaluate significant difference cross k-means groups 
# k = 2
df_sub_k2 <- em_df %>% 
  filter(group_kmeans_k2 %in% c("Group 1", "Group 2"))
table(df_sub_k2$group_kmeans_k2)

# t-value for numeric variables
num_var_results <- lapply(ordinal_vars, function(v) {
  group1_vals <- df_sub_k2[[v]][df_sub_k2$group_kmeans_k2 == "Group 1"]
  group2_vals <- df_sub_k2[[v]][df_sub_k2$group_kmeans_k2 == "Group 2"]
  
  if(length(na.omit(group1_vals)) > 2 && length(na.omit(group2_vals)) > 2){
    t_res <- t.test(group1_vals, group2_vals)
    data.frame(
      Variable = v,
      Mean_Group1 = mean(group1_vals, na.rm=TRUE),
      Mean_Group2 = mean(group2_vals, na.rm=TRUE),
      t_statistic = as.numeric(t_res$statistic),
      p_value = t_res$p.value
    )
  } else {
    NULL
  }
})
num_var_results <- do.call(rbind, num_var_results)

# chisq test \for categorial variables
cat_var_results <- lapply(nominal_vars, function(v) {
  tbl <- table(df_sub_k2[[v]], df_sub_k2$group_kmeans_k2)
  if(all(dim(tbl) >= 2)){
    chi_res <- chisq.test(tbl)
    data.frame(
      Variable = v,
      p_value = chi_res$p.value
    )
  } else {
    NULL
  }
})

cat_var_results <- do.call(rbind, cat_var_results)

# Print significant variables with（p < 0.05）
cat("Significant numeric variables (p < 0.05):\n")
sig_num_k2 <- num_var_results %>% filter(p_value < 0.05) %>%
  mutate(Diff = Mean_Group1 - Mean_Group2) %>%
  arrange(desc(Diff))

print(sig_num_k2)
print(xtable(sig_num_k2), include.rownames = FALSE)

cat("\nSignificant categorical variables (p < 0.05):\n")
sig_cat_k2 <- cat_var_results %>% filter(p_value < 0.05)
print(sig_cat_k2)
print(xtable(sig_cat_k2), include.rownames = FALSE)

#------------------------------------------------------
# k = 3 -----------------------------------------------
#------------------------------------------------------
set.seed(123)
km_k3 <- kmeans(clustering_data_scaled, centers = 3)
em_df$group_kmeans_k3 <- factor(km_k3$cluster)
table(em_df$group_kmeans_k3)

em_df$group_kmeans_k3<- recode(em_df$group_kmeans_k3,
                             `1` = "Group 1",
                             `2` = "Group Mild",
                             `3` = "Group 2")

em_df$group_kmeans_k3 <- factor(em_df$group_kmeans_k3, 
                               levels = c("Group 1", "Group 2", "Group Mild"))

ggplot(em_df, aes(x = eigen2, fill = group_kmeans_k3)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Group 1" = pal[7], "Group Mild" = pal[8], "Group 2" = pal[3])) +
  geom_vline(xintercept = c(low_threshold, high_threshold), 
             linetype = "dashed", color = pal[5], size = 1) +
  labs(
    #title = "Histogram of 2nd Eigenvector from Empirical Data",
    #subtitle = "k-means",
    x = "Entry Value",
    y = "Count",
    fill = "k-means Group"
  ) +
  theme_bw()

# Choosing optimal k
# Method 1 Elbow Method, using Within-cluster Sum of Squares, WSS
wss <- numeric(20)
set.seed(123)
for (k in 1:20) {
  wss[k] <- sum(kmeans(clustering_data_scaled, centers = k, nstart = 20)$withinss)
}

# Elbow plot
plot(1:20, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K", ylab = "Total within-clusters sum of squares",
     main = "Elbow Method For Optimal K")

# Method 2 Silhouette Method, using Silhouette Score
fviz_nbclust(clustering_data_scaled, kmeans, method = "silhouette") +
  labs(title = "Optimal Clusters - Silhouette Method")
# Based on the result, k = 2 is the optimal k

# Visualize the k-means group distribution
em_df$group_kmeans_k3<- recode(em_df$group_kmeans_k3,
                             `1` = "Group 1",
                             `2` = "Group Mild",
                             `3` = "Group 2")

em_df$group_kmeans_k3 <- factor(em_df$group_kmeans_k3, 
                               levels = c("Group 1", "Group 2", "Group Mild"))

ggplot(em_df, aes(x = eigen2, fill = group_kmeans_k3)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Group 1" = pal[7], "Group Mild" = pal[8], "Group 2" = pal[3])) +
  geom_vline(xintercept = c(low_threshold, high_threshold), 
             linetype = "dashed", color = pal[5], size = 1) +
  labs(
    #title = "Histogram of 2nd Eigenvector from Empirical Data",
    #subtitle = "k-means",
    x = "Entry Value",
    y = "Count",
    fill = "k-means Group"
  ) +
  theme_bw()

# Checking un-matching groupings between RMT vs k-means
table(em_df$eigen_group)
table(em_df$group_kmeans_k3, em_df$eigen_group)
# Highlight their positions in the value histogram
combo_levels <- levels(em_df$group_combo)
fill_colors <- setNames(
  rep(pal[8], length(combo_levels)),
  combo_levels
)
fill_colors["Group 1.Group 2"] <- pal[1] 
fill_colors["Group 1.Group Mild"] <- pal[2]  
fill_colors["Group 2.Group 1"] <- pal[3] 
fill_colors["Group 2.Group Mild"] <- pal[4]  
fill_colors["Group Mild.Group 1"] <- pal[5]  
fill_colors["Group Mild.Group 2"] <- pal[6]  

ggplot(em_df, aes(x = eigen2, fill = interaction(eigen_group, group_kmeans_k3))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_manual(values = fill_colors) +
  geom_vline(xintercept = c(low_threshold, high_threshold), 
             linetype = "dashed", color = pal[5], size = 1) +
  labs(
    title = "",
    x = "2nd Eigenvector Entry Value",
    y = "Count",
    fill = "Group Combination"
  ) +
  theme_bw()

# # Setting colors
fill_colors <- c("Group 1" = "#1f78b4", "Group 2" = "#33a02c", "Group Mild" = "grey70")
stroke_colors <- c("Group 1" = "#1f78b4", "Group 2" = "#33a02c", "Group Mild" = "grey20")

# Plot
ggplot(em_df, aes(x = eigen2, y = 1:nrow(em_df))) +
  geom_point(aes(fill = eigen_group), 
             shape = 21, size = 3, alpha = 0.9, color = "black") +
  geom_point(aes(color = group_kmeans_k3), 
             shape = 1, size = 3, stroke = 1.2) +
  scale_fill_manual(values = fill_colors, name = "Eigen Group") +
  scale_color_manual(values = stroke_colors, name = "K-means Group") +
  labs(
    title = "",
    x = "2nd Eigenvector Entry Value",
    y = "Individual Index"
  ) +
  theme_bw()


# Evaluate significant difference cross k-means groups 
# k = 3
df_sub_k3 <- em_df %>% 
  filter(group_kmeans_k3 %in% c("Group 1", "Group 2"))
table(df_sub_k3$group_kmeans_k3)

# t-value for numeric variables
num_var_results <- lapply(ordinal_vars, function(v) {
  group1_vals <- df_sub_k3[[v]][df_sub_k3$group_kmeans_k3 == "Group 1"]
  group2_vals <- df_sub_k3[[v]][df_sub_k3$group_kmeans_k3 == "Group 2"]
  
  if(length(na.omit(group1_vals)) > 2 && length(na.omit(group2_vals)) > 2){
    t_res <- t.test(group1_vals, group2_vals)
    data.frame(
      Variable = v,
      Mean_Group1 = mean(group1_vals, na.rm=TRUE),
      Mean_Group2 = mean(group2_vals, na.rm=TRUE),
      t_statistic = as.numeric(t_res$statistic),
      p_value = t_res$p.value
    )
  } else {
    NULL
  }
})
num_var_results <- do.call(rbind, num_var_results)

# chisq test \for categorial variables
cat_var_results <- lapply(nominal_vars, function(v) {
  tbl <- table(df_sub_k3[[v]], df_sub_k3$group_kmeans_k3)
  if(all(dim(tbl) >= 2)){
    chi_res <- chisq.test(tbl)
    data.frame(
      Variable = v,
      p_value = chi_res$p.value
    )
  } else {
    NULL
  }
})

cat_var_results <- do.call(rbind, cat_var_results)

# Print significant variables with（p < 0.05）
cat("Significant numeric variables (p < 0.05):\n")
sig_num_k3 <- num_var_results %>% filter(p_value < 0.05) %>%
  mutate(Diff = Mean_Group1 - Mean_Group2) %>%
  arrange(desc(Diff))

print(sig_num_k3)
print(xtable(sig_num_k3), include.rownames = FALSE)

cat("\nSignificant categorical variables (p < 0.05):\n")
sig_cat_k3 <- cat_var_results %>% filter(p_value < 0.05)
print(sig_cat_k3)
print(xtable(sig_cat_k3), include.rownames = FALSE)