library(readxl)
library(tidyverse)
library(gridExtra)

data <- read_excel("Desktop/Dataset finale studio comunicazione ispirante.xlsx")

# Get all variable names starting with 'SPCC_'
spcc_vars <- names(data)[grepl("^SPCC_", names(data))]

# Separate PRE and POST variables
spcc_pre_vars <- spcc_vars[grepl("_PRE$", spcc_vars)]
spcc_post_vars <- spcc_vars[grepl("_POST$", spcc_vars)]

# Sort the variables to ensure they are in the same order
spcc_pre_vars <- sort(spcc_pre_vars)
spcc_post_vars <- sort(spcc_post_vars)

# Initialize a list to store test results
wilcoxon_results <- list()

# Loop through each pair of variables
for (i in 1:length(spcc_pre_vars)) {
  pre_var <- spcc_pre_vars[i]
  post_var <- spcc_post_vars[i]
  
  # Extract the data for the current variable pair
  pre_data <- data[[pre_var]]
  post_data <- data[[post_var]]
  
  # Perform the Wilcoxon signed-rank test
  test_result <- wilcox.test(pre_data, post_data, paired = TRUE)
  
  # Store the result in the list
  wilcoxon_results[[paste(pre_var, "vs", post_var, sep = " ")]] <- test_result
}

#### GRAFICI ####

# Initialize a list to store plots
plot_list <- list()

# Generate Bar Plots and Store Them
for (i in 1:length(spcc_pre_vars)) {
  pre_var <- spcc_pre_vars[i]
  post_var <- spcc_post_vars[i]
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Condition = rep(c("PRE", "POST"), each = nrow(data)),
    Score = c(data[[pre_var]], data[[post_var]])
  )
  
  # Set the levels of 'Condition' factor to ensure 'PRE' comes before 'POST'
  plot_data$Condition <- factor(plot_data$Condition, levels = c("PRE", "POST"))
  
  # Calculate mean and standard error for each condition
  summary_data <- aggregate(Score ~ Condition, data = plot_data, 
                            FUN = function(x) c(Mean = mean(x, na.rm = TRUE), 
                                                SE = sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))))
  
  # Convert the aggregated data into a usable format
  summary_data <- do.call(data.frame, summary_data)
  summary_data$Mean <- summary_data$Score.Mean
  summary_data$SE <- summary_data$Score.SE
  
  # Create the bar plot
  p <- ggplot(summary_data, aes(x = Condition, y = Mean, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
    labs(title = paste(gsub("_PRE|_POST", "", pre_var)),
         x = "Condition",
         y = "Mean Score") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 12, face = "bold")) +
    scale_fill_manual(values = c("#619CFF", "#F8766D"))
  
  # Store the plot in the list
  plot_list[[i]] <- p
}

# Arrange plots in a 4 x 2 grid
grid.arrange(grobs = plot_list, nrow = 4, ncol = 2)

##### CORRELAZIONI #####

# Identify variables containing "IS_"
is_vars <- names(data)[grepl("^IS_", names(data))]

# Identify variables containing "ES_"
es_vars <- names(data)[grepl("^ES_", names(data))]

# Subset data for IS_ variables
is_data <- data[, is_vars]

# Subset data for ES_ variables
es_data <- data[, es_vars]

# Compute Spearman correlation matrix using base R cor()
cor_matrix <- cor(x = is_data, y = es_data, method = "spearman")

corrplot::corrplot(cor_matrix, method = "number",mar = c(1, 1, 1, 1))

##### ALTRE CORRELAZIONI #####

# Identify variables containing "IS_"
mps_vars <- names(data)[grepl("^MPS_", names(data))]

# Identify variables containing "ES_"
es_vars <- names(data)[grepl("^ES_", names(data))]

# Subset data for IS_ variables
mps_data <- data[, mps_vars]

# Subset data for ES_ variables
es_data <- data[, es_vars]

# Compute Spearman correlation matrix using base R cor()
cor_matrix <- cor(x = mps_data, y = es_data, method = "spearman")

corrplot::corrplot(cor_matrix, method = "number",mar = c(1, 1, 1, 1))


