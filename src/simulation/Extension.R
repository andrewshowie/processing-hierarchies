# Load necessary libraries
library(lme4)        
library(lmerTest)    
library(readxl)      
library(dplyr)       
library(ggplot2)     
library(tidyr)       
library(effectsize)  
library(performance) 
library(psych)       
library(MVN)         
library(car)         
library(datawizard)  
library(simr)
library(pwr)
library(effectsize)
library(gt)
library(parallel)

# Data loading functions 
load_similarity_data <- function(file_path, range = "B1:GW30") {
  print(paste("Loading data from:", file_path))
  data <- read_excel(file_path, range = range, col_names = FALSE, na = "")
  print(paste("Loaded matrix with dimensions:", nrow(data), "x", ncol(data)))
  return(as.matrix(data))
}

load_proficiency_data <- function(file_path, range = "H1:GW1") {
  print("Loading proficiency data")
  data <- read_excel(file_path, range = range, col_names = FALSE, na = "")
  prof_data <- as.numeric(data)
  print(paste("Loaded", length(prof_data), "proficiency scores"))
  return(prof_data)
}

# Data loading function 
load_all_data <- function() {
  print("=== Starting Data Loading Process ===")
  
  # Load similarity measures
  poly_data <- load_similarity_data("poly.xlsx")
  hierarchical_data <- load_similarity_data("hierarchical.xlsx")
  linear_data <- load_similarity_data("linear.xlsx")
  wer_data <- load_similarity_data("wer.xlsx")
  semantic_data <- load_similarity_data("semantic.xlsx")
  
  # Load proficiency data
  proficiency_data <- load_proficiency_data("proficiency.xlsx")
  
  # Define item lengths
  item_lengths <- c(8,8,9,9,10,11,11,12,12,12,13,13,14,14,14,15,15,16,16,16,
                    17,17,17,17,17,18,18,19,19,19)
  
  # Get dimensions
  n_items <- nrow(poly_data)  # Should be 30
  n_participants <- ncol(poly_data)  # Total number of participants
  n_l1 <- 6  # Number of L1 participants
  n_l2 <- n_participants - n_l1  # Number of L2 participants
  
  # Create group indicator and proficiency vector
  group <- c(rep(0, n_l1), rep(1, n_l2))  # 0 for L1, 1 for L2
  proficiency_combined <- c(rep(NA, n_l1), proficiency_data)
  
  # Create the data frame with all raw measures
  data_combined <- data.frame(
    WER = as.vector(wer_data),
    Polynomial = as.vector(poly_data),
    Hierarchical = as.vector(hierarchical_data),
    Linear = as.vector(linear_data),
    Semantic = as.vector(semantic_data),
    group = rep(group, each = n_items),
    proficiency = rep(proficiency_combined, each = n_items),
    item_length = rep(item_lengths, times = n_participants),
    item_id = rep(1:n_items, times = n_participants),
    participant_id = rep(1:n_participants, each = n_items)
  )
  
  # Center item_length
  data_combined$item_length_c <- scale(data_combined$item_length, scale = FALSE)
  
  return(data_combined)
}

# Function to fit mixed effects models 
fit_mixed_models <- function(data) {
  measures <- c("WER", "Polynomial", "Linear", "Hierarchical", "Semantic")
  models <- list()
  
  for(measure in measures) {
    print(paste("\nFitting model for", measure))
    
    # Group comparison formula
    formula_group <- as.formula(paste(
      measure, "~ group * item_length_c + (1|participant_id) + (1|item_id)"
    ))
    
    # Proficiency formula
    formula_prof <- as.formula(paste(
      measure, "~ proficiency * item_length_c + (1|participant_id) + (1|item_id)"
    ))
    
    # Fit models
    models[[paste0(measure, "_group")]] <- lmer(formula_group, data = data, REML = TRUE)
    models[[paste0(measure, "_prof")]] <- lmer(formula_prof, data = data[data$group == 1,], REML = TRUE)
    
    # Print summaries
    print(paste("\nResults for", measure, "- Group comparison:"))
    print(summary(models[[paste0(measure, "_group")]]))
    print(paste("\nResults for", measure, "- Proficiency effects (L2 only):"))
    print(summary(models[[paste0(measure, "_prof")]]))
  }
  
  return(models)
}

# Plotting function
plot_mixed_effects <- function(models, data) {
  plots <- list()
  measures <- c("WER", "Polynomial", "Linear", "Hierarchical", "Semantic")
  
  for(measure in measures) {
    # Group comparison plot
    plots[[paste0(measure, "_group")]] <- ggplot(data, aes(x = item_length, 
                                                           y = .data[[measure]], 
                                                           color = factor(group))) +
      geom_smooth(method = "lm") +
      geom_point(alpha = 0.2) +
      labs(title = paste(measure, "by Group and Item Length"),
           x = "Item Length",
           y = measure,
           color = "Group") +
      scale_color_discrete(labels = c("L1", "L2")) +
      theme_minimal()
    
    # Proficiency plot (L2 only)
    l2_data <- data[data$group == 1,]
    plots[[paste0(measure, "_prof")]] <- ggplot(l2_data, aes(x = proficiency, 
                                                             y = .data[[measure]],
                                                             color = item_length)) +
      geom_point(alpha = 0.2) +
      geom_smooth(method = "lm") +
      labs(title = paste(measure, "by Proficiency and Item Length (L2 only)"),
           x = "Proficiency",
           y = measure,
           color = "Item Length") +
      theme_minimal()
  }
  
  return(plots)
}

# Run analysis
print("=== Running Analysis ===")
data <- load_all_data()
models <- fit_mixed_models(data)

# Extract necessary variables
item_lengths <- unique(data$item_length)
original_l2_proficiency <- unique(data$proficiency[!is.na(data$proficiency)])

# Create scenario dataset
n_l1_new <- 25
n_l2_new <- 40
scenario_data <- create_scenario_data(
  n_l1 = n_l1_new,
  n_l2 = n_l2_new,
  item_lengths = item_lengths,
  original_l2_proficiency = original_l2_proficiency
)

# ----------------------
# PARAMETRIC BOOTSTRAPPING - Original Bootstrap
# ----------------------

# Function to simulate data based on a fitted model
simulate_original_bootstrap_data <- function(model, model_name, data) {
  # Extract fixed effects
  fixed_effects <- fixef(model)
  
  # Extract random effects variances
  vc <- VarCorr(model)
  var_participant <- as.numeric(vc$participant_id[1])
  var_item <- as.numeric(vc$item_id[1])
  
  # Extract residual variance
  res_var <- sigma(model)^2
  
  # Number of participants and items
  participants <- unique(data$participant_id)
  items <- unique(data$item_id)
  
  # Simulate new random intercepts
  participant_effects <- rnorm(length(participants), 0, sqrt(var_participant))
  item_effects <- rnorm(length(items), 0, sqrt(var_item))
  
  # Create a copy of the data
  sim_data <- data
  
  # Assign random effects
  sim_data$participant_effect <- participant_effects[sim_data$participant_id]
  sim_data$item_effect <- item_effects[sim_data$item_id]
  
  # Simulate residuals
  sim_data$residual <- rnorm(nrow(sim_data), 0, sqrt(res_var))
  
  # Create a new simulated outcome variable
  measure_name <- gsub("_group|_prof", "", model_name)
  sim_var_name <- paste0(measure_name, "_sim")
  
  if (grepl("_prof", model_name)) {
    # For proficiency models: simulate only for L2 participants
    sim_data[[sim_var_name]] <- NA
    l2_indices <- which(sim_data$group == 1)
    
    linpred <- fixed_effects["(Intercept)"] +
      fixed_effects["proficiency"] * sim_data$proficiency[l2_indices] +
      fixed_effects["item_length_c"] * sim_data$item_length_c[l2_indices] +
      fixed_effects["proficiency:item_length_c"] * sim_data$proficiency[l2_indices] * sim_data$item_length_c[l2_indices] +
      sim_data$participant_effect[l2_indices] +
      sim_data$item_effect[l2_indices] +
      sim_data$residual[l2_indices]
    
    sim_data[[sim_var_name]][l2_indices] <- linpred
    
  } else {
    # For group models: simulate for all participants
    linpred <- fixed_effects["(Intercept)"] +
      fixed_effects["group"] * sim_data$group +
      fixed_effects["item_length_c"] * sim_data$item_length_c +
      fixed_effects["group:item_length_c"] * sim_data$group * sim_data$item_length_c +
      sim_data$participant_effect +
      sim_data$item_effect +
      sim_data$residual
    
    sim_data[[sim_var_name]] <- linpred
  }
  
  return(sim_data)
}

# Function to re-fit the model on simulated data
analyze_original_bootstrap_sim_data <- function(sim_data, model_name, original_formula) {
  measure_name <- gsub("_group|_prof", "", model_name)
  sim_var_name <- paste0(measure_name, "_sim")
  
  # Update the formula to use the simulated outcome variable
  sim_formula <- update(original_formula, as.formula(paste(sim_var_name, "~ .")))
  
  # For proficiency models, subset to L2 participants
  if (grepl("_prof", model_name)) {
    sim_data_subset <- sim_data[sim_data$group == 1, ]
  } else {
    sim_data_subset <- sim_data
  }
  
  # Fit the model on the simulated data
  sim_model <- tryCatch(
    lmer(sim_formula, data = sim_data_subset, REML = TRUE),
    error = function(e) NULL
  )
  
  # Return fixed effects for analysis
  if (!is.null(sim_model)) {
    return(fixef(sim_model))
  } else {
    return(rep(NA, length(fixef(model))))
  }
}

# Perform the original bootstrap
n_simulations <- 1000  # Adjust as needed
bootstrap_results_original <- list()

for (model_name in names(models)) {
  cat("Bootstrapping model (Original):", model_name, "\n")
  
  # Extract original formula from the fitted model
  original_formula <- formula(models[[model_name]])
  
  # Initialize a matrix to store bootstrap estimates
  coef_names <- names(fixef(models[[model_name]]))
  bootstrap_matrix <- matrix(NA, nrow = length(coef_names), ncol = n_simulations)
  rownames(bootstrap_matrix) <- coef_names
  
  for (i in 1:n_simulations) {
    if (i %% 100 == 0) cat("  Simulation", i, "\n")  # Progress indicator
    
    # 1. Simulate new outcome variable
    sim_data <- simulate_original_bootstrap_data(models[[model_name]], model_name, data)
    
    # 2. Refit the model on the simulated data
    coefs <- analyze_original_bootstrap_sim_data(sim_data, model_name, original_formula)
    
    # 3. Store the coefficients
    bootstrap_matrix[, i] <- coefs
  }
  
  bootstrap_results_original[[model_name]] <- bootstrap_matrix
}


# ----------------------
# PARAMETRIC BOOTSTRAPPING - Scenario Bootstrap
# ----------------------

# Function to simulate data based on a fitted model and scenario dataset
simulate_scenario_bootstrap_data <- function(model, model_name, scenario_data) {
  # Extract fixed effects
  fixed_effects <- fixef(model)
  
  # Extract random effects variances
  vc <- VarCorr(model)
  var_participant <- as.numeric(vc$participant_id[1])
  var_item <- as.numeric(vc$item_id[1])
  
  # Extract residual variance
  res_var <- sigma(model)^2
  
  # Number of participants & items
  participants <- unique(scenario_data$participant_id)
  items <- unique(scenario_data$item_id)
  
  # Simulate new random intercepts
  participant_effects <- rnorm(length(participants), 0, sqrt(var_participant))
  item_effects <- rnorm(length(items), 0, sqrt(var_item))
  
  # Assign random effects
  scenario_data$participant_effect <- participant_effects[scenario_data$participant_id]
  scenario_data$item_effect <- item_effects[scenario_data$item_id]
  
  # Simulate residuals
  scenario_data$residual <- rnorm(nrow(scenario_data), 0, sqrt(res_var))
  
  # Create a new simulated outcome variable
  measure_name <- gsub("_group|_prof", "", model_name)
  sim_var_name <- paste0(measure_name, "_sim")
  
  if (grepl("_prof", model_name)) {
    # For proficiency models: simulate only for L2 participants
    scenario_data[[sim_var_name]] <- NA
    l2_indices <- which(scenario_data$group == 1)
    
    linpred <- fixed_effects["(Intercept)"] +
      fixed_effects["proficiency"] * scenario_data$proficiency[l2_indices] +
      fixed_effects["item_length_c"] * scenario_data$item_length_c[l2_indices] +
      fixed_effects["proficiency:item_length_c"] * scenario_data$proficiency[l2_indices] * scenario_data$item_length_c[l2_indices] +
      scenario_data$participant_effect[l2_indices] +
      scenario_data$item_effect[l2_indices] +
      scenario_data$residual[l2_indices]
    
    scenario_data[[sim_var_name]][l2_indices] <- linpred
    
  } else {
    # For group models: simulate for all participants
    linpred <- fixed_effects["(Intercept)"] +
      fixed_effects["group"] * scenario_data$group +
      fixed_effects["item_length_c"] * scenario_data$item_length_c +
      fixed_effects["group:item_length_c"] * scenario_data$group * scenario_data$item_length_c +
      scenario_data$participant_effect +
      scenario_data$item_effect +
      scenario_data$residual
    
    scenario_data[[sim_var_name]] <- linpred
  }
  
  return(scenario_data)
}

# Function to re-fit the model on simulated scenario data
analyze_scenario_bootstrap_sim_data <- function(sim_data, model_name, original_formula) {
  measure_name <- gsub("_group|_prof", "", model_name)
  sim_var_name <- paste0(measure_name, "_sim")
  
  # Update the formula to use the simulated outcome variable
  sim_formula <- update(original_formula, as.formula(paste(sim_var_name, "~ .")))
  
  # For proficiency models, subset to L2 participants
  if (grepl("_prof", model_name)) {
    sim_data_subset <- sim_data[sim_data$group == 1, ]
  } else {
    sim_data_subset <- sim_data
  }
  
  # Fit the model on the simulated data
  sim_model <- tryCatch(
    lmer(sim_formula, data = sim_data_subset, REML = TRUE),
    error = function(e) NULL
  )
  
  # Return fixed effects for analysis
  if (!is.null(sim_model)) {
    return(fixef(sim_model))
  } else {
    return(rep(NA, length(fixef(model))))
  }
}

# Perform the scenario bootstrap
bootstrap_results_scenario <- list()

for (model_name in names(models)) {
  cat("Bootstrapping model (Scenario):", model_name, "\n")
  
  # Extract original formula from the fitted model
  original_formula <- formula(models[[model_name]])
  
  # Define measure name (e.g., "WER")
  measure_name <- gsub("_group|_prof", "", model_name)
  
  # Initialize a matrix to store bootstrap estimates
  coef_names <- names(fixef(models[[model_name]]))
  bootstrap_matrix <- matrix(NA, nrow = length(coef_names), ncol = n_simulations)
  rownames(bootstrap_matrix) <- coef_names
  
  for (i in 1:n_simulations) {
    if (i %% 100 == 0) cat("  Simulation", i, "\n")  # Progress indicator
    
    # 1. Simulate new outcome variable
    sim_data <- simulate_scenario_bootstrap_data(models[[model_name]], model_name, scenario_data)
    
    # 2. Refit the model on the simulated data
    coefs <- analyze_scenario_bootstrap_sim_data(sim_data, model_name, original_formula)
    
    # 3. Store the coefficients
    bootstrap_matrix[, i] <- coefs
  }
  
  bootstrap_results_scenario[[model_name]] <- bootstrap_matrix
}


# ----------------------
# COMPARING MODELS
# ----------------------

# Organize Results 
comparison_data <- list()

for (model_name in names(models)) {
  measure_name <- gsub("_group|_prof", "", model_name)
  
  # Original model coefficients
  orig_coef <- fixef(models[[model_name]])
  
  # Original bootstrap coefficients
  boot_orig <- bootstrap_results_original[[model_name]]
  boot_orig_mean <- apply(boot_orig, 1, mean, na.rm = TRUE)
  boot_orig_ci <- apply(boot_orig, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Scenario bootstrap coefficients
  boot_scen <- bootstrap_results_scenario[[model_name]]
  boot_scen_mean <- apply(boot_scen, 1, mean, na.rm = TRUE)
  boot_scen_ci <- apply(boot_scen, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Combine into a dataframe
  df_orig <- data.frame(
    Measure = measure_name,
    Method = "Original Model",
    Coefficient = names(orig_coef),
    Estimate = orig_coef,
    CI_lower = NA,
    CI_upper = NA
  )
  
  df_boot_orig <- data.frame(
    Measure = measure_name,
    Method = "Original Bootstrap",
    Coefficient = names(boot_orig_mean),
    Estimate = boot_orig_mean,
    CI_lower = boot_orig_ci[1, ],
    CI_upper = boot_orig_ci[2, ]
  )
  
  df_boot_scen <- data.frame(
    Measure = measure_name,
    Method = "Scenario Bootstrap",
    Coefficient = names(boot_scen_mean),
    Estimate = boot_scen_mean,
    CI_lower = boot_scen_ci[1, ],
    CI_upper = boot_scen_ci[2, ]
  )
  
  # Combine all into one dataframe
  comparison_df <- bind_rows(df_orig, df_boot_orig, df_boot_scen)
  
  # Append to the list
  comparison_data[[model_name]] <- comparison_df
}

# Combine all measures into a single dataframe
comparison_all <- bind_rows(comparison_data)

# View the combined comparison data
print(comparison_all)

#----------------------
# VISUALIZATIONS OF COMPARISONS
#----------------------

# Function to create comparison plot across all three approaches
plot_bootstrap_comparisons <- function(data, models, bootstrap_results_original, bootstrap_results_scenario) {
  # Calculate standardized effects for original models
  get_std_effect <- function(model) {
    sum_model <- summary(model)
    group_effect <- sum_model$coefficients["group", "Estimate"]
    group_se <- sum_model$coefficients["group", "Std. Error"]
    
    var_comps <- as.data.frame(VarCorr(model))
    total_var <- sum(var_comps$vcov) + sigma(model)^2
    
    std_effect <- group_effect / sqrt(total_var)
    std_se <- group_se / sqrt(total_var)
    
    return(c(std_effect, std_se))
  }
  
  # Get original model effects
  original_effects <- sapply(measures, function(m) {
    get_std_effect(models[[paste0(m, "_group")]])
  })
  
  # Process bootstrap results
  process_bootstrap_results <- function(bootstrap_results) {
    sapply(measures, function(m) {
      boot_matrix <- bootstrap_results[[paste0(m, "_group")]]
      effect_samples <- boot_matrix["group",]
      c(mean(effect_samples), sd(effect_samples))
    })
  }
  
  original_boot_effects <- process_bootstrap_results(bootstrap_results_original)
  scenario_boot_effects <- process_bootstrap_results(bootstrap_results_scenario)
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Measure = factor(measures, 
                     levels = c("WER", "Linear", "Polynomial", "Hierarchical", "Semantic"),
                     labels = c("Word Form", "Linear Syntax", "Polynomial", "Hierarchical", "Semantic")),
    Original_Effect = original_effects[1,],
    Original_SE = original_effects[2,],
    Bootstrap_Effect = original_boot_effects[1,],
    Bootstrap_SE = original_boot_effects[2,],
    Scenario_Effect = scenario_boot_effects[1,],
    Scenario_SE = scenario_boot_effects[2,]
  )
  
  # Create the comparison plot
  ggplot(plot_data) +
    geom_pointrange(aes(x = Measure, y = Original_Effect,
                        ymin = Original_Effect - 1.96*Original_SE,
                        ymax = Original_Effect + 1.96*Original_SE,
                        color = "Original Model"),
                    position = position_dodge(width = 0.5)) +
    geom_pointrange(aes(x = Measure, y = Bootstrap_Effect,
                        ymin = Bootstrap_Effect - 1.96*Bootstrap_SE,
                        ymax = Bootstrap_Effect + 1.96*Bootstrap_SE,
                        color = "Parametric Bootstrap"),
                    position = position_dodge(width = 0.5)) +
    geom_pointrange(aes(x = Measure, y = Scenario_Effect,
                        ymin = Scenario_Effect - 1.96*Scenario_SE,
                        ymax = Scenario_Effect + 1.96*Scenario_SE,
                        color = "Scenario Bootstrap"),
                    position = position_dodge(width = 0.5)) +
    coord_flip() +
    scale_color_manual(values = c("Original Model" = "#8884d8",
                                  "Parametric Bootstrap" = "#82ca9d",
                                  "Scenario Bootstrap" = "#ffc658")) +
    labs(title = "Comparison of Model Estimates Across Processing Levels",
         y = "Standardized Effect Size (L2 vs L1)",
         x = "Processing Level",
         color = "Model Type") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank())
}

# Create a similar function for proficiency effects
plot_proficiency_comparisons <- function(data, models, bootstrap_results_original, bootstrap_results_scenario) {
  # Similar structure to above, but for proficiency effects
  get_std_prof_effect <- function(model) {
    coef <- coef(summary(model))["proficiency", "Estimate"]
    se <- coef(summary(model))["proficiency", "Std. Error"]
    
    var_comps <- as.data.frame(VarCorr(model))
    total_var <- sum(var_comps$vcov) + sigma(model)^2
    
    std_effect <- coef / sqrt(total_var)
    std_se <- se / sqrt(total_var)
    
    return(c(std_effect, std_se))
  }
  
  # Get original proficiency effects
  prof_effects <- sapply(measures, function(m) {
    get_std_prof_effect(models[[paste0(m, "_prof")]])
  })
  
  # Process bootstrap results for proficiency
  process_prof_bootstrap_results <- function(bootstrap_results) {
    sapply(measures, function(m) {
      boot_matrix <- bootstrap_results[[paste0(m, "_prof")]]
      effect_samples <- boot_matrix["proficiency",]
      c(mean(effect_samples), sd(effect_samples))
    })
  }
  
  original_boot_prof <- process_prof_bootstrap_results(bootstrap_results_original)
  scenario_boot_prof <- process_prof_bootstrap_results(bootstrap_results_scenario)
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Measure = factor(measures, 
                     levels = c("WER", "Linear", "Polynomial", "Hierarchical", "Semantic"),
                     labels = c("Word Form", "Linear Syntax", "Polynomial", "Hierarchical", "Semantic")),
    Original_Effect = prof_effects[1,],
    Original_SE = prof_effects[2,],
    Bootstrap_Effect = original_boot_prof[1,],
    Bootstrap_SE = original_boot_prof[2,],
    Scenario_Effect = scenario_boot_prof[1,],
    Scenario_SE = scenario_boot_prof[2,]
  )
  
  # Create the proficiency comparison plot
  ggplot(plot_data) +
    geom_pointrange(aes(x = Measure, y = Original_Effect,
                        ymin = Original_Effect - 1.96*Original_SE,
                        ymax = Original_Effect + 1.96*Original_SE,
                        color = "Original Model"),
                    position = position_dodge(width = 0.5)) +
    geom_pointrange(aes(x = Measure, y = Bootstrap_Effect,
                        ymin = Bootstrap_Effect - 1.96*Bootstrap_SE,
                        ymax = Bootstrap_Effect + 1.96*Bootstrap_SE,
                        color = "Parametric Bootstrap"),
                    position = position_dodge(width = 0.5)) +
    geom_pointrange(aes(x = Measure, y = Scenario_Effect,
                        ymin = Scenario_Effect - 1.96*Scenario_SE,
                        ymax = Scenario_Effect + 1.96*Scenario_SE,
                        color = "Scenario Bootstrap"),
                    position = position_dodge(width = 0.5)) +
    coord_flip() +
    scale_color_manual(values = c("Original Model" = "#8884d8",
                                  "Parametric Bootstrap" = "#82ca9d",
                                  "Scenario Bootstrap" = "#ffc658")) +
    labs(title = "Comparison of Proficiency Effects Across Processing Levels",
         y = "Standardized Proficiency Effect",
         x = "Processing Level",
         color = "Model Type") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank())
}

# Generate and save the plots
comparison_plot <- plot_bootstrap_comparisons(data, models, 
                                              bootstrap_results_original, 
                                              bootstrap_results_scenario)
proficiency_plot <- plot_proficiency_comparisons(data, models, 
                                                 bootstrap_results_original, 
                                                 bootstrap_results_scenario)

# Save plots
ggsave("bootstrap_comparison_plot.png", comparison_plot, width = 10, height = 8)
ggsave("proficiency_comparison_plot.png", proficiency_plot, width = 10, height = 8)


# Function to create  table for bootstrap comparisons
create_bootstrap_comparison_table <- function(data, models, bootstrap_results_original, bootstrap_results_scenario) {
  # Standardization function 
  get_std_effect <- function(model) {
    sum_model <- summary(model)
    group_effect <- sum_model$coefficients["group", "Estimate"]
    group_se <- sum_model$coefficients["group", "Std. Error"]
    
    var_comps <- as.data.frame(VarCorr(model))
    total_var <- sum(var_comps$vcov) + sigma(model)^2
    
    std_effect <- group_effect / sqrt(total_var)
    std_se <- group_se / sqrt(total_var)
    
    return(c(std_effect, std_se))
  }
  
  # Process results for each measure
  table_data <- lapply(measures, function(m) {
    # Original model standardized effects
    orig_effects <- get_std_effect(models[[paste0(m, "_group")]])
    
    # Bootstrap results standardization
    boot_orig <- bootstrap_results_original[[paste0(m, "_group")]]["group",]
    boot_scen <- bootstrap_results_scenario[[paste0(m, "_group")]]["group",]
    
    # Get variance components for standardization
    var_comps_orig <- as.data.frame(VarCorr(models[[paste0(m, "_group")]]))
    total_var <- sum(var_comps_orig$vcov) + sigma(models[[paste0(m, "_group")]])^2
    
    # Standardize bootstrap results
    boot_orig_std <- boot_orig / sqrt(total_var)
    boot_scen_std <- boot_scen / sqrt(total_var)
    
    data.frame(
      Measure = m,
      Original = sprintf("%.3f (%.3f)", orig_effects[1], orig_effects[2]),
      Bootstrap = sprintf("%.3f (%.3f)", mean(boot_orig_std), sd(boot_orig_std)),
      Scenario = sprintf("%.3f (%.3f)", mean(boot_scen_std), sd(boot_scen_std))
    )
  })
  
  # Combine into single dataframe
  table_df <- do.call(rbind, table_data)
  
  # Create formatted table
  gt(table_df) %>%
    tab_header(
      title = "Comparison of Standardized Effects Across Processing Levels",
      subtitle = "Group Effects (L2 vs L1) with Standard Errors"
    ) %>%
    cols_label(
      Measure = "Processing Level",
      Original = "Original Model",
      Bootstrap = "Parametric Bootstrap",
      Scenario = "Scenario Bootstrap"
    ) %>%
    tab_spanner(
      label = "Modeling Approach",
      columns = c("Original", "Bootstrap", "Scenario")
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    fmt_markdown(columns = everything()) %>%
    tab_footnote(
      footnote = "Values represent standardized effects calculated as coefficient/sqrt(total variance)",
      locations = cells_column_labels(columns = c(Original, Bootstrap, Scenario))
    ) %>%
    tab_footnote(
      footnote = "Standard errors in parentheses",
      locations = cells_column_labels(columns = c(Original, Bootstrap, Scenario))
    ) %>%
    cols_align(
      align = "left",
      columns = Measure
    ) %>%
    cols_align(
      align = "center",
      columns = c(Original, Bootstrap, Scenario)
    )
}

comparison_table <- create_bootstrap_comparison_table(data, models, 
                                                      bootstrap_results_original, 
                                                      bootstrap_results_scenario)

#----------------------
# STATISTICAL COMPARISONS
#----------------------

# Function to perform statistical comparisons between approaches

compare_bootstrap_approaches <- function(models, bootstrap_results_original, bootstrap_results_scenario) {
  comparison_results <- list()
  
  for(measure in measures) {
    cat("\nAnalyzing", measure, "\n")
    
    # Get original model effects with proper standardization
    orig_model <- models[[paste0(measure, "_group")]]
    
    # Calculate total variance
    var_comps <- as.data.frame(VarCorr(orig_model))
    total_var <- sum(var_comps$vcov) + sigma(orig_model)^2
    
    # Get standardized original coefficient
    orig_coef <- fixef(orig_model)["group"] / sqrt(total_var)
    orig_se <- sqrt(vcov(orig_model)["group", "group"]) / sqrt(total_var)
    
    # Get standardized bootstrap distributions
    boot_orig <- bootstrap_results_original[[paste0(measure, "_group")]]["group",] / sqrt(total_var)
    boot_scen <- bootstrap_results_scenario[[paste0(measure, "_group")]]["group",] / sqrt(total_var)
    
    # Calculate properly standardized z-scores
    z_orig_boot <- (orig_coef - mean(boot_orig)) / 
      sqrt(orig_se^2 + var(boot_orig))
    p_orig_boot <- 2 * (1 - pnorm(abs(z_orig_boot)))
    
    z_orig_scen <- (orig_coef - mean(boot_scen)) / 
      sqrt(orig_se^2 + var(boot_scen))
    p_orig_scen <- 2 * (1 - pnorm(abs(z_orig_scen)))
    
    z_boot_scen <- (mean(boot_orig) - mean(boot_scen)) / 
      sqrt(var(boot_orig) + var(boot_scen))
    p_boot_scen <- 2 * (1 - pnorm(abs(z_boot_scen)))
    
    # Store results
    comparison_results[[measure]] <- data.frame(
      Comparison = c("Original vs Bootstrap",
                     "Original vs Scenario",
                     "Bootstrap vs Scenario"),
      Z_statistic = c(z_orig_boot, z_orig_scen, z_boot_scen),
      P_value = c(p_orig_boot, p_orig_scen, p_boot_scen)
    )
  }
  
  return(comparison_results)
}

# Function to create visual comparison of differences
plot_approach_differences <- function(comparison_results) {
  # Combine all results into one dataframe
  all_comparisons <- do.call(rbind, Map(function(measure, results) {
    results$Measure <- measure
    results
  }, names(comparison_results), comparison_results))
  
  # Add significance indicators
  all_comparisons$Significance <- ifelse(all_comparisons$P_value < 0.001, "***",
                                         ifelse(all_comparisons$P_value < 0.01, "**",
                                                ifelse(all_comparisons$P_value < 0.05, "*", "ns")))
  
  # Create plot
  ggplot(all_comparisons, 
         aes(x = reorder(paste(Measure, Comparison), abs(Z_statistic)), 
             y = abs(Z_statistic),
             fill = P_value < 0.05)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Significance), vjust = -0.5) +
    scale_fill_manual(values = c("grey70", "darkblue"),
                      labels = c("Non-significant", "Significant"),
                      name = "Significance") +
    coord_flip() +
    labs(title = "Differences Between Modeling Approaches",
         x = "",
         y = "Absolute Z-statistic") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12, face = "bold"))
}

# Function to create summary table
create_comparison_summary <- function(comparison_results) {
  # Process results into a summary format
  summary_data <- lapply(names(comparison_results), function(measure) {
    results <- comparison_results[[measure]]
    data.frame(
      Measure = measure,
      Max_Difference = max(abs(results$Z_statistic)),
      Any_Significant = any(results$P_value < 0.05),
      Most_Different_Comparison = results$Comparison[which.max(abs(results$Z_statistic))],
      Min_P_Value = min(results$P_value)
    )
  })
  
  summary_df <- do.call(rbind, summary_data)
  
  # Create formatted table using gt
  gt_table <- summary_df %>%
    gt() %>%
    tab_header(
      title = "Summary of Differences Between Modeling Approaches",
      subtitle = "Comparing Original, Bootstrap, and Scenario Results"
    ) %>%
    fmt_number(
      columns = c("Max_Difference", "Min_P_Value"),
      decimals = 3
    ) %>%
    cols_label(
      Measure = "Processing Level",
      Max_Difference = "Maximum Z-statistic",
      Any_Significant = "Significant Differences",
      Most_Different_Comparison = "Most Different Comparison",
      Min_P_Value = "Minimum p-value"
    )
  
  return(gt_table)
}

# Run the comparisons
comparison_results <- compare_bootstrap_approaches(models, 
                                                   bootstrap_results_original, 
                                                   bootstrap_results_scenario)

# Create and save visualization
comparison_plot <- plot_approach_differences(comparison_results)
ggsave("approach_differences_plot.png", comparison_plot, width = 10, height = 8)

# Create summary table
summary_table <- create_comparison_summary(comparison_results)
print(summary_table)

# Add proficiency comparisons
compare_proficiency_approaches <- function(models, bootstrap_results_original, bootstrap_results_scenario) {
  proficiency_results <- list()
  
  for(measure in measures) {
    cat("\nAnalyzing proficiency effects for", measure, "\n")
    
    # Get original model effects
    orig_model <- models[[paste0(measure, "_prof")]]
    orig_coef <- fixef(orig_model)["proficiency"]
    orig_se <- sqrt(vcov(orig_model)["proficiency", "proficiency"])
    
    # Get bootstrap distributions
    boot_orig <- bootstrap_results_original[[paste0(measure, "_prof")]]["proficiency",]
    boot_scen <- bootstrap_results_scenario[[paste0(measure, "_prof")]]["proficiency",]
    
    # Calculate comparisons (similar to group comparisons)
    z_orig_boot <- (orig_coef - mean(boot_orig)) / 
      sqrt(orig_se^2 + var(boot_orig))
    p_orig_boot <- 2 * (1 - pnorm(abs(z_orig_boot)))
    
    z_orig_scen <- (orig_coef - mean(boot_scen)) / 
      sqrt(orig_se^2 + var(boot_scen))
    p_orig_scen <- 2 * (1 - pnorm(abs(z_orig_scen)))
    
    z_boot_scen <- (mean(boot_orig) - mean(boot_scen)) / 
      sqrt(var(boot_orig) + var(boot_scen))
    p_boot_scen <- 2 * (1 - pnorm(abs(z_boot_scen)))
    
    proficiency_results[[measure]] <- data.frame(
      Comparison = c("Original vs Bootstrap",
                     "Original vs Scenario",
                     "Bootstrap vs Scenario"),
      Z_statistic = c(z_orig_boot, z_orig_scen, z_boot_scen),
      P_value = c(p_orig_boot, p_orig_scen, p_boot_scen)
    )
  }
  
  return(proficiency_results)
}

# Run proficiency comparisons
proficiency_comparison_results <- compare_proficiency_approaches(models, 
                                                                 bootstrap_results_original, 
                                                                 bootstrap_results_scenario)

# Create visualization for proficiency comparisons
proficiency_comparison_plot <- plot_approach_differences(proficiency_comparison_results)
ggsave("proficiency_approach_differences_plot.png", proficiency_comparison_plot, 
       width = 10, height = 8)

# Create summary table for proficiency comparisons
proficiency_summary_table <- create_comparison_summary(proficiency_comparison_results)
print(proficiency_summary_table)

# Create approach comparison table
create_approach_comparison_table <- function(comparison_results) {
  # Standardize z-statistics using total variance
  table_data <- lapply(names(comparison_results), function(measure) {
    results <- comparison_results[[measure]]
    
    # Get total variance for standardization
    var_comps <- as.data.frame(VarCorr(models[[paste0(measure, "_group")]]))
    total_var <- sum(var_comps$vcov) + sigma(models[[paste0(measure, "_group")]])^2
    
    # Format z-statistics and p-values with significance stars
    format_stat <- function(z, p) {
      z_std <- z / sqrt(total_var)
      stars <- ifelse(p < 0.001, "***",
                      ifelse(p < 0.01, "**",
                             ifelse(p < 0.05, "*", "")))
      sprintf("%.3f%s", z_std, stars)
    }
    
    data.frame(
      Measure = measure,
      Orig_vs_Boot = format_stat(results$Z_statistic[1], results$P_value[1]),
      Orig_vs_Scen = format_stat(results$Z_statistic[2], results$P_value[2]),
      Boot_vs_Scen = format_stat(results$Z_statistic[3], results$P_value[3])
    )
  })
  

  table_df <- do.call(rbind, table_data)
  
  gt(table_df) %>%
    tab_header(
      title = "Statistical Comparisons Between Modeling Approaches",
      subtitle = "Standardized Z-statistics with Significance Indicators"
    ) %>%
    cols_label(
      Measure = "Processing Level",
      Orig_vs_Boot = "Original vs.\nBootstrap",
      Orig_vs_Scen = "Original vs.\nScenario",
      Boot_vs_Scen = "Bootstrap vs.\nScenario"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    fmt_markdown(columns = everything()) %>%
    tab_footnote(
      footnote = "* p < .05, ** p < .01, *** p < .001",
      locations = cells_column_labels(columns = c(Orig_vs_Boot, Orig_vs_Scen, Boot_vs_Scen))
    ) %>%
    tab_footnote(
      footnote = "Values represent standardized differences accounting for total variance",
      locations = cells_column_labels(columns = c(Orig_vs_Boot, Orig_vs_Scen, Boot_vs_Scen))
    ) %>%
    cols_align(
      align = "left",
      columns = Measure
    ) %>%
    cols_align(
      align = "center",
      columns = c(Orig_vs_Boot, Orig_vs_Scen, Boot_vs_Scen)
    )
}

# Generate table

statistical_table <- create_approach_comparison_table(comparison_results)

