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

# Run check assumptions function
check_assumptions <- function(data, models) {
  results <- list()
  
  # Standardize data for checks
  data_std <- data
  for(measure in measures) {
    data_std[[measure]] <- datawizard::standardize(data[[measure]])
  }
  
  # 1. Internal Consistency with reversed scoring where appropriate
  reliability <- psych::alpha(data_std[,measures], check.keys=TRUE)
  results$reliability <- reliability
  
  # 2. Multivariate Normality
  mvn_result <- mvn(data_std[,measures], mvnTest = "hz")
  results$mvn <- mvn_result
  
  # 3. Model Diagnostics
  for(measure in measures) {
    model <- models[[paste0(measure, "_group")]]
    
    # Basic model checks
    results[[paste0(measure, "_diagnostics")]] <- check_model(model)
    
    # VIF for fixed effects
    results[[paste0(measure, "_vif")]] <- car::vif(model)
    
    # Convergence check
    results[[paste0(measure, "_convergence")]] <- isSingular(model)
    
    # Additional diagnostics
    results[[paste0(measure, "_ranef_plot")]] <- plot(ranef(model))
  }
  
  return(results)
}

plots <- plot_mixed_effects(models, data)

# Save plots
print("\n=== Saving Plots ===")
for(name in names(plots)) {
  filename <- paste0(name, "_plot.png")
  print(paste("Saving", filename))
  ggsave(filename, plots[[name]])
}

measures <- c("WER", "Polynomial", "Linear", "Hierarchical", "Semantic")
check_assumptions(data, models)

#get summary() for each model
for(measure in measures) {
  print(summary(models[[paste0(measure, "_group")]]))
  print(summary(models[[paste0(measure, "_prof")]]))
}

#----------------------
# ADDITIONAL VISUALIZATIONS
#----------------------

plot_effect_sizes <- function(data, models) {
  # Calculate standardized effects and CIs for each measure
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
  
  effects <- sapply(measures, function(m) {
    get_std_effect(models[[paste0(m, "_group")]])
  })
  
  plot_data <- data.frame(
    Measure = factor(measures, 
                     levels = c("WER", "Linear", "Polynomial", "Hierarchical", "Semantic"),
                     labels = c("Word Form", "Linear Syntax", "Polynomial", "Hierarchical", "Semantic")),
    Effect = effects[1,],
    SE = effects[2,]
  )
  
  ggplot(plot_data, aes(x = Measure, y = Effect, fill = Effect)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = Effect - 1.96*SE, 
                      ymax = Effect + 1.96*SE), 
                  width = 0.2) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0) +
    coord_flip() +
    labs(title = "Standardized Group Effects Across Processing Levels",
         y = "Standardized Effect Size (L2 vs L1)",
         x = "Processing Level") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Function to create interaction plots
plot_interactions <- function(data) {
  long_data <- data %>%
    pivot_longer(cols = all_of(measures),
                 names_to = "measure", 
                 values_to = "score") %>%
    mutate(measure = factor(measure, 
                            levels = measures))
  
  # Standardize long_data using datawizard ignoring N/As
  long_data$score <- datawizard::standardize(long_data$score, na.rm = TRUE)
  
  ggplot(long_data, aes(x = item_length_c, y = score, color = factor(group))) +
    geom_smooth(method = "lm") +
    facet_wrap(~measure, scales = "free_y") +
    labs(title = "Processing Depth √ó Item Length Interactions",
         x = "Item Length (centered)",
         y = "Score",
         color = "Group") +
    scale_color_discrete(labels = c("L1", "L2")) +
    theme_minimal()
}

# Function to create proficiency gradient plot
plot_proficiency_effects <- function(data, models) {
  get_prof_effect <- function(model) {
    coef(summary(model))["proficiency", "Estimate"]
  }
  
  prof_effects <- sapply(measures, function(m) {
    get_prof_effect(models[[paste0(m, "_prof")]])
  })
  
  plot_data <- data.frame(
    Measure = factor(measures,
                     levels = measures),
    Effect = prof_effects
  )
  
  # Standardize prof_effects using datawizard ignoring N/As
  plot_data$Effect <- datawizard::standardize(plot_data$Effect, na.rm = TRUE)
  
  ggplot(plot_data, aes(x = Measure, y = Effect, fill = Effect)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0) +
    coord_flip() +
    labs(title = "Proficiency Effects Across Processing Levels",
         y = "Proficiency Coefficient",
         x = "Processing Level") +
    theme_minimal()
}

plots2 <- list(
  effect_sizes = plot_effect_sizes(data, models),
  interactions = plot_interactions(data),
  proficiency = plot_proficiency_effects(data, models)
)

# Function to get standardized group effects (from plot_effect_sizes)
get_std_group_effect <- function(model) {
  sum_model <- summary(model)
  group_effect <- sum_model$coefficients["group", "Estimate"]
  group_se <- sum_model$coefficients["group", "Std. Error"]
  
  var_comps <- as.data.frame(VarCorr(model))
  total_var <- sum(var_comps$vcov) + sigma(model)^2
  
  std_effect <- group_effect / sqrt(total_var)
  std_se <- group_se / sqrt(total_var)
  
  return(c(std_effect, std_se))
}

# Function to get standardized proficiency effects (from plot_proficiency_effects)
get_std_prof_effect <- function(model) {
  coef <- coef(summary(model))["proficiency", "Estimate"]
  se <- coef(summary(model))["proficiency", "Std. Error"]
  
  var_comps <- as.data.frame(VarCorr(model))
  total_var <- sum(var_comps$vcov) + sigma(model)^2
  
  std_effect <- coef / sqrt(total_var)
  std_se <- se / sqrt(total_var)
  
  return(c(std_effect, std_se))
}

# Create tables for both
group_effects <- sapply(measures, function(m) {
  get_std_group_effect(models[[paste0(m, "_group")]])
})

prof_effects <- sapply(measures, function(m) {
  get_std_prof_effect(models[[paste0(m, "_prof")]])
})

# Create dataframes
group_table <- data.frame(
  Measure = factor(measures, 
                   levels = c("WER", "Linear", "Polynomial", "Hierarchical", "Semantic"),
                   labels = c("Word Form", "Linear Syntax", "Polynomial", "Hierarchical", "Semantic")),
  Effect = group_effects[1,],
  SE = group_effects[2,]
) %>%
  mutate(
    CI_Lower = Effect - 1.96 * SE,
    CI_Upper = Effect + 1.96 * SE
  )

prof_table <- data.frame(
  Measure = factor(measures, 
                   levels = c("WER", "Linear", "Polynomial", "Hierarchical", "Semantic"),
                   labels = c("Word Form", "Linear Syntax", "Polynomial", "Hierarchical", "Semantic")),
  Effect = prof_effects[1,],
  SE = prof_effects[2,]
) %>%
  mutate(
    CI_Lower = Effect - 1.96 * SE,
    CI_Upper = Effect + 1.96 * SE
  )

print("Group Effects:")
print(group_table)
print("\nProficiency Effects:")
print(prof_table)


#----------------------
# EFFECT SIZE COMPARISONS
#----------------------

calc_std_effect <- function(model) {
  coef <- fixef(model)["group"]
  se <- sqrt(vcov(model)["group", "group"])
  
  vc <- VarCorr(model)
  total_var <- sum(sapply(vc, function(x) x[1])) + sigma(model)^2
  
  std_coef <- coef / sqrt(total_var)
  std_se <- se / sqrt(total_var)
  
  return(c(std_coef, std_se))
}

compare_effects <- function(model1, model2) {
  e1 <- calc_std_effect(model1)
  e2 <- calc_std_effect(model2)
  
  z_stat <- (e1[1] - e2[1]) / sqrt(e1[2]^2 + e2[2]^2)
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  return(list(z_stat = z_stat, p_value = p_value))
}

# Check if comparisons between effects are significant
effect_comparisons <- list()
for(i in 1:(length(measures)-1)) {
  for(j in (i+1):length(measures)) {
    comparison <- compare_effects(models[[paste0(measures[i], "_group")]], 
                                  models[[paste0(measures[j], "_group")]])
    effect_comparisons[[paste(measures[i], "vs", measures[j])]] <- comparison
  }
}

# Plot the above
plot_effect_size_comparisons <- function(effect_comparisons) {
  comparison_df <- data.frame(
    Comparison = names(effect_comparisons),
    Z_statistic = sapply(effect_comparisons, function(x) x$z_stat),
    P_value = sapply(effect_comparisons, function(x) x$p_value)
  )
  
  comparison_df$Significance <- ifelse(comparison_df$P_value < 0.001, "***",
                                       ifelse(comparison_df$P_value < 0.01, "**",
                                              ifelse(comparison_df$P_value < 0.05, "*", "ns")))
  
  ggplot(comparison_df, aes(x = reorder(Comparison, abs(Z_statistic)), y = abs(Z_statistic), 
                            fill = P_value < 0.05)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Significance), vjust = -0.5) +
    scale_fill_manual(values = c("grey70", "darkblue"), 
                      labels = c("Non-significant", "Significant"),
                      name = "Significance") +
    coord_flip() +
    labs(title = "Effect Size Comparisons Between Processing Levels",
         x = "Comparison",
         y = "Absolute Z-statistic") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12))
}

plot3 <- plot_effect_size_comparisons(effect_comparisons)


#----------------------
# POWER ANALYSIS
#----------------------

# Function to get effect size from pilot data more robustly
get_pilot_effect <- function(model) {
  # Get fixed effect for group
  beta <- fixef(model)["group"]
  
  # Get residual variance
  res_var <- sigma(model)^2
  
  # Get random effect variances, handling potential singularity
  vc <- VarCorr(model)
  rand_var <- sum(sapply(vc, function(x) {
    var <- attr(x, "stddev")^2
    if(is.na(var) || var < 1e-10) 0 else var
  }))
  
  # Calculate standardized effect
  total_var <- rand_var + res_var
  std_effect <- beta / sqrt(total_var)
  
  return(std_effect)
}

# Modified power analysis function
run_basic_power <- function(pilot_model, n_l1_seq, n_l2_seq) {
  # Get standardized effect size
  d <- get_pilot_effect(pilot_model)
  
  # Calculate power for each sample size combination
  results <- expand.grid(
    n_l1 = n_l1_seq,
    n_l2 = n_l2_seq,
    power = NA
  )
  
  for(i in 1:nrow(results)) {
    # Calculate power for unequal sample sizes
    n1 <- results$n_l1[i]
    n2 <- results$n_l2[i]
    
    # Use pwr.t2n.test for unequal sample sizes
    power_result <- pwr.t2n.test(
      n1 = n1,
      n2 = n2,
      d = abs(d),
      sig.level = 0.05,
      power = NULL
    )
    
    results$power[i] <- power_result$power
  }
  
  return(results)
}

# Function to create power plots
plot_power_results <- function(power_results, measure_name) {
  ggplot(power_results, aes(x = n_l2, y = power, color = factor(n_l1))) +
    geom_line() +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = paste("Power Analysis Results for", measure_name),
         x = "Number of L2 Participants",
         y = "Statistical Power",
         color = "Number of L1\nParticipants") +
    theme_minimal()
}

# Run power analysis for each measure
measures <- c("WER", "Polynomial", "Linear", "Hierarchical", "Semantic")
n_l1_seq <- c(10, 15, 20, 25)  # Sequence of L1 sample sizes to test
n_l2_seq <- seq(20, 60, by = 5)  # Sequence of L2 sample sizes to test

# Store results
power_results <- list()
power_plots <- list()

for(measure in measures) {
  cat("\nRunning power analysis for", measure, "\n")
  
  # Get pilot model
  pilot_model <- models[[paste0(measure, "_group")]]
  
  # Run power analysis
  power_results[[measure]] <- run_basic_power(pilot_model, n_l1_seq, n_l2_seq)
  
  # Create and store plot
  power_plots[[measure]] <- plot_power_results(power_results[[measure]], measure)
}

# Create summary table
create_power_summary <- function(power_results) {
  summary_table <- data.frame(
    Measure = character(),
    N_L1_min = numeric(),
    N_L2_min = numeric(),
    Power = numeric(),
    Effect_Size = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(measure in names(power_results)) {
    results <- power_results[[measure]]
    
    # Find minimum sample sizes that achieve 80% power
    power_80 <- results[results$power >= 0.8,]
    if(nrow(power_80) > 0) {
      # Find the combination with smallest total N
      power_80$total_n <- power_80$n_l1 + power_80$n_l2
      min_idx <- which.min(power_80$total_n)[1]
      
      summary_table <- rbind(summary_table,
                             data.frame(
                               Measure = measure,
                               N_L1_min = power_80$n_l1[min_idx],
                               N_L2_min = power_80$n_l2[min_idx],
                               Power = power_80$power[min_idx],
                               Effect_Size = abs(get_pilot_effect(models[[paste0(measure, "_group")]]))))
    }
  }
  
  # Format the table using gt
  gt_table <- summary_table %>%
    gt() %>%
    tab_header(
      title = "Minimum Sample Size Requirements for 80% Power",
      subtitle = "Based on pilot data effect sizes"
    ) %>%
    fmt_number(
      columns = c("Power", "Effect_Size"),
      decimals = 3
    ) %>%
    cols_label(
      Measure = "Measure",
      N_L1_min = "Min L1 N",
      N_L2_min = "Min L2 N",
      Power = "Achieved Power",
      Effect_Size = "Std. Effect Size"
    )
  
  return(gt_table)
}

# Create and display summary table
summary_table <- create_power_summary(power_results)
print(summary_table)

# Save all plots
for(measure in measures) {
  ggsave(
    paste0("power_analysis_", tolower(measure), ".png"),
    power_plots[[measure]],
    width = 8,
    height = 6
  )
}
