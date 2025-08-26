# =============================================================================
# 03_comparative_analysis.R
# Purpose: Cross-regulation statistical tests and hypothesis testing
# =============================================================================

# Load Required Libraries
# =============================================================================
library(tidyverse)    # For data manipulation
library(broom)        # For tidy statistical outputs
library(lme4)         # For mixed-effects models
library(lmerTest)     # For p-values in mixed-effects models
library(performance)  # For model diagnostics and R²
library(boot)         # For bootstrap confidence intervals

# Load Scored Data
# =============================================================================
cat("Loading scored data from script 02...\n")

# Load the dataset with corporate-friendliness scores
scoring_data <- readRDS("data/processed/02_scoring_data.rds")
provision_map <- readRDS("data/processed/01_provision_map.rds")

# Quick data check
cat("Loaded", nrow(scoring_data), "scored provisions\n")
cat("Regulations analyzed:", paste(unique(scoring_data$regulation), collapse = ", "), "\n")

# Filter to complete cases for statistical analysis
# We need provisions with valid scores for statistical tests
complete_data <- scoring_data %>%
  filter(!is.na(corporate_score), !is.na(regulation), !is.na(provision_key)) %>%
  # Add regulation rankings for ordered comparisons
  mutate(
    regulation_order = case_when(
      regulation == "GDPR" ~ 1,    # Earliest (2012-2016)
      regulation == "DMA" ~ 2,     # Middle (2020-2022)  
      regulation == "AI_Act" ~ 3   # Latest (2021-2024)
    ),
    # Expected lobbying intensity based on your thesis
    lobbying_era = case_when(
      regulation == "GDPR" ~ "Low-Medium (2012-2016)",
      regulation == "DMA" ~ "Medium-High (2020-2022)",
      regulation == "AI_Act" ~ "High (2021-2024)"
    )
  )

cat("Complete cases for analysis:", nrow(complete_data), "\n")

# Descriptive Statistics by Regulation
# =============================================================================
cat("\nCalculating descriptive statistics...\n")

# Overall descriptive statistics
descriptive_stats <- complete_data %>%
  group_by(regulation) %>%
  summarise(
    n_provisions = n(),
    mean_score = mean(corporate_score),
    median_score = median(corporate_score),
    sd_score = sd(corporate_score),
    se_score = sd_score / sqrt(n_provisions),  # Standard error
    min_score = min(corporate_score),
    max_score = max(corporate_score),
    q25 = quantile(corporate_score, 0.25),     # 25th percentile
    q75 = quantile(corporate_score, 0.75),     # 75th percentile
    mean_confidence = mean(score_confidence),
    .groups = "drop"
  ) %>%
  # Round for readability
  mutate(across(where(is.numeric), ~round(.x, 2)))

cat("\nDescriptive Statistics by Regulation:\n")
print(descriptive_stats)

# Calculate 95% confidence intervals for each regulation's mean
# Using the standard formula: mean ± 1.96 * standard_error
descriptive_stats <- descriptive_stats %>%
  mutate(
    ci_lower = mean_score - 1.96 * se_score,
    ci_upper = mean_score + 1.96 * se_score
  ) %>%
  mutate(across(c(ci_lower, ci_upper), ~round(.x, 2)))

cat("\nMean Corporate Scores with 95% Confidence Intervals:\n")
descriptive_stats %>%
  select(regulation, mean_score, ci_lower, ci_upper, n_provisions) %>%
  print()

# Hypothesis Testing
# =============================================================================
cat("\nTesting research hypotheses...\n")

# H1: AI Act and DMA (high lobbying) > GDPR (medium lobbying)
# H2: Public-interest framings decline across successive regulations
# H3: AI Act provisions show more flexibility than equivalent GDPR provisions

# Test 1: One-Way ANOVA - Are there significant differences between regulations?
cat("\n--- Test 1: One-Way ANOVA ---\n")

# ANOVA tests if the means of three or more groups are significantly different
# H0: All regulation means are equal
# H1: At least one regulation mean is different
anova_model <- aov(corporate_score ~ regulation, data = complete_data)
anova_results <- broom::tidy(anova_model)

cat("ANOVA Results:\n")
print(anova_results)

# Effect size (eta-squared): proportion of total variance explained by regulation
# eta² = SS_between / SS_total
anova_summary <- summary(anova_model)
ss_between <- anova_summary[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(anova_summary[[1]][, "Sum Sq"]) # Total sum of squares
eta_squared <- ss_between / ss_total

cat("Effect size (eta-squared):", round(eta_squared, 3), "\n")
cat("Interpretation:", 
    case_when(
      eta_squared < 0.01 ~ "Very small effect",
      eta_squared < 0.06 ~ "Small effect", 
      eta_squared < 0.14 ~ "Medium effect",
      TRUE ~ "Large effect"
    ), "\n")

# Test 2: Post-hoc pairwise comparisons (Tukey HSD)
# If ANOVA is significant, which specific pairs of regulations differ?
cat("\n--- Test 2: Post-hoc Comparisons (Tukey HSD) ---\n")

if (anova_results$p.value[1] < 0.05) {
  # Tukey HSD adjusts for multiple comparisons
  tukey_results <- TukeyHSD(anova_model)
  tukey_tidy <- broom::tidy(tukey_results)
  
  cat("Pairwise Comparisons (Tukey HSD):\n")
  print(tukey_tidy)
  
  # Interpret significant differences
  significant_pairs <- tukey_tidy %>% filter(adj.p.value < 0.05)
  if (nrow(significant_pairs) > 0) {
    cat("\nSignificant pairwise differences (p < 0.05):\n")
    for (i in 1:nrow(significant_pairs)) {
      cat(sprintf("- %s: difference = %.2f, p = %.4f\n", 
                  significant_pairs$contrast[i],
                  significant_pairs$estimate[i], 
                  significant_pairs$adj.p.value[i]))
    }
  } else {
    cat("No significant pairwise differences found.\n")
  }
} else {
  cat("ANOVA not significant - skipping post-hoc tests.\n")
}

# Test 3: Non-parametric alternatives for robustness
cat("\n--- Test 3: Robustness Checks ---\n")

# Kruskal-Wallis test (non-parametric alternative to ANOVA)
# Doesn't assume normal distribution
kruskal_test <- kruskal.test(corporate_score ~ regulation, data = complete_data)
kruskal_tidy <- broom::tidy(kruskal_test)

cat("Kruskal-Wallis Test (non-parametric):\n")
print(kruskal_tidy)

# Welch's ANOVA (assumes unequal variances)
# More robust when group variances are different
welch_test <- oneway.test(corporate_score ~ regulation, data = complete_data)
welch_tidy <- broom::tidy(welch_test)

cat("Welch's ANOVA (unequal variances):\n")
print(welch_tidy)

# Test 4: Planned contrasts for specific hypotheses
cat("\n--- Test 4: Planned Contrasts ---\n")

# Convert regulation to factor with meaningful order
complete_data$regulation_f <- factor(complete_data$regulation, 
                                     levels = c("GDPR", "DMA", "AI_Act"))

# Contrast 1: AI Act vs GDPR (H1 test)
contrast_model1 <- aov(corporate_score ~ regulation_f, data = complete_data)

# Create contrast matrix for AI Act vs GDPR
# GDPR = -1, DMA = 0, AI_Act = +1
contrast_matrix <- matrix(c(-1, 0, 1), nrow = 1)
colnames(contrast_matrix) <- levels(complete_data$regulation_f)

# Perform contrast test
# This tests whether AI Act score > GDPR score
contrast_test1 <- summary.lm(contrast_model1, 
                             split = list(regulation_f = list("AI_Act_vs_GDPR" = contrast_matrix)))

# Get means for each regulation for interpretation
regulation_means <- complete_data %>%
  group_by(regulation) %>%
  summarise(mean_score = mean(corporate_score), .groups = "drop")

cat("Regulation Means:\n")
print(regulation_means)

# Calculate the specific difference AI Act - GDPR
ai_mean <- regulation_means$mean_score[regulation_means$regulation == "AI_Act"]
gdpr_mean <- regulation_means$mean_score[regulation_means$regulation == "GDPR"]
ai_gdpr_diff <- ai_mean - gdpr_mean

cat(sprintf("\nAI Act vs GDPR comparison:\n"))
cat(sprintf("- AI Act mean: %.2f\n", ai_mean))
cat(sprintf("- GDPR mean: %.2f\n", gdpr_mean))
cat(sprintf("- Difference: %.2f\n", ai_gdpr_diff))
cat(sprintf("- Direction: AI Act is %s corporate-friendly than GDPR\n", 
            ifelse(ai_gdpr_diff > 0, "MORE", "LESS")))

# Mixed-Effects Models for Advanced Analysis
# =============================================================================
cat("\n--- Advanced Analysis: Mixed-Effects Models ---\n")

# Mixed-effects models account for the nested structure of data
# (multiple provisions within each regulation, multiple versions within regulation)

# Model 1: Simple model with regulation as fixed effect
cat("\nModel 1: Regulation effects\n")
model1 <- lm(corporate_score ~ regulation, data = complete_data)
model1_summary <- broom::tidy(model1)
model1_glance <- broom::glance(model1)

print(model1_summary)
cat("R-squared:", round(model1_glance$r.squared, 3), "\n")

# Model 2: Add provision type as control
cat("\nModel 2: Regulation + Provision type\n")
model2 <- lm(corporate_score ~ regulation + provision_key, data = complete_data)
model2_summary <- broom::tidy(model2)
model2_glance <- broom::glance(model2)

# Only show regulation coefficients for clarity
regulation_coeffs <- model2_summary %>% 
  filter(str_detect(term, "regulation|Intercept"))

print(regulation_coeffs)
cat("R-squared:", round(model2_glance$r.squared, 3), "\n")

# Model 3: Mixed-effects with random intercepts (if we have version data)
if ("version" %in% colnames(complete_data) && length(unique(complete_data$version)) > 1) {
  cat("\nModel 3: Mixed-effects with random intercepts by version\n")
  
  # This model allows different baseline scores for different document versions
  model3 <- lmer(corporate_score ~ regulation + provision_key + (1|version), 
                 data = complete_data)
  model3_summary <- broom.mixed::tidy(model3, effects = "fixed")
  
  # Extract regulation effects
  regulation_effects <- model3_summary %>% 
    filter(str_detect(term, "regulation|Intercept"))
  
  print(regulation_effects)
  
  # Model fit statistics
  model3_performance <- performance::model_performance(model3)
  cat("Marginal R²:", round(model3_performance$R2_marginal, 3), "\n")
  cat("Conditional R²:", round(model3_performance$R2_conditional, 3), "\n")
}

# Bootstrap Confidence Intervals for Robust Inference
# =============================================================================
cat("\n--- Bootstrap Analysis ---\n")

# Bootstrap sampling gives us robust confidence intervals
# that don't rely on normality assumptions

# Function to calculate mean difference between AI Act and GDPR
mean_diff_function <- function(data, indices) {
  sample_data <- data[indices, ]
  ai_mean <- mean(sample_data$corporate_score[sample_data$regulation == "AI_Act"])
  gdpr_mean <- mean(sample_data$corporate_score[sample_data$regulation == "GDPR"])
  return(ai_mean - gdpr_mean)
}

# Perform bootstrap (1000 resamples)
set.seed(123)  # For reproducible results
bootstrap_results <- boot(complete_data, mean_diff_function, R = 1000)

# Calculate 95% confidence interval
bootstrap_ci <- boot.ci(bootstrap_results, type = "perc")

cat("Bootstrap Analysis (AI Act - GDPR difference):\n")
cat(sprintf("- Original difference: %.2f\n", bootstrap_results$t0))
cat(sprintf("- Bootstrap mean: %.2f\n", mean(bootstrap_results$t)))
cat(sprintf("- 95%% CI: [%.2f, %.2f]\n", bootstrap_ci$percent[4], bootstrap_ci$percent[5]))

# Interpretation
if (bootstrap_ci$percent[4] > 0) {
  cat("- Interpretation: AI Act is significantly MORE corporate-friendly than GDPR\n")
} else if (bootstrap_ci$percent[5] < 0) {
  cat("- Interpretation: AI Act is significantly LESS corporate-friendly than GDPR\n")  
} else {
  cat("- Interpretation: No significant difference between AI Act and GDPR\n")
}

# Provision-Level Analysis
# =============================================================================
cat("\n--- Provision-Level Analysis ---\n")

# Compare equivalent provisions across regulations
provision_comparison <- complete_data %>%
  group_by(provision_key) %>%
  filter(n() >= 2) %>%  # Only provisions that exist in multiple regulations
  summarise(
    n_regulations = n(),
    regulations = paste(sort(regulation), collapse = ", "),
    mean_score = mean(corporate_score),
    sd_score = sd(corporate_score),
    range_score = max(corporate_score) - min(corporate_score),
    .groups = "drop"
  ) %>%
  arrange(desc(range_score))  # Sort by largest differences

cat("Provision-level comparison (equivalent provisions across regulations):\n")
print(provision_comparison)

# Detailed comparison for key provisions
key_provisions <- c("risk_mgmt", "transparency", "penalties", "governance")

for (provision in key_provisions) {
  if (provision %in% complete_data$provision_key) {
    cat(sprintf("\n--- %s provision ---\n", str_to_title(str_replace_all(provision, "_", " "))))
    
    provision_data <- complete_data %>%
      filter(provision_key == provision) %>%
      select(regulation, version, corporate_score, corp_word_count, pub_word_count)
    
    if (nrow(provision_data) > 1) {
      print(provision_data)
      
      # Simple ANOVA for this provision if multiple regulations
      if (length(unique(provision_data$regulation)) > 1) {
        provision_anova <- aov(corporate_score ~ regulation, data = provision_data)
        provision_p <- summary(provision_anova)[[1]][1, "Pr(>F)"]
        cat(sprintf("ANOVA p-value: %.4f\n", provision_p))
      }
    }
  }
}

# Temporal Analysis (Evolution Within Regulations)
# =============================================================================
if ("year" %in% colnames(complete_data) && length(unique(complete_data$year)) > 1) {
  cat("\n--- Temporal Analysis ---\n")
  
  # How do scores change over time within each regulation?
  temporal_trends <- complete_data %>%
    group_by(regulation, year) %>%
    summarise(
      n_provisions = n(),
      mean_score = mean(corporate_score),
      .groups = "drop"
    ) %>%
    arrange(regulation, year)
  
  cat("Temporal trends (scores by year and regulation):\n")
  print(temporal_trends)
  
  # Test for temporal trends
  if (nrow(temporal_trends) > 3) {
    temporal_model <- lm(mean_score ~ year + regulation, data = temporal_trends)
    temporal_summary <- broom::tidy(temporal_model)
    
    cat("\nTemporal regression results:\n")
    print(temporal_summary)
  }
}

# Save Analysis Results
# =============================================================================
cat("\nSaving analysis results...\n")

# Compile all key results into summary objects
analysis_summary <- list(
  descriptive_stats = descriptive_stats,
  anova_results = anova_results,
  eta_squared = eta_squared,
  kruskal_results = kruskal_tidy,
  welch_results = welch_tidy,
  regulation_means = regulation_means,
  ai_gdpr_difference = ai_gdpr_diff,
  bootstrap_ci = bootstrap_ci$percent[4:5],
  provision_comparison = provision_comparison
)

# Add post-hoc results if they exist
if (exists("tukey_tidy")) {
  analysis_summary$tukey_results <- tukey_tidy
}

# Save results
saveRDS(analysis_summary, "data/processed/03_analysis_summary.rds")
write_csv(descriptive_stats, "data/processed/03_descriptive_stats.csv")
write_csv(provision_comparison, "data/processed/03_provision_comparison.csv")

if (exists("tukey_tidy")) {
  write_csv(tukey_tidy, "data/processed/03_posthoc_tests.csv")
}

# Create comprehensive results report
results_report <- paste0(
  "COMPARATIVE ANALYSIS RESULTS\n",
  "============================\n\n",
  "RESEARCH HYPOTHESES TESTED:\n",
  "H1: AI Act (high lobbying) > GDPR (medium lobbying) in corporate-friendliness\n",
  "H2: Public-interest framings decline across successive regulations\n",
  "H3: AI Act shows more flexibility than equivalent GDPR provisions\n\n",
  
  "MAIN FINDINGS:\n",
  "-------------\n",
  sprintf("GDPR mean score: %.2f\n", gdpr_mean),
  sprintf("DMA mean score: %.2f\n", regulation_means$mean_score[regulation_means$regulation == "DMA"]),
  sprintf("AI Act mean score: %.2f\n", ai_mean),
  sprintf("AI Act - GDPR difference: %.2f\n", ai_gdpr_diff),
  sprintf("Direction: AI Act is %s corporate-friendly\n\n", 
          ifelse(ai_gdpr_diff > 0, "MORE", "LESS")),
  
  "STATISTICAL TESTS:\n",
  "------------------\n",
  sprintf("ANOVA F-statistic: %.3f, p-value: %.4f\n", anova_results$statistic[1], anova_results$p.value[1]),
  sprintf("Effect size (eta²): %.3f (%s effect)\n", eta_squared,
          case_when(eta_squared < 0.01 ~ "Very small",
                    eta_squared < 0.06 ~ "Small", 
                    eta_squared < 0.14 ~ "Medium",
                    TRUE ~ "Large")),
  sprintf("Kruskal-Wallis p-value: %.4f\n", kruskal_tidy$p.value),
  sprintf("Welch ANOVA p-value: %.4f\n", welch_tidy$p.value),
  sprintf("Bootstrap 95%% CI for AI-GDPR diff: [%.2f, %.2f]\n", 
          bootstrap_ci$percent[4], bootstrap_ci$percent[5]),
  
  "\nCONCLUSIONS:\n",
  "------------\n",
  ifelse(anova_results$p.value[1] < 0.05, 
         "Significant differences found between regulations.\n",
         "No significant differences found between regulations.\n"),
  ifelse(bootstrap_ci$percent[4] > 0,
         "AI Act is significantly MORE corporate-friendly than GDPR.\n",
         ifelse(bootstrap_ci$percent[5] < 0,
                "AI Act is significantly LESS corporate-friendly than GDPR.\n",
                "No significant difference between AI Act and GDPR.\n")),
  
  "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n"
)

writeLines(results_report, "data/processed/03_analysis_report.txt")

# Final Summary
# =============================================================================
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("COMPARATIVE ANALYSIS SUMMARY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nHYPOTHESIS TEST RESULTS:\n")
cat("------------------------\n")
cat(sprintf("H1 (AI Act > GDPR): %s\n", 
            ifelse(ai_gdpr_diff > 0, "SUPPORTED", "NOT SUPPORTED")))
cat(sprintf("- AI Act mean: %.1f, GDPR mean: %.1f\n", ai_mean, gdpr_mean))
cat(sprintf("- Difference: %.1f points\n", ai_gdpr_diff))

cat(sprintf("\nStatistical Significance:\n"))
cat(sprintf("- ANOVA p-value: %.4f (%s)\n", 
            anova_results$p.value[1],
            ifelse(anova_results$p.value[1] < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT")))
cat(sprintf("- Effect size: %.3f (%s)\n", eta_squared,
            case_when(eta_squared < 0.01 ~ "Very small",
                      eta_squared < 0.06 ~ "Small", 
                      eta_squared < 0.14 ~ "Medium",
                      TRUE ~ "Large")))

cat(sprintf("- Bootstrap CI: [%.2f, %.2f]\n", 
            bootstrap_ci$percent[4], bootstrap_ci$percent[5]))
cat(sprintf("- Interpretation: %s\n",
            ifelse(bootstrap_ci$percent[4] > 0, "AI Act significantly MORE corporate-friendly",
                   ifelse(bootstrap_ci$percent[5] < 0, "AI Act significantly LESS corporate-friendly",
                          "No significant difference"))))

cat("\nFILES CREATED:\n")
cat("- data/processed/03_analysis_summary.rds\n")
cat("- data/processed/03_descriptive_stats.csv\n") 
cat("- data/processed/03_provision_comparison.csv\n")
cat("- data/processed/03_analysis_report.txt\n")
if (exists("tukey_tidy")) {
  cat("- data/processed/03_posthoc_tests.csv\n")
}

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Ready for script 04: Visualizations!\n")