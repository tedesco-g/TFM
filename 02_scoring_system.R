# =============================================================================
# 02_scoring_system.R
# Purpose: Create corporate-friendliness scoring system and apply to text data
# =============================================================================

# Load Required Libraries
# =============================================================================
library(tidyverse)   # For data manipulation
library(stringr)     # For text processing
library(binom)       # For confidence intervals on proportions

# Load Prepared Data
# =============================================================================
cat("Loading prepared data from script 01...\n")

# Load the master dataset created in the previous script
# readRDS() loads R's native format which preserves all data types
master_data <- readRDS("data/processed/01_master_data.rds")
provision_map <- readRDS("data/processed/01_provision_map.rds")

# Quick check that data loaded correctly
cat("Loaded dataset with", nrow(master_data), "observations\n")
cat("Regulations:", paste(unique(master_data$regulation), collapse = ", "), "\n")

# Define Corporate and Public Interest Keywords
# =============================================================================
cat("\nDefining corporate-friendly and public-interest keyword lists...\n")

# CORPORATE-FRIENDLY KEYWORDS
# These words suggest flexibility, industry accommodation, and business-friendly approaches
tech_corporate_keywords <- c(
  # Core flexibility & innovation terms
  "innovation", "innovative", "flexibility", "flexible", "proportionate", "proportionality",
  "risk-based", "voluntary", "self-assessment", "self-regulation", "self-certification",
  "self-declaration", "self-evaluation", "internal assessment", "internal controls",
  
  # Business & market orientation
  "competitive", "competitiveness", "market-driven", "market-based", "business-friendly",
  "industry expertise", "industry standards", "industry best practices", "industry guidance",
  "technical standards", "technical feasibility", "technical specifications",
  "cost-effective", "cost-benefit", "efficient", "efficiency", "effectiveness",
  "economic", "economically viable", "commercially viable", "practical",
  
  # Regulatory approach emphasizing balance
  "balanced", "balance", "reasonable", "reasonably", "appropriate", "appropriately",
  "phased implementation", "phased approach", "gradual", "transition period", 
  "grace period", "implementation period", "reasonable timeframe", "sufficient time",
  "progressive", "step-by-step", "staged", "pilot", "trial period",
  
  # Consultation & cooperation language
  "consultation", "stakeholder engagement", "multi-stakeholder", "dialogue",
  "cooperation", "collaboration", "partnership", "co-regulation", "soft law",
  "guidelines", "recommendations", "best practices", "codes of conduct",
  
  # Conditional & escape language (creates wiggle room)
  "where appropriate", "where applicable", "where necessary", "where feasible",
  "taking into account", "considering", "subject to", "without prejudice to",
  "insofar as", "to the extent that", "as far as possible", "where practicable",
  "unless", "except", "save", "notwithstanding", "however", "provided that",
  "may be exempted", "may be excluded", "derogation", "exception",
  
  # Soft obligations (suggests rather than mandates)
  "should", "could", "may", "might", "encouraged", "invited", "expected",
  "strives", "endeavours", "aims", "seeks", "promotes", "supports",
  "recommends", "suggests", "considers", "recognizes",
  
  # Technical & procedural flexibility
  "state of the art", "technical state of the art", "generally accepted",
  "industry practice", "common practice", "established practice",
  "alternative", "alternatives", "equivalent", "comparable", "similar",
  "adapted", "tailored", "customized", "case-by-case", "contextual",
  
  # Innovation & development focus
  "technological development", "technological advancement", "cutting-edge",
  "emerging technology", "disruptive", "breakthrough", "research", "development",
  "experimentation", "testing", "sandbox", "regulatory sandbox",
  "ai development", "machine learning", "algorithmic", "automated",
  
  # Performance-based approaches
  "performance-based", "outcome-based", "results-oriented", "evidence-based",
  "data-driven", "proportionate response", "least burdensome", "minimally invasive",
  "streamlined", "simplified"
)

# PUBLIC-INTEREST KEYWORDS  
# These words suggest strong regulation, protection, and public accountability
tech_public_keywords <- c(
  # Strong regulatory obligations
  "mandatory", "compulsory", "obligatory", "binding", "enforceable",
  "shall", "must", "required", "needs to", "has to", "ought to",
  "imperative", "essential", "necessary", "requisite", "demanded",
  
  # Prohibitions & restrictions
  "prohibited", "forbidden", "banned", "outlawed", "illegal", "unlawful",
  "not permitted", "not allowed", "restricted", "limited", "constrained",
  "prevented", "blocked", "stopped", "ceased", "suspended",
  
  # Enforcement & penalties
  "enforcement", "penalty", "penalties", "sanctions", "fines",
  "punishment", "disciplinary", "corrective measures", "remedial action",
  "administrative fines", "criminal liability", "civil liability",
  "withdrawal", "suspension", "revocation", "termination",
  "cease and desist", "injunction", "court order",
  
  # Oversight & supervision
  "supervision", "oversight", "monitoring", "surveillance", "inspection",
  "audit", "examination", "investigation", "review", "assessment",
  "verification", "validation", "certification", "accreditation",
  "authorization", "approval", "permission", "license", "permit",
  
  # Independent oversight
  "independent", "third party", "external", "competent authority",
  "regulatory authority", "supervisory authority", "notified body",
  "independent assessment", "independent audit", "external verification",
  "regulatory oversight", "government oversight", "public oversight",
  
  # Rights & protection focus
  "fundamental rights", "human rights", "individual rights", "personal rights",
  "protection", "safeguards", "safety", "security", "privacy", 
  "data protection", "consumer protection", "user protection",
  "vulnerable groups", "children", "minors", "elderly", "disabled",
  
  # Transparency & accountability
  "transparency", "transparent", "openness", "disclosure", "publication",
  "public", "publicly available", "accessible", "visibility",
  "accountability", "accountable", "responsible", "liability", "liable",
  "explainability", "explainable", "interpretability", "interpretable",
  "auditability", "auditable", "traceable", "verifiable",
  
  # Democratic & social values
  "democratic", "democracy", "democratic values", "rule of law",
  "public interest", "common good", "social benefit", "societal benefit",
  "public welfare", "general interest", "collective interest",
  "social impact", "societal impact", "public impact", "community impact",
  
  # Fairness & non-discrimination
  "fairness", "fair", "equality", "equal", "non-discrimination",
  "discrimination", "bias", "biased", "prejudice", "stereotyping",
  "inclusive", "inclusion", "diversity", "equitable", "just", "justice",
  "algorithmic bias", "unfair", "unjust", "inequitable", "unequal",
  
  # Human oversight & control
  "human oversight", "human control", "human intervention", "human review",
  "human decision", "human judgment", "human supervision", "human monitoring",
  "meaningful human control", "human in the loop", "human on the loop",
  
  # Prior requirements & preconditions
  "prior to", "before", "in advance", "preliminary", "prerequisite",
  "precondition", "prior authorization", "prior approval", "prior assessment",
  "ex-ante", "proactive", "preventive", "precautionary",
  
  # Strict standards
  "strict", "stringent", "rigorous", "robust", "comprehensive", "thorough",
  "detailed", "specific", "precise", "exact", "clear", "unambiguous",
  "high standards", "demanding", "exacting", "challenging", "tough",
  
  # Immediate action requirements
  "immediate", "immediately", "urgent", "urgently", "without delay",
  "prompt", "promptly", "timely", "expeditiously", "forthwith",
  "as soon as possible", "deadline", "time limit",
  
  # Comprehensive coverage
  "all", "every", "each", "comprehensive", "complete", "full", "total",
  "universal", "across the board", "without exception", "regardless",
  "irrespective", "systematic", "wide-ranging", "extensive", "broad"
)

# Display keyword counts for verification
cat("Corporate-friendly keywords:", length(tech_corporate_keywords), "\n")
cat("Public-interest keywords:", length(tech_public_keywords), "\n")

# Corporate-Friendliness Scoring Functions
# =============================================================================
cat("\nCreating scoring functions...\n")

# Main scoring function that calculates corporate-friendliness for a single text
compute_enhanced_score <- function(text, corp_keywords, pub_keywords) {
  
  # Handle missing or very short text
  if(is.na(text) || text == "" || nchar(text) < 10) {
    return(list(score = NA_real_, confidence = NA_real_, 
                corp_count = 0, pub_count = 0, total_words = 0))
  }
  
  # Clean and tokenize the text
  # str_to_lower() converts to lowercase for consistent matching
  # str_split() breaks text into individual words using non-word characters as separators
  # \\W+ means one or more non-word characters (spaces, punctuation, etc.)
  words <- str_to_lower(str_split(text, "\\W+")[[1]])
  
  # Filter out very short words (length < 3) that are probably not meaningful
  # nchar() counts characters in each word
  words <- words[nchar(words) > 2]
  
  # Count how many corporate and public-interest keywords appear in the text
  # %in% checks if each word is in our keyword lists
  # sum() counts how many TRUE values (i.e., matches) we get
  corp_hits <- sum(words %in% corp_keywords)
  pub_hits <- sum(words %in% pub_keywords)
  total_hits <- corp_hits + pub_hits
  
  # If no relevant keywords found, return neutral score (50)
  if(total_hits == 0) {
    return(list(score = 50, confidence = 0, 
                corp_count = corp_hits, pub_count = pub_hits, 
                total_words = length(words)))
  }
  
  # Calculate the corporate-friendliness score as a percentage
  # Score = (corporate keywords / total relevant keywords) * 100
  # Higher score = more corporate-friendly language
  # Lower score = more public-interest language
  score <- 100 * corp_hits / total_hits
  
  # Calculate confidence based on sample size
  # More keyword matches = higher confidence in our score
  # We cap confidence at 1.0 (100%)
  confidence <- min(total_hits / 10, 1.0)
  
  # Return all the information we calculated
  return(list(
    score = score,              # Corporate-friendliness score (0-100)
    confidence = confidence,    # How confident we are in this score (0-1)
    corp_count = corp_hits,     # Number of corporate keywords found
    pub_count = pub_hits,       # Number of public-interest keywords found
    total_words = length(words) # Total words in cleaned text
  ))
}

# Apply Scoring to All Text Data
# =============================================================================
cat("\nApplying corporate-friendliness scoring to all provisions...\n")

# Create a new dataset with scoring results
# We'll use the cleaned text from the data preparation step
scoring_data <- master_data %>%
  # Filter out completely missing texts to avoid errors
  filter(!is.na(clean_text), clean_text != "") %>%
  
  # Add a row number for progress tracking
  mutate(row_id = row_number()) %>%
  
  # Apply scoring function to each text
  # map() applies our function to each element of clean_text
  # The ~ syntax creates an anonymous function
  mutate(
    # Calculate scores for each text
    score_results = map(clean_text, ~compute_enhanced_score(.x, tech_corporate_keywords, tech_public_keywords)),
    
    # Extract individual components from the score results
    # map_dbl() extracts numeric values from the list results
    corporate_score = map_dbl(score_results, ~.x$score),
    score_confidence = map_dbl(score_results, ~.x$confidence),
    corp_word_count = map_dbl(score_results, ~.x$corp_count),
    pub_word_count = map_dbl(score_results, ~.x$pub_count),
    total_words = map_dbl(score_results, ~.x$total_words)
  ) %>%
  
  # Remove the intermediate score_results column (we don't need it anymore)
  select(-score_results) %>%
  
  # Calculate additional metrics
  mutate(
    # Total relevant keywords found
    total_hits = corp_word_count + pub_word_count,
    
    # Calculate proportion of corporate keywords (alternative to score)
    corp_prop = ifelse(total_hits > 0, corp_word_count / total_hits, NA_real_),
    
    # Text complexity indicators
    text_complexity = str_count(clean_text, "shall|must|may|should|pursuant|notwithstanding"),
    
    # Keywords per 100 words (keyword density)
    keyword_density = ifelse(total_words > 0, (total_hits / total_words) * 100, 0)
  )

# Calculate confidence intervals for corporate proportion
# =============================================================================
cat("\nCalculating confidence intervals for scores...\n")

# For each text, we calculate a confidence interval around our corporate proportion
# This uses the Wilson score interval, which is robust for proportions
scoring_data <- scoring_data %>%
  mutate(
    # Calculate Wilson confidence intervals for each text
    # This gives us upper and lower bounds for our corporate proportion
    ci_results = purrr::map2(corp_word_count, total_hits, function(successes, trials) {
      if (trials == 0 || is.na(trials) || is.na(successes)) {
        return(data.frame(lower = NA_real_, upper = NA_real_))
      }
      # binom.wilson() calculates Wilson score confidence intervals
      # This is more accurate than normal approximation for small samples
      binom::binom.wilson(successes, trials)[, c("lower", "upper")]
    }),
    
    # Extract lower and upper bounds
    corp_prop_lower = map_dbl(ci_results, ~ .x$lower),
    corp_prop_upper = map_dbl(ci_results, ~ .x$upper)
  ) %>%
  
  # Remove intermediate calculations
  select(-ci_results)

# Add provision metadata
# =============================================================================
cat("\nJoining with provision metadata...\n")

# Add information about each provision type from our provision mapping
scoring_data <- scoring_data %>%
  left_join(
    provision_map %>% 
      select(provision_key, provision_importance, is_core_provision),
    by = "provision_key"
  )

# Data Quality and Validation Checks
# =============================================================================
cat("\nPerforming scoring validation checks...\n")

# Check the distribution of scores
score_summary <- scoring_data %>%
  filter(!is.na(corporate_score)) %>%
  summarise(
    n_provisions = n(),
    mean_score = round(mean(corporate_score), 2),
    median_score = round(median(corporate_score), 2),
    sd_score = round(sd(corporate_score), 2),
    min_score = round(min(corporate_score), 2),
    max_score = round(max(corporate_score), 2),
    mean_confidence = round(mean(score_confidence), 3),
    mean_keyword_density = round(mean(keyword_density), 2)
  )

cat("\nScoring Summary Statistics:\n")
print(score_summary)

# Check distribution by regulation
regulation_scores <- scoring_data %>%
  filter(!is.na(corporate_score)) %>%
  group_by(regulation) %>%
  summarise(
    n_provisions = n(),
    mean_score = round(mean(corporate_score), 2),
    sd_score = round(sd(corporate_score), 2),
    mean_corp_words = round(mean(corp_word_count), 1),
    mean_pub_words = round(mean(pub_word_count), 1),
    mean_confidence = round(mean(score_confidence), 3),
    .groups = "drop"
  )

cat("\nScores by Regulation:\n")
print(regulation_scores)

# Identify extreme cases for validation
extreme_cases <- scoring_data %>%
  filter(!is.na(corporate_score)) %>%
  arrange(desc(corporate_score)) %>%
  slice(c(1:3, (n()-2):n())) %>%  # Top 3 and bottom 3 scores
  mutate(text_preview = substr(clean_text, 1, 100)) %>%  # Create preview column first
  select(regulation, version, provision_key, corporate_score, 
         corp_word_count, pub_word_count, word_count, text_preview)

cat("\nExtreme Cases (Highest and Lowest Scores):\n")
print(extreme_cases)

# Check for any potential issues
quality_checks <- list(
  missing_scores = sum(is.na(scoring_data$corporate_score)),
  zero_keywords = sum(scoring_data$total_hits == 0, na.rm = TRUE),
  very_short_texts = sum(scoring_data$total_words < 10, na.rm = TRUE),
  high_confidence = sum(scoring_data$score_confidence > 0.5, na.rm = TRUE),
  low_confidence = sum(scoring_data$score_confidence < 0.1, na.rm = TRUE)
)

cat("\nQuality Checks:\n")
for(check_name in names(quality_checks)) {
  cat(sprintf("- %s: %d\n", gsub("_", " ", check_name), quality_checks[[check_name]]))
}

# Save Scoring Results
# =============================================================================
cat("\nSaving scoring results...\n")

# Save the complete scoring dataset
saveRDS(scoring_data, "data/processed/02_scoring_data.rds")
write_csv(scoring_data, "data/processed/02_scoring_data.csv")

# Save summary statistics
write_csv(score_summary, "data/processed/02_score_summary.csv")
write_csv(regulation_scores, "data/processed/02_regulation_scores.csv")

# Save keyword lists for reference and reproducibility
keyword_metadata <- tibble(
  keyword_type = c(rep("corporate", length(tech_corporate_keywords)),
                   rep("public", length(tech_public_keywords))),
  keyword = c(tech_corporate_keywords, tech_public_keywords)
)

write_csv(keyword_metadata, "data/processed/02_keyword_lists.csv")

# Create a methods documentation file
methods_doc <- paste0(
  "CORPORATE-FRIENDLINESS SCORING METHODOLOGY\n",
  "==========================================\n\n",
  "Scoring Method: Keyword-based lexical analysis\n",
  "Corporate Keywords: ", length(tech_corporate_keywords), " terms\n",
  "Public-Interest Keywords: ", length(tech_public_keywords), " terms\n\n",
  "Score Calculation:\n",
  "- Corporate Score = (Corporate Keywords / Total Relevant Keywords) × 100\n",
  "- Range: 0-100 (higher = more corporate-friendly)\n",
  "- Neutral score (50) assigned when no relevant keywords found\n\n",
  "Confidence Calculation:\n",
  "- Based on total number of relevant keywords found\n",
  "- Higher keyword count = higher confidence\n",
  "- Confidence = min(total_keywords / 10, 1.0)\n\n",
  "Confidence Intervals:\n",
  "- Wilson score intervals for corporate proportion\n",
  "- More robust than normal approximation for small samples\n\n",
  "Data Processing:\n",
  "- Text cleaned and converted to lowercase\n",
  "- Words with < 3 characters excluded\n",
  "- Non-ASCII characters removed\n",
  "- Missing or very short texts handled appropriately\n\n",
  "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n"
)

writeLines(methods_doc, "data/processed/02_methods_documentation.txt")

# Final Summary
# =============================================================================
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("SCORING SYSTEM SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("✓ Applied scoring to", nrow(scoring_data), "provisions\n")
cat("✓ Mean corporate score:", round(score_summary$mean_score, 1), "\n")
cat("✓ Score range:", score_summary$min_score, "-", score_summary$max_score, "\n")
cat("✓ Mean confidence level:", round(score_summary$mean_confidence, 2), "\n")
cat("✓ Keyword density:", round(score_summary$mean_keyword_density, 1), "keywords per 100 words\n")
cat("✓ Saved scoring results and documentation\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

cat("\nFiles created:\n")
cat("- data/processed/02_scoring_data.rds (complete dataset)\n")
cat("- data/processed/02_scoring_data.csv (human-readable)\n")
cat("- data/processed/02_score_summary.csv (summary statistics)\n")
cat("- data/processed/02_regulation_scores.csv (by regulation)\n") 
cat("- data/processed/02_keyword_lists.csv (keyword reference)\n")
cat("- data/processed/02_methods_documentation.txt (methodology)\n")

cat("\nReady for script 03: Comparative Analysis!\n")