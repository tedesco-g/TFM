# =============================================================================
# 01_data_preparation.R
# Purpose: Load PDFs, extract articles, and clean text for analysis
# =============================================================================

# Load Required Libraries
# =============================================================================
# We need specific packages for different tasks:
# - pdftools: reads PDF files and extracts text
# - tidyverse: collection of packages for data manipulation (dplyr, stringr, etc.)
# - stringr: specifically for text/string manipulation

library(pdftools)    # For reading PDF files
library(tidyverse)   # For data manipulation and cleaning
library(stringr)     # For string/text operations (part of tidyverse but loaded explicitly)

# Set up file paths and document information
# =============================================================================
# Create a named list that maps our document names to their file paths
# This makes it easy to loop through all documents later
# PDFs are located in the data/raw/ subfolder

document_files <- list(
  # AI Act versions (from earliest to latest)
  AI_Act_2021 = "data/raw/ai_act_2021.pdf",
  AI_Act_Council = "data/raw/ai_act_council.pdf", 
  AI_Act_2023_Parliament = "data/raw/ai_act_2023_parl.pdf",
  AI_Act_Final = "data/raw/ai_act_final.pdf",
  
  # GDPR versions (proposal and final)
  GDPR_Proposal = "data/raw/gdpr_prop.pdf",
  GDPR_Final = "data/raw/gdpr_final.pdf",
  
  # DMA versions (proposal and final)
  DMA_Proposal = "data/raw/dma_prop.pdf",
  DMA_Final = "data/raw/dma.pdf"
)

# Print file list to confirm we have everything
cat("Documents to process:\n")
print(names(document_files))

# Load all PDF documents
# =============================================================================
# The map() function applies pdf_text() to each file path
# pdf_text() returns a character vector where each element is one page
# We paste(collapse = " ") to combine all pages into one long string per document
cat("\nLoading PDF documents...\n")

document_texts <- map(document_files, function(file_path) {
  cat("Loading:", file_path, "\n")  # Progress indicator
  
  # pdf_text() extracts text from all pages of the PDF
  pages <- pdf_text(file_path)
  
  # Combine all pages into one string with spaces between pages
  # collapse = " " puts a space between each page when joining
  full_text <- paste(pages, collapse = " ")
  
  return(full_text)
})

# Check that all documents loaded successfully
cat("\nDocuments loaded successfully:\n")
for(i in 1:length(document_texts)) {
  doc_name <- names(document_texts)[i]
  text_length <- nchar(document_texts[[i]])  # Count characters to verify content
  cat(doc_name, ":", text_length, "characters\n")
}

# Article Extraction Functions
# =============================================================================
# This function finds and extracts specific articles from legal documents
# Legal documents have patterns like "Article 5" or "Article 52a"

# For testing this function interactively, uncomment these lines:
# article_number <- "5"  # Test with article 5
# annex_ref <- NA        # Test with no annex
# law_text <- document_texts[[1]]  # Use first document for testing

extract_article <- function(law_text, article_number, annex_ref = NA) {
  
  # Handle missing article numbers
  if (is.na(article_number)) {
    return(NA_character_)  # Return NA if no article number provided
  }
  
  # Extract ANNEX if specified (some provisions are in annexes, not articles)
  if (!is.na(annex_ref)) {
    # (?i) makes the search case-insensitive
    # \\s+ means one or more whitespace characters
    # \\b means word boundary (ensures we match complete words)
    annex_pattern <- paste0("(?i)Annex\\s+", annex_ref, "\\b")
    
    # str_locate_all() finds all positions where pattern matches
    # [[1]] gets the first (and usually only) result
    # [, 1] gets the starting position
    starts <- str_locate_all(law_text, annex_pattern)[[1]][, 1]
    
    if (length(starts) == 0) {
      return(NA_character_)  # Return NA if annex not found
    }
    
    # Find where this annex ends (next annex starts)
    next_annex <- str_locate_all(law_text, "(?i)Annex\\s+[IVX]+\\b")[[1]][, 1]
    nexts <- next_annex[next_annex > starts[1]]  # Only annexes that come after
    end <- if (length(nexts) == 0) nchar(law_text) else min(nexts) - 1
    
    # Extract text between start and end positions
    return(substr(law_text, starts[1], end))
  }
  
  # Extract regular Articles
  # This pattern handles various article numbering formats:
  # - "Article 5" 
  # - "Article 5(1)" (with subsections)
  # - "Article 52a" (with letter suffixes)
  # (?:[a-z]|\\([0-9]+\\)|\\([a-z]\\))* handles optional suffixes
  # (?:...) is a non-capturing group (matches but doesn't save separately)
  pattern <- paste0("Article\\s+", article_number, "(?:[a-z]|\\([0-9]+\\)|\\([a-z]\\))*\\b")
  
  # Find all positions where our target article starts
  starts <- str_locate_all(law_text, pattern)[[1]][, 1]
  
  if (length(starts) == 0) {
    return(NA_character_)  # Return NA if article not found
  }
  
  # Find where this article ends (next article starts)
  # Look for any article pattern in the document
  all_articles <- str_locate_all(law_text, "Article\\s+[0-9]+(?:[a-z]|\\([0-9]+\\)|\\([a-z]\\))*\\b")[[1]][, 1]
  nexts <- all_articles[all_articles > starts[1]]  # Only articles that come after ours
  end <- if (length(nexts) == 0) nchar(law_text) else min(nexts) - 1
  
  # Extract the article text
  return(substr(law_text, starts[1], end))
}

# Helper function to extract multiple provisions from a single document
extract_provisions_version <- function(text, article_nums, annex_refs = NULL) {
  
  # If no annex references provided, create a vector of NAs
  if (is.null(annex_refs)) {
    annex_refs <- rep(NA, length(article_nums))
  }
  
  # map2_chr applies a function to pairs of elements from two vectors
  # It returns a character vector (the "_chr" suffix specifies this)
  # For each (article_number, annex_ref) pair, extract that provision
  extracted_texts <- map2_chr(article_nums, annex_refs, 
                              ~extract_article(text, .x, .y))
  
  return(extracted_texts)
}

# Define Provision Mapping
# =============================================================================
# This tibble (data frame) maps comparable provisions across different regulations
# Each row represents a provision type that exists in multiple regulations
provision_map <- tibble(
  # provision_key: our internal name for this type of provision
  provision_key = c("definition_scope", "gpai", "risk_mgmt", "transparency",
                    "penalties", "law_enforcement", "governance",
                    "prohibited_practices", "high_risk_systems"),
  
  # Article numbers in each regulation (NA means this provision doesn't exist in that regulation)
  ai_act_num = c("3", "52a", "9", "13", "71", "83", "56", "5", "6"),
  gdpr_num = c("4", NA, "25", "12", "83", "23", "68", NA, NA),
  dma_num = c("2", NA, "34", "13", "30", "2", "32", NA, NA),
  
  # Some AI Act provisions are in annexes instead of articles
  ai_act_annex = c(NA, NA, NA, NA, NA, NA, NA, NA, "III"),
  
  # Lobbying intensity estimates (0-100 scale, based on external research)
  # These represent how intensely each provision was lobbied
  ai_act_lobbying = c(45, 95, 75, 82, 35, 20, 60, 88, 92),
  gdpr_lobbying = c(30, 0, 45, 60, 25, 15, 40, 0, 0),     # Lower during GDPR era
  dma_lobbying = c(40, 0, 55, 75, 40, 10, 50, 35, 25),    # Platform-focused lobbying
  
  # Additional metadata about each provision type
  provision_importance = c(90, 100, 85, 80, 70, 60, 75, 95, 90),  # How important this provision is (0-100)
  is_core_provision = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)  # Whether this is a core provision
)

# Display the mapping to verify it looks correct
cat("\nProvision mapping created:\n")
print(provision_map)

# Create Master Dataset
# =============================================================================
cat("\nCreating master dataset with all extracted provisions...\n")

# Function to create complete dataset for cross-regulation comparison
create_cross_regulation_data <- function() {
  
  # Process AI Act versions
  # We have 4 versions of the AI Act to track its evolution
  ai_versions <- c("2021", "Council", "2023_Parliament", "Final")
  
  # map_dfr() applies a function to each element and combines results into a data frame
  # dfr = "data frame by rows" (stacks results vertically)
  ai_data <- map_dfr(ai_versions, function(version) {
    
    # Create the key to look up this version in our document_texts list
    version_key <- case_when(
      version == "2021" ~ "AI_Act_2021",
      version == "Council" ~ "AI_Act_Council", 
      version == "2023_Parliament" ~ "AI_Act_2023_Parliament",
      TRUE ~ "AI_Act_Final"  # Default case (Final)
    )
    
    cat("Processing AI Act version:", version, "\n")
    
    # Extract all provisions for this version
    provisions <- extract_provisions_version(
      document_texts[[version_key]], 
      provision_map$ai_act_num, 
      provision_map$ai_act_annex
    )
    
    # Create a data frame for this version
    tibble(
      regulation = "AI_Act",
      version = version,
      year = case_when(  # Assign approximate years to each version
        version == "2021" ~ 2021,
        version == "Council" ~ 2022,
        version == "2023_Parliament" ~ 2023,
        version == "Final" ~ 2024
      ),
      provision_key = provision_map$provision_key,  # What type of provision this is
      text = provisions,  # The actual extracted text
      lobbying_intensity = provision_map$ai_act_lobbying  # Lobbying intensity for this provision
    )
  })
  
  # Process GDPR data (Proposal and Final versions)
  gdpr_data <- map_dfr(c("Proposal", "Final"), function(version) {
    version_key <- paste0("GDPR_", version)
    
    cat("Processing GDPR version:", version, "\n")
    
    # Extract provisions (no annexes for GDPR, so only article numbers)
    provisions <- extract_provisions_version(
      document_texts[[version_key]], 
      provision_map$gdpr_num
    )
    
    tibble(
      regulation = "GDPR", 
      version = version,
      year = ifelse(version == "Proposal", 2012, 2016),
      provision_key = provision_map$provision_key,
      text = provisions,
      lobbying_intensity = provision_map$gdpr_lobbying
    )
  })
  
  # Process DMA data (Proposal and Final versions)
  dma_data <- map_dfr(c("Proposal", "Final"), function(version) {
    version_key <- paste0("DMA_", version)
    
    cat("Processing DMA version:", version, "\n")
    
    # Extract provisions
    provisions <- extract_provisions_version(
      document_texts[[version_key]], 
      provision_map$dma_num
    )
    
    tibble(
      regulation = "DMA",
      version = version, 
      year = ifelse(version == "Proposal", 2020, 2022),
      provision_key = provision_map$provision_key,
      text = provisions,
      lobbying_intensity = provision_map$dma_lobbying
    )
  })
  
  # Combine all regulations into one master dataset
  # bind_rows() stacks data frames on top of each other
  combined_data <- bind_rows(ai_data, gdpr_data, dma_data) %>%
    mutate(
      # Create a combined identifier for regulation and version
      regulation_version = paste(regulation, version, sep = "_"),
      
      # Flag whether this is the final version of a regulation
      is_final = str_detect(version, "Final|2024"),
      
      # Add contextual information about the lobbying era
      regulation_era = case_when(
        regulation == "GDPR" ~ "Pre-AI Era (2012-2016)",
        regulation == "DMA" ~ "Platform Era (2020-2022)", 
        regulation == "AI_Act" ~ "AI Era (2021-2024)"
      ),
      
      # Expected lobbying intensity category
      expected_lobbying_intensity = case_when(
        regulation == "AI_Act" ~ "High",
        regulation == "DMA" ~ "Medium-High",
        regulation == "GDPR" ~ "Low-Medium"
      )
    )
  
  return(combined_data)
}

# Create the master dataset
master_data <- create_cross_regulation_data()

# Basic Text Cleaning
# =============================================================================
cat("\nApplying basic text cleaning...\n")

# Clean the extracted text to prepare for analysis
master_data <- master_data %>%
  mutate(
    # Calculate basic text statistics before cleaning
    raw_text_length = nchar(text),  # Count characters in original text
    
    # Clean the text:
    # 1. Convert to lowercase for consistent analysis
    # 2. Remove extra whitespace (multiple spaces become single spaces)
    # 3. Remove non-ASCII characters that might cause problems
    clean_text = text %>%
      str_to_lower() %>%  # Convert to lowercase
      str_squish() %>%    # Remove extra whitespace (tidyverse function)
      iconv(to = "ASCII", sub = ""),  # Remove non-ASCII characters
    
    # Calculate statistics for cleaned text
    clean_text_length = nchar(clean_text),
    
    # Count approximate word count (split by whitespace)
    word_count = str_count(clean_text, "\\S+"),  # \\S+ matches non-whitespace sequences
    
    # Flag very short texts that might be problematic
    is_very_short = word_count < 10,  # Flag provisions with fewer than 10 words
    
    # Flag missing provisions (where extraction returned NA)
    is_missing = is.na(text) | text == ""
  )

# Data Quality Check
# =============================================================================
cat("\nPerforming data quality checks...\n")

# Check for missing provisions
missing_summary <- master_data %>%
  group_by(regulation, provision_key) %>%
  summarise(
    total_versions = n(),  # How many versions of this regulation we have
    missing_count = sum(is_missing),  # How many are missing
    short_count = sum(is_very_short, na.rm = TRUE),  # How many are very short
    .groups = "drop"  # Remove grouping after summarise
  ) %>%
  filter(missing_count > 0 | short_count > 0)  # Only show problematic cases

cat("Provisions with missing or very short text:\n")
if (nrow(missing_summary) > 0) {
  print(missing_summary)
} else {
  cat("All provisions successfully extracted!\n")
}

# Overall statistics
cat("\nOverall dataset statistics:\n")
cat("Total observations:", nrow(master_data), "\n")
cat("Regulations:", length(unique(master_data$regulation)), "\n")
cat("Provision types:", length(unique(master_data$provision_key)), "\n")
cat("Document versions:", length(unique(master_data$regulation_version)), "\n")

# Distribution by regulation
regulation_summary <- master_data %>%
  group_by(regulation) %>%
  summarise(
    observations = n(),
    mean_word_count = round(mean(word_count, na.rm = TRUE), 1),
    missing_texts = sum(is_missing),
    .groups = "drop"
  )

cat("\nBy regulation:\n")
print(regulation_summary)

# Save Prepared Data
# =============================================================================
cat("\nSaving prepared data...\n")

# Create processed data folder if it doesn't exist
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Save the master dataset for use in subsequent scripts
# We save both RDS (R's native format, preserves data types) and CSV (human-readable)
saveRDS(master_data, "data/processed/01_master_data.rds")
write_csv(master_data, "data/processed/01_master_data.csv")

# Also save the provision mapping for reference
saveRDS(provision_map, "data/processed/01_provision_map.rds")
write_csv(provision_map, "data/processed/01_provision_map.csv")

# Save document metadata
document_metadata <- tibble(
  document_name = names(document_files),
  file_path = unlist(document_files),
  text_length = map_dbl(document_texts, nchar),
  load_timestamp = Sys.time()
)

write_csv(document_metadata, "data/processed/01_document_metadata.csv")

cat("\nData preparation complete!\n")
cat("Files saved:\n")
cat("- data/processed/01_master_data.rds (for R scripts)\n")
cat("- data/processed/01_master_data.csv (for inspection)\n") 
cat("- data/processed/01_provision_map.rds (provision mapping)\n")
cat("- data/processed/01_document_metadata.csv (document info)\n")

# Summary message
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("DATA PREPARATION SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("✓ Loaded", length(document_files), "PDF documents\n")
cat("✓ Extracted", length(unique(master_data$provision_key)), "provision types\n")
cat("✓ Created dataset with", nrow(master_data), "observations\n")
cat("✓ Applied text cleaning and quality checks\n")
cat("✓ Saved prepared data for analysis\n")
cat(paste(rep("=", 50), collapse = ""), "\n")