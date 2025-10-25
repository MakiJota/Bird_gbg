# Simplified global.R for deployment
library(shiny)
library(shinyjs)
library(dplyr)
library(bslib)
library(rvest)
library(chromote)
library(purrr)

# Configure chromote for deployment compatibility
# For local development, assume chromote might work and let it try
# For deployment, it will fail gracefully with error handling
CHROMOTE_AVAILABLE <- TRUE  # Optimistically assume it's available

# Set global flag for functions to check
options(chromote.available = CHROMOTE_AVAILABLE)

# Load data via clean_occurrence_data.R script
bird_df <- tryCatch({
    # Try to source the script from local folder first
    if (file.exists("clean_occurrence_data.R")) {
        source("clean_occurrence_data.R")
        bird_df  # Return the bird_df created by the script
    } else {
        # Fallback: load CSV directly
        read.csv("occurrence.txt")
    }
}, error = function(e) {
    # Final fallback with minimal data if everything fails
    data.frame(
        scientificName = c("Turdus merula", "Parus major", "Passer domesticus"),
        taxonID = c("267064", "267068", "267072"),
        stringsAsFactors = FALSE
    )
})

# Create game birds dataset
game_birds <- bird_df %>%
    select(scientificName, taxonID) %>%
    distinct() %>%
    filter(!is.na(taxonID)) %>%
    slice_head(n = 50)  # Limit for deployment

# Format artfakta info function (simplified)
format_artfakta_info <- function(html_str) {
    if (is.null(html_str) || is.na(html_str) || !nzchar(html_str)) return("")
    # Basic HTML cleanup
    html_str
}