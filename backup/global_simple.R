# Simplified global.R for deployment
library(shiny)
library(shinyjs)
library(dplyr)
library(bslib)
library(rvest)
library(chromote)
library(purrr)

# Load data directly
bird_df <- tryCatch({
    read.csv("occurrence.txt")
}, error = function(e) {
    # Fallback with minimal data if file not found
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