# Load required packages
library(shiny)
library(shinyjs)
library(dplyr)
library(tuneR)
library(bslib)

# Source the data processing scripts to get bird_df
# Robustly find project root by walking up directories looking for known files/folders
find_project_root <- function(markers = c("01_scripts", "BirdMapGothenburg.Rproj")) {
    cur <- tryCatch(normalizePath(getwd(), winslash = "/"), error = function(e) NA)
    if (is.na(cur)) return(NULL)
    repeat {
        for (m in markers) {
            if (file.exists(file.path(cur, m)) || dir.exists(file.path(cur, m))) return(cur)
        }
        parent <- dirname(cur)
        if (parent == cur) break
        cur <- parent
    }
    return(NULL)
}

project_root <- find_project_root()

# Try a few fallbacks if project root wasn't found
if (is.null(project_root)) {
    # Try parent directory relative to this app folder
    try_root <- tryCatch(normalizePath(file.path(getwd(), ".."), winslash = "/"), error = function(e) NULL)
    if (!is.null(try_root) && (file.exists(file.path(try_root, "01_scripts")) || file.exists(file.path(try_root, "BirdMapGothenburg.Rproj")))) {
        project_root <- try_root
    }
}

# Source the data processing scripts to get bird_df; try several candidate locations
script_candidates <- character()
if (!is.null(project_root)) {
    script_candidates <- c(script_candidates, file.path(project_root, "01_scripts", "clean_occurrence_data.R"))
}
script_candidates <- c(script_candidates,
                       file.path(getwd(), "..", "01_scripts", "clean_occurrence_data.R"),
                       file.path(getwd(), "01_scripts", "clean_occurrence_data.R"),
                       file.path("..", "01_scripts", "clean_occurrence_data.R"),
                       file.path("01_scripts", "clean_occurrence_data.R"))

script_found <- NULL
for (s in unique(script_candidates)) {
    if (!is.na(s) && nzchar(s) && file.exists(s)) { script_found <- s; break }
}

if (!is.null(script_found)) {
    source(script_found)
} else {
    message("Warning: could not find '01_scripts/clean_occurrence_data.R'. Using empty placeholder 'bird_df'.")
    bird_df <- data.frame(scientificName = character(), taxonID = character(), stringsAsFactors = FALSE)
}

# Function to find image directory
# Function to find image directory from multiple sensible locations
find_image_dir <- function() {
    candidates <- character()
    if (!is.null(project_root)) {
        candidates <- c(candidates,
                        file.path(project_root, "02_ShinyBird", "00_assets", "bird_pics"),
                        file.path(project_root, "02_ShinyBird", "00_assets"),
                        file.path(project_root, "00_assets", "bird_pics"))
    }
    # Also try relative locations from current working directory
    candidates <- c(candidates,
                    file.path(getwd(), "00_assets", "bird_pics"),
                    file.path(getwd(), "02_ShinyBird", "00_assets", "bird_pics"),
                    file.path("..", "02_ShinyBird", "00_assets", "bird_pics"),
                    file.path("02_ShinyBird", "00_assets", "bird_pics"))

    for (c in unique(candidates)) {
        if (!is.na(c) && nzchar(c) && dir.exists(c)) return(normalizePath(c))
        # if it's a file path to jpgs
        files <- tryCatch(list.files(c, pattern = "\\.jpg$", ignore.case = TRUE), error = function(e) character(0))
        if (length(files) > 0) return(normalizePath(c))
    }
    return(NULL)
}

# Get the image directory
image_dir <- find_image_dir()

# Create bird_game_data from bird_df
bird_game_data <- bird_df %>%
    select(scientificName, taxonID) %>%
    distinct() %>%
    filter(!is.na(taxonID))

# Verify images exist
bird_game_data$has_image <- sapply(bird_game_data$taxonID, function(id) {
    img_path <- file.path(image_dir, paste0(id, ".jpg"))
    file.exists(img_path)
})

# Create final game dataset with only birds that have images
game_birds <- bird_game_data %>%
    filter(has_image) %>%
    select(-has_image)