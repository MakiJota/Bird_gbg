#!/usr/bin/env Rscript
##################
## Sound Downloader for Bird Game
## Downloads bird sounds from Artfakta and saves them locally
####################

library(rvest)
library(chromote)
library(purrr)
library(dplyr)

# Source the main data and functions
source("global.R")
source("aux_functions.R")

# Create the sounds directory
if (!dir.exists("www")) {
    dir.create("www", recursive = TRUE)
}
if (!dir.exists("www/cuack")) {
    dir.create("www/cuack", recursive = TRUE)
}

# Function to download and convert sound file
download_bird_sound <- function(taxon_id, sound_url, output_dir = "www/cuack") {
    if (is.null(sound_url) || length(sound_url) == 0 || is.na(sound_url)) {
        message("No sound URL for taxon ID: ", taxon_id)
        return(NULL)
    }
    
    # Use first sound if multiple available
    if (length(sound_url) > 1) {
        sound_url <- sound_url[1]
    }
    
    # Create filename
    output_file <- file.path(output_dir, paste0(taxon_id, ".mp3"))
    
    # Skip if file already exists
    if (file.exists(output_file)) {
        message("Sound already exists for taxon ID: ", taxon_id)
        return(output_file)
    }
    
    # Download the sound file
    tryCatch({
        message("Downloading sound for taxon ID: ", taxon_id)
        message("URL: ", sound_url)
        
        # Download with user agent to avoid blocking
        download.file(
            url = sound_url,
            destfile = output_file,
            mode = "wb",
            quiet = FALSE,
            headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
        )
        
        # Check if file was downloaded successfully
        if (file.exists(output_file) && file.size(output_file) > 0) {
            message("✓ Successfully downloaded: ", basename(output_file))
            return(output_file)
        } else {
            message("✗ Failed to download sound for taxon ID: ", taxon_id)
            return(NULL)
        }
        
    }, error = function(e) {
        message("✗ Error downloading sound for taxon ID ", taxon_id, ": ", e$message)
        return(NULL)
    })
}

# Function to download all bird sounds
download_all_bird_sounds <- function() {
    message("=== Starting Bird Sound Download ===")
    message("Number of birds to process: ", nrow(game_birds))
    
    # Get unique taxon IDs
    taxon_ids <- unique(game_birds$taxonID)
    
    # Download sounds for each bird
    results <- list()
    successful <- 0
    failed <- 0
    
    for (i in seq_along(taxon_ids)) {
        taxon_id <- taxon_ids[i]
        bird_name <- game_birds$scientificName[game_birds$taxonID == taxon_id][1]
        
        message("\\n[", i, "/", length(taxon_ids), "] Processing: ", bird_name, " (ID: ", taxon_id, ")")
        
        # Get sound URLs using existing function
        sound_urls <- get_sounds_go_to(taxon_id)
        
        if (!is.null(sound_urls) && length(sound_urls) > 0) {
            # Download the sound
            result <- download_bird_sound(taxon_id, sound_urls[1])
            
            if (!is.null(result)) {
                results[[taxon_id]] <- result
                successful <- successful + 1
            } else {
                failed <- failed + 1
            }
        } else {
            message("No sound URLs found for: ", bird_name)
            failed <- failed + 1
        }
        
        # Add small delay to be respectful to the server
        Sys.sleep(1)
    }
    
    message("\\n=== Download Summary ===")
    message("✓ Successful downloads: ", successful)
    message("✗ Failed downloads: ", failed)
    message("📁 Sounds saved to: www/cuack/")
    
    return(results)
}

# Function to create a manifest of available sounds
create_sound_manifest <- function() {
    sound_files <- list.files("www/cuack", pattern = "\\.mp3$", full.names = FALSE)
    taxon_ids <- gsub("\\.mp3$", "", sound_files)
    
    manifest <- data.frame(
        taxonID = taxon_ids,
        filename = sound_files,
        filepath = file.path("cuack", sound_files),
        created = Sys.time(),
        stringsAsFactors = FALSE
    )
    
    # Add bird names
    manifest <- manifest %>%
        left_join(game_birds %>% select(taxonID, scientificName) %>% distinct(), 
                  by = "taxonID")
    
    # Save manifest
    write.csv(manifest, "www/cuack/sound_manifest.csv", row.names = FALSE)
    message("Sound manifest created: www/cuack/sound_manifest.csv")
    
    return(manifest)
}

# Main execution
if (!interactive()) {
    # Command line execution
    cat("Bird Sound Downloader\\n")
    cat("====================\\n")
    
    # Check if chromote is available
    if (!isTRUE(getOption("chromote.available", FALSE))) {
        cat("Error: Chromote is not available. Please install Chrome/Chromium.\\n")
        quit(status = 1)
    }
    
    # Run the download
    results <- download_all_bird_sounds()
    
    # Create manifest
    manifest <- create_sound_manifest()
    
    cat("\\nDownload complete!\\n")
    cat("Files saved to: www/cuack/\\n")
    cat("Manifest: www/cuack/sound_manifest.csv\\n")
}

# Interactive functions for manual use
if (interactive()) {
    cat("Bird Sound Downloader loaded.\\n")
    cat("Available functions:\\n")
    cat("- download_all_bird_sounds()\\n")
    cat("- create_sound_manifest()\\n")
    cat("- download_bird_sound(taxon_id, sound_url)\\n")
}


download_all_bird_sounds()