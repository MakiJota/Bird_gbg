#!/usr/bin/env Rscript
##################
## Sound Manager for Bird Game
## Creates manifest of existing local sound files
####################

library(dplyr)

# Create the sounds directory structure
if (!dir.exists("www")) {
    dir.create("www", recursive = TRUE)
}
if (!dir.exists("www/cuack")) {
    dir.create("www/cuack", recursive = TRUE)
}

# Function to create a manifest of available sounds
create_sound_manifest <- function() {
    if (!dir.exists("www/cuack")) {
        message("Sound directory www/cuack does not exist")
        return(NULL)
    }
    
    sound_files <- list.files("www/cuack", pattern = "\\.(mp3|wav|m4a)$", full.names = FALSE)
    
    if (length(sound_files) == 0) {
        message("No sound files found in www/cuack/")
        return(NULL)
    }
    
    # Extract taxon IDs from filenames (remove file extension)
    taxon_ids <- gsub("\\.(mp3|wav|m4a)$", "", sound_files)
    
    manifest <- data.frame(
        taxonID = taxon_ids,
        filename = sound_files,
        filepath = file.path("cuack", sound_files),
        created = Sys.time(),
        stringsAsFactors = FALSE
    )
    
    # Try to add bird names if global data is available
    tryCatch({
        if (exists("game_birds") && nrow(game_birds) > 0) {
            manifest <- manifest %>%
                left_join(game_birds %>% select(taxonID, scientificName) %>% distinct(), 
                          by = "taxonID")
        }
    }, error = function(e) {
        message("Could not add bird names to manifest")
    })
    
    # Save manifest
    write.csv(manifest, "www/cuack/sound_manifest.csv", row.names = FALSE)
    message("Sound manifest created: www/cuack/sound_manifest.csv")
    message("Found ", nrow(manifest), " sound files")
    
    return(manifest)
}

# Function to list available sounds
list_available_sounds <- function() {
    if (!dir.exists("www/cuack")) {
        message("Sound directory www/cuack does not exist")
        return(NULL)
    }
    
    sound_files <- list.files("www/cuack", pattern = "\\.(mp3|wav|m4a)$", full.names = FALSE)
    
    if (length(sound_files) == 0) {
        message("No sound files found in www/cuack/")
        return(NULL)
    }
    
    message("Available sound files:")
    for (file in sound_files) {
        taxon_id <- gsub("\\.(mp3|wav|m4a)$", "", file)
        message("  ", file, " (Taxon ID: ", taxon_id, ")")
    }
    
    return(sound_files)
}

# Function to copy sample sounds (if you have them elsewhere)
copy_sample_sounds <- function(source_dir) {
    if (!dir.exists(source_dir)) {
        message("Source directory does not exist: ", source_dir)
        return(FALSE)
    }
    
    source_files <- list.files(source_dir, pattern = "\\.(mp3|wav|m4a)$", full.names = TRUE)
    
    if (length(source_files) == 0) {
        message("No sound files found in source directory")
        return(FALSE)
    }
    
    copied <- 0
    for (file in source_files) {
        dest_file <- file.path("www/cuack", basename(file))
        tryCatch({
            file.copy(file, dest_file, overwrite = FALSE)
            copied <- copied + 1
            message("Copied: ", basename(file))
        }, error = function(e) {
            message("Failed to copy: ", basename(file))
        })
    }
    
    message("Copied ", copied, " sound files")
    return(copied > 0)
}

# Interactive functions
if (interactive()) {
    cat("Bird Sound Manager loaded.\\n")
    cat("Available functions:\\n")
    cat("- create_sound_manifest()     # Create manifest of existing sounds\\n")
    cat("- list_available_sounds()     # List all sound files\\n")
    cat("- copy_sample_sounds(dir)     # Copy sounds from another directory\\n")
    cat("\\n")
    cat("To use: Place .mp3/.wav/.m4a files in www/cuack/ folder\\n")
    cat("Files should be named with taxonID (e.g., '267064.mp3')\\n")
}

# Run manifest creation if script is executed directly
if (!interactive()) {
    cat("Bird Sound Manager\\n")
    cat("==================\\n")
    
    list_available_sounds()
    create_sound_manifest()
}