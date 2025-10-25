#!/usr/bin/env Rscript
##################
## Test and Run Sound Downloader
####################

# Test if we can run the sound downloader
cat("Testing Bird Sound Downloader\n")
cat("============================\n")

# Check if we're in the right directory
if (!file.exists("get_sounds.R")) {
    cat("Error: get_sounds.R not found. Make sure you're in the 04_BirdGame directory.\n")
    quit(status = 1)
}

# Source the downloader
cat("Loading sound downloader...\n")
source("get_sounds.R")

# Test with a single bird first
cat("\nTesting with a single bird...\n")
if (nrow(game_birds) > 0) {
    test_bird <- game_birds[1, ]
    cat("Test bird:", test_bird$scientificName, "(ID:", test_bird$taxonID, ")\n")
    
    # Test the sound scraping function
    cat("Testing sound scraping...\n")
    test_sounds <- get_sounds_go_to(test_bird$taxonID)
    
    if (!is.null(test_sounds) && length(test_sounds) > 0) {
        cat("✓ Found", length(test_sounds), "sound(s) for test bird\n")
        cat("  URL:", test_sounds[1], "\n")
        
        # Try downloading one sound
        cat("Testing download...\n")
        result <- download_bird_sound(test_bird$taxonID, test_sounds[1])
        
        if (!is.null(result)) {
            cat("✓ Test download successful!\n")
            cat("Ready to download all sounds.\n")
        } else {
            cat("✗ Test download failed.\n")
        }
    } else {
        cat("✗ No sounds found for test bird. Check chromote setup.\n")
    }
} else {
    cat("✗ No birds found in game_birds dataset.\n")
}

cat("\nTo download all sounds, run:\n")
cat("source('get_sounds.R')\n")
cat("download_all_bird_sounds()\n")