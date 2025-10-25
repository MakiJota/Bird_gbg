        #!/usr/bin/env Rscript
        ##################
        ## Download Bird Sounds from Artfakta
        ## Uses the same scraping logic as the app to get sound URLs
        ## Then downloads them to www/cuack/ folder
        ####################
        
        # Load required libraries and functions
        source("global.R")
        source("aux_functions.R")
        
        # Create the sounds directory
        if (!dir.exists("www/cuack")) {
            dir.create("www/cuack", recursive = TRUE)
            cat("Created directory: www/cuack/\n")
        }
        
        # Function to download a single bird sound
        download_bird_sound <- function(taxon_id, bird_name = NULL) {
            # Check if file already exists
            output_file <- file.path("www/cuack", paste0(taxon_id, ".mp3"))
            
            if (file.exists(output_file)) {
                cat("  ✓ Already exists: ", taxon_id, ".mp3\n", sep = "")
                return(TRUE)
            }
            
            # Get sound URLs from Artfakta
            cat("  Fetching sound URL for taxon ID:", taxon_id, "\n")
            sound_urls <- get_sounds_go_to(taxon_id)
            
            if (is.null(sound_urls) || length(sound_urls) == 0) {
                cat("  ✗ No sound found for:", taxon_id, "\n")
                return(FALSE)
            }
            
            # Use the first sound URL
            sound_url <- sound_urls[1]
            cat("  Found URL:", sound_url, "\n")
            
            # Download the sound file
            tryCatch({
                download.file(
                    url = sound_url,
                    destfile = output_file,
                    mode = "wb",
                    quiet = TRUE
                )
                
                # Verify download
                if (file.exists(output_file) && file.size(output_file) > 0) {
                    size_kb <- round(file.size(output_file) / 1024, 1)
                    cat("  ✓ Downloaded:", taxon_id, ".mp3 (", size_kb, "KB)\n", sep = "")
                    return(TRUE)
                } else {
                    cat("  ✗ Download failed:", taxon_id, "\n")
                    return(FALSE)
                }
                
            }, error = function(e) {
                cat("  ✗ Error:", taxon_id, "-", e$message, "\n")
                return(FALSE)
            })
        }
        
        # Main download function
        download_all_sounds <- function() {
            cat("\n")
            cat("========================================\n")
            cat("  Bird Sound Downloader\n")
            cat("========================================\n\n")
            
            # Get list of birds from game_birds
            if (!exists("game_birds") || nrow(game_birds) == 0) {
                cat("Error: No birds found in game_birds dataset\n")
                return(NULL)
            }
            
            total_birds <- nrow(game_birds)
            cat("Total birds to process:", total_birds, "\n\n")
            
            # Track progress
            successful <- 0
            failed <- 0
            skipped <- 0
            
            # Process each bird
            for (i in 1:total_birds) {
                taxon_id <- game_birds$taxonID[i]
                bird_name <- game_birds$scientificName[i]
                
                cat("[", i, "/", total_birds, "] ", bird_name, " (", taxon_id, ")\n", sep = "")
                
                # Check if already downloaded
                if (file.exists(file.path("www/cuack", paste0(taxon_id, ".mp3")))) {
                    cat("  ✓ Already exists\n")
                    skipped <- skipped + 1
                } else {
                    # Download
                    result <- download_bird_sound(taxon_id, bird_name)
                    
                    if (result) {
                        successful <- successful + 1
                    } else {
                        failed <- failed + 1
                    }
                    
                    # Small delay to be respectful to the server
                    Sys.sleep(2)
                }
                
                cat("\n")
            }
            
            # Summary
            cat("========================================\n")
            cat("  Download Complete!\n")
            cat("========================================\n")
            cat("✓ Successful downloads:", successful, "\n")
            cat("○ Already existed:", skipped, "\n")
            cat("✗ Failed downloads:", failed, "\n")
            cat("📁 Location: www/cuack/\n")
            cat("\nTotal sound files:", successful + skipped, "/", total_birds, "\n")
            
            # List downloaded files
            sound_files <- list.files("www/cuack", pattern = "\\.mp3$")
            cat("\nSound files ready for deployment:", length(sound_files), "\n")
            
            return(list(
                successful = successful,
                skipped = skipped,
                failed = failed,
                total = total_birds
            ))
        }
        
        # Run the download
        cat("\nStarting download process...\n")
        cat("This may take a while depending on the number of birds.\n")
        cat("Press Ctrl+C to cancel at any time.\n\n")
        
        results <- download_all_sounds()
        
        cat("\n✅ Done! You can now deploy the app with local sound files.\n")
        
        
        
        
