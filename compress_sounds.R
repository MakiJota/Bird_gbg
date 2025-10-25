#!/usr/bin/env Rscript
##################
## Compress Bird Sound Files — Windows-friendly (fixed escaping)
##################

if (!requireNamespace("av", quietly = TRUE)) install.packages("av")
if (!requireNamespace("future.apply", quietly = TRUE)) install.packages("future.apply")
if (!requireNamespace("parallel", quietly = TRUE)) install.packages("parallel")

library(av)
library(future.apply)
library(parallel)

compress_sound <- function(input_file, output_file = NULL, target_bitrate = 64) {
        if (is.null(output_file)) output_file <- input_file
        tryCatch({
                original_size <- file.size(input_file)
                av_audio_convert(
                        audio = input_file,
                        output = output_file,
                        format = "mp3",
                        channels = 1,
                        sample_rate = 22050,
                        bit_rate = target_bitrate * 1000
                )
                compressed_size <- file.size(output_file)
                list(
                        success = TRUE,
                        file = basename(input_file),
                        reduction = round((1 - compressed_size / original_size) * 100, 1),
                        original_size = original_size,
                        compressed_size = compressed_size
                )
        }, error = function(e) {
                list(success = FALSE, file = basename(input_file), error = e$message)
        })
}

compress_all_sounds <- function(path = "www/cuack", target_bitrate = 64) {
        cat("\n========================================\n")
        cat("  Bird Sound Compressor (Parallel R / Windows)\n")
        cat("========================================\n\n")
        
        # normalize the path (works with forward slashes or backslashes)
        path <- normalizePath(path, winslash = "/", mustWork = FALSE)
        
        if (!dir.exists(path)) stop("Error: directory not found: ", path)
        # NOTE: regex uses double backslash to escape the dot
        sound_files <- list.files(path, pattern = "\\.mp3$", full.names = TRUE)
        if (length(sound_files) == 0) stop("No MP3 files found in ", path)
        
        backup_dir <- file.path(dirname(path), "cuack_backup")
        backup_dir <- normalizePath(backup_dir, winslash = "/", mustWork = FALSE)
        if (!dir.exists(backup_dir)) {
                dir.create(backup_dir, recursive = TRUE)
                cat("Created backup directory:", backup_dir, "\n\n")
        }
        
        # Backup originals (do not overwrite existing backups)
        invisible(lapply(sound_files, function(f) {
                dest <- file.path(backup_dir, basename(f))
                if (!file.exists(dest)) file.copy(f, dest)
        }))
        
        cores_to_use <- max(1, detectCores() - 1)
        cat("Found", length(sound_files), "files → compressing using", cores_to_use, "cores\n")
        cat("Target bitrate:", target_bitrate, "kbps | 22kHz mono\n\n")
        
        plan(multisession, workers = cores_to_use)
        results <- future_lapply(sound_files, function(f) {
                # skip tiny files to save time
                if (file.size(f) < 150 * 1024) {
                        return(list(success = FALSE, file = basename(f), skipped = TRUE))
                }
                compress_sound(f, target_bitrate = target_bitrate)
        })
        plan(sequential)
        
        successful <- Filter(function(x) isTRUE(x$success), results)
        failed <- Filter(function(x) !isTRUE(x$success) && is.null(x$skipped), results)
        skipped <- Filter(function(x) isTRUE(x$skipped), results)
        
        total_orig <- if (length(successful) > 0) sum(sapply(successful, `[[`, "original_size")) else 0
        total_comp <- if (length(successful) > 0) sum(sapply(successful, `[[`, "compressed_size")) else 0
        reduction <- if (total_orig > 0) round((1 - total_comp / total_orig) * 100, 1) else 0
        
        cat("\n========================================\n")
        cat("  Compression Complete\n")
        cat("========================================\n")
        cat("✓ Success:", length(successful), "files\n")
        cat("✗ Failed:", length(failed), "files\n")
        cat("⏭️ Skipped (tiny):", length(skipped), "files\n\n")
        cat("Total reduction:", reduction, "% (",
            round(total_orig / 1e6, 1), "→", round(total_comp / 1e6, 1), "MB)\n", sep = "")
        cat("💾 Backup saved to:", backup_dir, "\n\n")
        
        invisible(results)
}

# --- Run ---
cat("\nBird Sound Compressor — Parallel Mode (Windows)\n")
cat("==============================================\n")
# If you want to use an absolute path, supply it here, e.g.:
# compress_all_sounds("C:/Users/edgardo/OneDrive - Chalmers/Documents/GitHub/BirdMapGothenburg/04_BirdGame/www/cuack")
compress_all_sounds(target_bitrate = 64)
cat("\n✅ All done! Sounds ready for deployment.\n")
