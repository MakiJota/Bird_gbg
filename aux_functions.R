##################
## Auxiliary functions
####################
library(rvest)
library(chromote)
library(purrr)
# Note: bird_df is loaded in global.R


#############

clean_bird <- function(db) {
        db$taxon_number <- sub(".*Taxon:", "", db$taxonID)
        
        
        
        
        #####
        return(db)
}



####
# Generate unique something
#############


get_unique <- function(db, var_obj) {
     
        unique_list <- unique(db[[var_obj]] )  
        unique_list <- as.numeric(unique_list)
        
        
        return(unique_list)
           
}



get_sounds_go_to <- function(taxon_id, delay = 1) {
        # Check if chromote is available in this environment
        if (!isTRUE(getOption("chromote.available", FALSE))) {
                return(NULL)
        }
        
        tryCatch({
                url <- paste0("https://artfakta.se/taxa/", taxon_id, "/information")
                b <- ChromoteSession$new()
                on.exit(b$close(), add = TRUE)
                
                # go_to waits for Page.loadEventFired and then waits `delay` seconds
                b$go_to(url, delay = delay)
                
                # grab rendered outer HTML
                doc <- b$DOM$getDocument()
                html <- b$DOM$getOuterHTML(nodeId = doc$root$nodeId)$outerHTML
                page <- read_html(html)
                
                mp3_links <- page %>% 
                        html_nodes("audio.w-100 source") %>%   # target the <source> inside <audio>
                        html_attr("src")
                mp3_links
        }, error = function(e) {
                # Return NULL if chromote fails (fallback gracefully)
                NULL
        })
}

# Safe wrapper for getting sounds with user feedback
get_sounds_safe <- function(taxon_id) {
        sounds <- get_sounds_go_to(taxon_id)
        if (is.null(sounds) || length(sounds) == 0) {
                return(NULL)
        }
        return(sounds)
}

# Function to get local bird sound file path
get_local_sound <- function(taxon_id) {
        sound_file <- file.path("www", "cuack", paste0(taxon_id, ".mp3"))
        
        # Check if local sound file exists
        if (file.exists(sound_file)) {
                # Return the web-accessible path
                return(paste0("cuack/", taxon_id, ".mp3"))
        }
        
        return(NULL)
}

# Enhanced sound getter that prioritizes local files over scraping
get_sound_url <- function(taxon_id) {
        # First, try to get local sound
        local_sound <- get_local_sound(taxon_id)
        if (!is.null(local_sound)) {
                return(local_sound)
        }
        
        # Fallback to web scraping if no local file
        web_sounds <- get_sounds_safe(taxon_id)
        if (!is.null(web_sounds) && length(web_sounds) > 0) {
                return(web_sounds[1])
        }
        
        return(NULL)
}



get_images_go_to <- function(taxon_id, delay = 1) {
        # Check if chromote is available in this environment
        if (!isTRUE(getOption("chromote.available", FALSE))) {
                return(character(0))
        }
        
        tryCatch({
                url <- paste0("https://artfakta.se/taxa/", taxon_id, "/information")
                b <- ChromoteSession$new()
                on.exit(b$close(), add = TRUE)
                
                # go_to waits for Page.loadEventFired and then waits `delay` seconds
                b$go_to(url, delay = delay)
                
                # grab rendered outer HTML
                doc <- b$DOM$getDocument()
                html <- b$DOM$getOuterHTML(nodeId = doc$root$nodeId)$outerHTML
                page <- read_html(html)
                
                imgs <- page %>% html_nodes("img.img-fluid") %>% html_attr("src")
                imgs
        }, error = function(e) {
                # Return empty vector if chromote fails
                character(0)
        })
}


# Retrieve the rendered HTML for the main content section of the Artfakta information page
# Returns a single character string containing the HTML of <div id="main-content" class="adb-plate-primary"> ...
get_info_main_content <- function(taxon_id, delay = 1) {
        url <- paste0("https://artfakta.se/taxa/", taxon_id, "/information")
        # First try with Chromote (for hydrated content) if available
        html_out <- if (isTRUE(getOption("chromote.available", FALSE))) {
                tryCatch({
                        b <- ChromoteSession$new()
                        on.exit(b$close(), add = TRUE)
                        b$go_to(url, delay = delay)
                        doc <- b$DOM$getDocument()
                        b$DOM$getOuterHTML(nodeId = doc$root$nodeId)$outerHTML
                }, error = function(e) NA_character_)
        } else {
                NA_character_
        }

        extract_main <- function(html) {
                if (is.na(html) || is.null(html) || !nzchar(html)) return(NA_character_)
                page <- read_html(html)
                node <- page %>% html_nodes("div#main-content.adb-plate-primary")
                if (length(node) == 0) return(NA_character_)
                as.character(node[[1]])
        }

        main <- extract_main(html_out)

        # Fallback: direct read_html of the URL if Chromote failed or didn't find the node
        if (is.na(main)) {
                main <- tryCatch({
                        page <- read_html(url)
                        node <- page %>% html_nodes("div#main-content.adb-plate-primary")
                        if (length(node) == 0) NA_character_ else as.character(node[[1]])
                }, error = function(e) NA_character_)
        }

        main
}


# Format Artfakta information with better styling
# Takes raw HTML string from get_info_main_content and wraps it in a styled container
format_artfakta_info <- function(html_content) {
        if (is.na(html_content) || html_content == "") {
                return("<p style='color: #888; font-style: italic;'>No information available</p>")
        }
        
        # Wrap in a styled container with better formatting
        formatted <- paste0(
                "<div class='artfakta-info-wrapper' style='",
                "background: #f8f9fa; ",
                "padding: 20px; ",
                "border-radius: 10px; ",
                "border: 1px solid #dee2e6; ",
                "margin-top: 15px; ",
                "font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif; ",
                "line-height: 1.6;",
                "'>",
                "<style>",
                ".artfakta-info-wrapper h2, .artfakta-info-wrapper h3, .artfakta-info-wrapper h4 { ",
                "  color: #2c3e50; ",
                "  margin-top: 1em; ",
                "  margin-bottom: 0.5em; ",
                "} ",
                ".artfakta-info-wrapper p { ",
                "  margin-bottom: 0.75em; ",
                "  color: #495057; ",
                "} ",
                ".artfakta-info-wrapper ul, .artfakta-info-wrapper ol { ",
                "  margin-left: 20px; ",
                "  margin-bottom: 1em; ",
                "} ",
                ".artfakta-info-wrapper a { ",
                "  color: #007bff; ",
                "  text-decoration: none; ",
                "} ",
                ".artfakta-info-wrapper a:hover { ",
                "  text-decoration: underline; ",
                "} ",
                ".artfakta-info-wrapper img { ",
                "  max-width: 100%; ",
                "  height: auto; ",
                "  border-radius: 8px; ",
                "  margin: 10px 0; ",
                "} ",
                "</style>",
                html_content,
                "</div>"
        )
        
        return(formatted)
}



#####################
# make the images
###############################







make_mosaic <- function(urls, target_height = 150, 
                        max_row_width = 800, spacing = 5, 
                        bg = "white") {
        
        # Step 1: resize images by height
        imgs <- lapply(urls, function(u) {
                img <- image_read(u)
                image_resize(img, paste0("x", target_height))
        })
        
        # Step 2: get widths
        infos <- lapply(imgs, image_info)
        widths <- sapply(infos, function(x) x$width)
        
        # Step 3: group images into rows based on max_row_width
        rows <- list()
        current <- list()
        cum_width <- 0
        
        for (i in seq_along(imgs)) {
                if (cum_width + widths[i] + spacing * length(current) > max_row_width && length(current) > 0) {
                        rows[[length(rows) + 1]] <- current
                        current <- list()
                        cum_width <- 0
                }
                current[[length(current) + 1]] <- imgs[[i]]
                cum_width <- cum_width + widths[i]
        }
        if (length(current) > 0) {
                rows[[length(rows) + 1]] <- current
        }
        
        row_imgs <- lapply(rows, function(r) {
                padded <- list()
                for (j in seq_along(r)) {
                        padded[[length(padded) + 1]] <- r[[j]]
                        if (j < length(r)) {
                                
                                
                                spacer <- image_blank(width = spacing, height = target_height, color = bg)
                                padded[[length(padded) + 1]] <- spacer
                        }
                }
                image_append(image_join(padded))
        })
        
        
        padded_rows <- list()
        for (k in seq_along(row_imgs)) {
                padded_rows[[length(padded_rows) + 1]] <- row_imgs[[k]]
                if (k < length(row_imgs)) {
                        
                        
                        
                        spacer <- image_blank(width = image_info(row_imgs[[k]])$width,
                                              height = spacing, color = bg)
                        padded_rows[[length(padded_rows) + 1]] <- spacer
                }
        }
        
        mosaic <- image_append(image_join(padded_rows), stack = TRUE)
        return(mosaic)
}





save_image <- function(taxo_id, url, folder = "asset") {
        if (!dir.exists(folder)) {
                dir.create(folder, recursive = TRUE)
        }
        
        # handle missing URLs
        if (is.na(url) || url == "") {
                message("TAXONOMY IMAGE NOT FOUND :: ID ", taxo_id)
                return(invisible(NULL))
        }
        
        # filename as taxo_id.jpg
        filename <- paste0(taxo_id, ".jpg")
        destfile <- file.path(folder, filename)
        
        tryCatch(
                {
                        download.file(url, destfile, mode = "wb", quiet = TRUE)
                        message("Downloaded: ", filename)
                },
                error = function(e) message("Failed: ", taxo_id, " -> ", e$message)
        )
        
        invisible(destfile)
}


make_mosaic_center <- function(urls, target_height = 150, 
                               max_row_width = 800, spacing = 5, 
                               bg = "white") {
        
        # Step 1: resize images by height
        imgs <- lapply(urls, function(u) {
                img <- image_read(u)
                image_resize(img, paste0("x", target_height))
        })
        
        # Step 2: get widths
        infos <- lapply(imgs, image_info)
        widths <- sapply(infos, function(x) x$width)
        
        # Step 3: group images into rows based on max_row_width
        rows <- list()
        current <- list()
        cum_width <- 0
        
        for (i in seq_along(imgs)) {
                if (cum_width + widths[i] + spacing * length(current) > max_row_width && length(current) > 0) {
                        rows[[length(rows) + 1]] <- current
                        current <- list()
                        cum_width <- 0
                }
                current[[length(current) + 1]] <- imgs[[i]]
                cum_width <- cum_width + widths[i]
        }
        if (length(current) > 0) {
                rows[[length(rows) + 1]] <- current
        }
        
        # Step 4: build rows and center them
        row_imgs <- lapply(rows, function(r) {
                # Build row with spacing
                padded <- list()
                for (j in seq_along(r)) {
                        padded[[length(padded) + 1]] <- r[[j]]
                        if (j < length(r)) {
                                spacer <- image_blank(width = spacing, height = target_height, color = bg)
                                padded[[length(padded) + 1]] <- spacer
                        }
                }
                row <- image_append(image_join(padded))
                
                # Center by adding left + right padding
                row_width <- image_info(row)$width
                if (row_width < max_row_width) {
                        pad_left <- floor((max_row_width - row_width) / 2)
                        row <- image_border(row, color = bg, geometry = paste0(pad_left, "x0"))
                        row <- image_extent(row, paste0(max_row_width, "x", target_height), 
                                            gravity = "West", color = bg)
                }
                
                row
        })
        
        # Step 5: stack rows with vertical spacing
        padded_rows <- list()
        for (k in seq_along(row_imgs)) {
                padded_rows[[length(padded_rows) + 1]] <- row_imgs[[k]]
                if (k < length(row_imgs)) {
                        spacer <- image_blank(width = max_row_width,
                                              height = spacing, color = bg)
                        padded_rows[[length(padded_rows) + 1]] <- spacer
                }
        }
        
        mosaic <- image_append(image_join(padded_rows), stack = TRUE)
        return(mosaic)
}



