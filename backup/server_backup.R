server <- function(input, output, session) {
    # Source auxiliary functions and set up resources
    source("../01_scripts/aux_functions.R")
    if (requireNamespace("dplyr", quietly = TRUE)) {
        library(dplyr)
    }
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        library(ggplot2)
    }
    # Serve bird images if folder exists
    image_dir <- normalizePath(file.path("..","02_ShinyBird","00_assets","bird_pics"), winslash = "\\", mustWork = FALSE)
    if (dir.exists(image_dir)) {
        addResourcePath("birdpics", image_dir)
    }

    # Helper to create safe input IDs from arbitrary strings
    safe_id <- function(x) {
        gsub("[^A-Za-z0-9_]", "_", as.character(x))
    }
    
    # Game state reactive values
    rv <- reactiveValues(
        cards = NULL,
        flipped = c(),
        matched = c(),
        moves = 0,
        start_time = NULL,
        selected_bird = NULL,
        game_active = FALSE,
        exploration_mode = FALSE
    )
    
    # Timer
    observe({
        invalidateLater(1000)
        if (!is.null(rv$start_time) && rv$game_active) {
            rv$elapsed <- difftime(Sys.time(), rv$start_time, units = "secs")
        }
    })

    # Initialize new game
    observeEvent(input$start_game, {
        # Reset game state
        rv$matched <- c()
        rv$flipped <- c()
        rv$moves <- 0
        rv$start_time <- Sys.time()
        rv$game_active <- TRUE
        rv$exploration_mode <- FALSE
        
        # Get number of pairs based on difficulty
        n_pairs <- as.numeric(input$difficulty)
        
        # Select random birds for the game
        n_pairs <- min(n_pairs, nrow(game_birds))
        sample_idx <- sample(nrow(game_birds), n_pairs)
        selected_birds <- game_birds[sample_idx, , drop = FALSE]
        
        # Create pairs of cards and shuffle
        cards <- bind_rows(selected_birds, selected_birds)
        cards$card_id <- seq_len(nrow(cards))
        cards$order <- sample(nrow(cards))
        rv$cards <- cards[order(cards$order), ]
    })
    
    # Render memory cards (only on game start or reset)
    output$memory_cards <- renderUI({
        req(rv$cards)
        n_cards <- nrow(rv$cards)
        # Use 6 columns for 8/12/16 pairs; otherwise 4
        n_pairs <- n_cards / 2
        n_cols <- if (n_pairs >= 8) 6 else 4
        n_rows <- ceiling(n_cards / n_cols)
        n_pairs <- n_cards / 2
        
        # Wrap with difficulty class for responsive sizing
        div(class = paste0("difficulty-", n_pairs),
            lapply(1:n_rows, function(row) {
                start_idx <- (row - 1) * n_cols + 1
                end_idx <- min(row * n_cols, n_cards)
                
                div(class = "row",
                    lapply(start_idx:end_idx, function(i) {
                        card <- rv$cards[i, ]
                        is_matched <- card$card_id %in% rv$matched
                        
                        div(class = paste0("col-", floor(12/n_cols)),
                            div(id = paste0("card-", card$card_id),
                                class = paste("game-card position-relative",
                                             if(is_matched) "matched flipped" else ""),
                                onClick = sprintf(
                                    "Shiny.setInputValue('flip_card', %d, {priority: 'event'});",
                                    card$card_id
                                ),
                                div(class = "card-inner",
                                    div(class = "card-front",
                                        icon("feather")
                                    ),
                                    div(class = "card-back",
                                        img(src = file.path("/birdpics", paste0(card$taxonID, ".jpg")))
                                    )
                                ),
                                # Overlay for matched cards
                                if (is_matched) tagList(
                                    div(
                                        style="position:absolute;top:12px;left:12px;color:#fff;font-size:0.9em;background:rgba(0,123,255,0.95);padding:3px 8px;border-radius:4px;white-space:nowrap;font-weight:bold;z-index:10;",
                                        card$scientificName
                                    ),
                                    actionButton(
                                        inputId = paste0("memory_play_sound_", card$taxonID),
                                        label = tagList(icon("play"), "Play"),
                                        class = "btn btn-sm btn-primary position-absolute",
                                        style = "bottom:12px;left:12px;z-index:10;"
                                    )
                                )
                            )
                        )
                    })
                )
            })
        )
    })
    
    # Handle card flips
    observeEvent(input$flip_card, {
        cat("Flip card triggered:", input$flip_card, "\n")
        req(rv$game_active)
        card_id <- input$flip_card
        cat("Card ID:", card_id, "Game active:", rv$game_active, "\n")
        cat("Currently flipped:", rv$flipped, "Matched:", rv$matched, "\n")
        
        # Ignore if card is already matched or if two cards are already flipped
        if (!(card_id %in% rv$matched) && length(rv$flipped) < 2 && !(card_id %in% rv$flipped)) {
            cat("Flipping card", card_id, "\n")
            # Flip card using client-side JS (no re-render)
            session$sendCustomMessage("addClass", list(id = paste0("card-", card_id), class = "flipped"))
            rv$flipped <- c(rv$flipped, card_id)
            
            # Update selected bird info
            selected_card <- rv$cards[rv$cards$card_id == card_id, ]
            rv$selected_bird <- selected_card
            cat("Selected bird:", selected_card$scientificName, "\n")
            
            # Check for match when two cards are flipped
            if (length(rv$flipped) == 2) {
                rv$moves <- rv$moves + 1
                cat("Two cards flipped, checking match\n")
                
                # Get the two flipped cards
                cards <- rv$cards[rv$cards$card_id %in% rv$flipped, ]
                
                # Check if they match
                if (cards$taxonID[1] == cards$taxonID[2]) {
                    cat("Match found!\n")
                    # Match found - mark as matched client-side
                    session$sendCustomMessage("addClass", list(id = paste0("card-", rv$flipped[1]), class = "matched"))
                    session$sendCustomMessage("addClass", list(id = paste0("card-", rv$flipped[2]), class = "matched"))
                    
                    # Add overlays to both matched cards using custom message
                    matched_card <- rv$cards[rv$cards$card_id == rv$flipped[1], ]
                    overlay_html <- sprintf('
                        <div style="position:absolute;top:12px;left:12px;color:#fff;font-size:0.9em;background:rgba(0,123,255,0.95);padding:3px 8px;border-radius:4px;white-space:nowrap;font-weight:bold;z-index:10;">%s</div>
                        <button id="memory_play_sound_%s" class="btn btn-sm btn-primary position-absolute action-button shiny-bound-input" style="bottom:12px;left:12px;z-index:10;" type="button"><i class="fa fa-play" role="presentation" aria-label="play icon"></i> Play</button>
                    ', matched_card$scientificName, matched_card$taxonID)
                    
                    session$sendCustomMessage("addOverlay", list(id = paste0("card-", rv$flipped[1]), html = overlay_html))
                    session$sendCustomMessage("addOverlay", list(id = paste0("card-", rv$flipped[2]), html = overlay_html))
                    
                    rv$matched <- c(rv$matched, rv$flipped)
                    rv$flipped <- c()
                    
                    # Check if game is complete
                    if (length(rv$matched) == nrow(rv$cards)) {
                        session$sendCustomMessage("birdConfetti", list())
                        showNotification(
                            sprintf("🎉 Congratulations! You won in %d moves and %d seconds!", 
                                    rv$moves, round(as.numeric(rv$elapsed))),
                            type = "message",
                            duration = 5
                        )
                        rv$game_active <- FALSE
                        rv$exploration_mode <- TRUE
                    }
                } else {
                    cat("No match, flipping back\n")
                    # No match - flip cards back after delay (client-side)
                    later::later(function() {
                        session$sendCustomMessage("removeClass", list(id = paste0("card-", isolate(rv$flipped[1])), class = "flipped"))
                        session$sendCustomMessage("removeClass", list(id = paste0("card-", isolate(rv$flipped[2])), class = "flipped"))
                        rv$flipped <- c()
                    }, 1)
                }
            }
        }
    })
    
    # Play bird sound
    observeEvent(input$play_sound, {
        req(rv$selected_bird)
        
        # Get sound URL using get_sounds_go_to function
        sound_urls <- get_sounds_go_to(rv$selected_bird$taxonID)
        if(length(sound_urls) > 0) {
            sound_url <- sound_urls[1]
            runjs(sprintf("
                var audio = document.getElementById('bird_sound');
                audio.src = '%s';
                audio.play();
            ", sound_url))
        }
    })

    # Stop bird sound (memory tab)
    observeEvent(input$stop_sound, {
        runjs("
            var audio = document.getElementById('bird_sound');
            if(audio){ audio.pause(); audio.currentTime = 0; }
        ")
    })
    
    # Play sound from matched memory card overlay
    observe({
        req(rv$cards)
        cards_df <- rv$cards
        # Get unique taxon IDs (same bird appears twice in memory game)
        unique_taxon_ids <- unique(cards_df$taxonID)
        lapply(unique_taxon_ids, function(taxon_id) {
            observeEvent(input[[paste0("memory_play_sound_", taxon_id)]], {
                cat("Memory play button clicked for taxonID:", taxon_id, "\n")
                urls <- get_sounds_go_to(taxon_id)
                cat("Sound URLs retrieved:", length(urls), "\n")
                if (length(urls) > 0) {
                    cat("Playing sound:", urls[1], "\n")
                    runjs(sprintf(
                        "var audio = document.getElementById('bird_sound');audio.src = '%s';audio.play();",
                        urls[1]
                    ))
                }
            })
        })
    })
    
    # Display game stats
    output$moves_count <- renderText({
        sprintf("Moves: %d", rv$moves)
    })
    
    output$pairs_found <- renderText({
        req(rv$cards)
        sprintf("Pairs found: %d/%d", length(rv$matched)/2, nrow(rv$cards)/2)
    })
    
    output$time_elapsed <- renderText({
        if (!is.null(rv$elapsed)) {
            sprintf("Time: %d seconds", round(as.numeric(rv$elapsed)))
        }
    })
    
    # Display selected bird info (title)
    output$selected_bird_name <- renderText({
        if (!is.null(rv$selected_bird)) {
            rv$selected_bird$scientificName
        } else {
            "No bird selected"
        }
    })

    # Selected bird statistics panel (memory tab)
    output$bird_statistics <- renderUI({
        req(rv$selected_bird)
        bd <- tryCatch({
            bird_df %>% filter(taxonID == rv$selected_bird$taxonID)
        }, error = function(e) NULL)
        if (is.null(bd) || nrow(bd) == 0) {
            return(NULL)  # Don't show anything if no data
        }
        # Ensure dates parse
        if (!inherits(bd$eventDate, "Date")) {
            suppressWarnings({ bd$eventDate <- as.Date(bd$eventDate) })
        }
        first_seen <- min(bd$eventDate, na.rm = TRUE)
        last_seen  <- max(bd$eventDate, na.rm = TRUE)
        top_sites <- names(sort(table(bd$siteName), decreasing = TRUE)[1:min(3, length(unique(bd$siteName)))])
        tagList(
            div(HTML(paste0("<b>Observations:</b> ", nrow(bd)))),
            if (is.finite(first_seen)) div(HTML(paste0("<b>First seen:</b> ", format(first_seen, "%B %d, %Y")))) ,
            if (is.finite(last_seen))  div(HTML(paste0("<b>Last seen:</b> ",  format(last_seen,  "%B %d, %Y")))) ,
            if (length(top_sites) > 0) div(HTML(paste0("<b>Common sites:</b> ", paste(top_sites, collapse = ", "))))
        )
    })

    # Cache for info HTML to avoid repeated fetches
    info_cache <- reactiveValues(map = list())
    get_cached_info <- function(tid) {
        key <- as.character(tid)
        cat("==== INFO RETRIEVAL DEBUG ====\n")
        cat("Taxon ID:", key, "\n")
        
        if (!is.null(info_cache$map[[key]])) {
            cat("Returning cached info for taxon", key, "\n")
            cat("Cached content length:", nchar(info_cache$map[[key]]), "characters\n")
            return(info_cache$map[[key]])
        }
        
        cat("Fetching new info for taxon", key, "\n")
        html <- tryCatch({
            result <- get_info_main_content(key, delay = 1)
            cat("Fetch result - is.na:", is.na(result), "is.null:", is.null(result), "\n")
            if (!is.na(result) && !is.null(result)) {
                cat("Fetched content length:", nchar(result), "characters\n")
            }
            result
        }, error = function(e) {
            cat("ERROR in get_info_main_content:", conditionMessage(e), "\n")
            NA_character_
        })
        
        info_cache$map[[key]] <- html
        cat("==== END DEBUG ====\n")
        html
    }

    # Filter Artfakta HTML: remove nav and layout columns, keep main content
    filter_artfakta_content <- function(html_str) {
        if (is.null(html_str) || is.na(html_str) || !nzchar(html_str)) return(html_str)
        doc <- tryCatch(read_html(html_str), error = function(e) NULL)
        if (is.null(doc)) return(html_str)

        # Scope to the site's main content wrapper if present
        main <- tryCatch(rvest::html_element(doc, "div#main-content.adb-plate-primary"), error = function(e) NULL)
        if (is.null(main)) {
            main <- xml2::xml_root(doc)
        }

        # Remove the top navigation tabs inside main
        suppressWarnings({
            nav_tabs <- rvest::html_elements(main, "nav[role='tablist']")
            if (length(nav_tabs) > 0) {
                for (elem in nav_tabs) xml2::xml_remove(elem)
            }
        })

        # Prefer the main text column if present; else keep 'main'
        primary_col <- tryCatch(rvest::html_element(main, "div.col-xl-8.order-last.order-lg-first"), error = function(e) NULL)
        content_node <- if (!is.null(primary_col)) primary_col else main

        # Remove any hover nav spans within the selected content
        suppressWarnings({
            hover_nav <- rvest::html_elements(content_node, "span.text-decoration-underline-hover")
            if (length(hover_nav) > 0) {
                for (elem in hover_nav) xml2::xml_remove(elem)
            }
        })

        # Remove app-main-image components to avoid large hero image blocks
        suppressWarnings({
            hero <- rvest::html_elements(content_node, "app-main-image")
            if (length(hero) > 0) {
                for (elem in hero) xml2::xml_remove(elem)
            }
        })

        # Return the cleaned content node only (excludes sidebars like .col-xl-4)
        as.character(content_node)
    }

    # Helper to remove app-main-image but otherwise keep full content (for Browser tab)
    remove_app_main_image <- function(html_str) {
        if (is.null(html_str) || is.na(html_str) || !nzchar(html_str)) return(html_str)
        doc <- tryCatch(read_html(html_str), error = function(e) NULL)
        if (is.null(doc)) return(html_str)
        root <- xml2::xml_root(doc)
        suppressWarnings({
            hero <- rvest::html_elements(root, "app-main-image")
            if (length(hero) > 0) for (elem in hero) xml2::xml_remove(elem)
        })
        as.character(root)
    }

    # Extract title as "<vernacular> <scientific> (<author>)" from Artfakta HTML when available
    extract_artfakta_title <- function(html_str, fallback = NULL) {
        if (is.null(html_str) || is.na(html_str) || !nzchar(html_str)) return(fallback %||% "")
        doc <- tryCatch(read_html(html_str), error = function(e) NULL)
        if (is.null(doc)) return(fallback %||% "")
        get_text <- function(node) if (length(node)) trimws(xml2::xml_text(node)) else ""
        vern <- tryCatch(get_text(rvest::html_element(doc, "span.me-2 > div.capitalize")), error=function(e) "")
        sci  <- tryCatch(get_text(rvest::html_element(doc, "em.me-2.text-break")), error=function(e) "")
        auth <- tryCatch(get_text(rvest::html_element(doc, "span.fs-5")), error=function(e) "")
        parts <- c(vern, sci, auth)
        parts <- parts[nzchar(parts)]
        if (length(parts) == 0) return(fallback %||% "")
        paste(parts, collapse = " ")
    }

    `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

    # Render Artfakta main-content for selected bird (memory tab)
    output$selected_bird_info_html <- renderUI({
        req(rv$selected_bird)
        cat("\n>>> Rendering selected_bird_info_html for:", rv$selected_bird$scientificName, "taxonID:", rv$selected_bird$taxonID, "\n")
        
        html <- get_cached_info(rv$selected_bird$taxonID)
        
        cat("After get_cached_info - is.na:", is.na(html), "is.null:", is.null(html), "\n")
        if (!is.na(html) && !is.null(html)) {
            cat("HTML length before filtering:", nchar(html), "\n")
        }
        
        if (is.na(html) || is.null(html) || !nzchar(html)) {
            cat("INFO EMPTY - showing iframe fallback\n")
            # Fallback: embed Artfakta page if scraping fails
            url <- paste0("https://artfakta.se/taxa/", rv$selected_bird$taxonID, "/information")
            return(tags$div(
                class = "mt-2",
                tags$div(class = "text-muted", "Live info (embedded from Artfakta):"),
                tags$iframe(src = url, style = "width:100%;height:420px;border:1px solid #eee;border-radius:8px;")
            ))
        }
        
        # Lightly clean and format the HTML (remove nav/sidebars)
        cat("Cleaning content (remove nav/sidebars)\n")
        html <- filter_artfakta_content(html)
        title <- extract_artfakta_title(html, fallback = rv$selected_bird$scientificName)
        tagList(
            h5(title),
            HTML(format_artfakta_info(html))
        )
    })

    # Name-Image Matching Game Logic
    rv_name_matching <- reactiveValues(
        birds = NULL,
        selected_name = NULL,
        selected_image = NULL,
        selected_bird = NULL,
        matches = 0,
        total = 0,
        matched_ids = character(0),
        order_images = NULL,
        order_names = NULL
    )

    # Initialize name-image matching game
    observeEvent(input$start_name_matching, {
        # Select random birds
        n_birds <- 6  # Number of birds to match
        available_birds <- game_birds
        selected_indices <- sample(nrow(available_birds), min(n_birds, nrow(available_birds)))
        rv_name_matching$birds <- available_birds[selected_indices, ]
        rv_name_matching$matches <- 0
        rv_name_matching$total <- length(selected_indices)
        rv_name_matching$selected_name <- NULL
        rv_name_matching$selected_image <- NULL
        rv_name_matching$selected_bird <- NULL
        rv_name_matching$matched_ids <- character(0)
        # Freeze list and grid order so images don't move on match
        rv_name_matching$order_images <- sample(seq_len(nrow(rv_name_matching$birds)))
        rv_name_matching$order_names  <- sample(seq_len(nrow(rv_name_matching$birds)))
    })

    # Render bird names list
    output$name_matching_list <- renderUI({
        cat("\n>>> RE-RENDERING name_matching_list at", as.character(Sys.time()), "\n")
        birds_df <- req(rv_name_matching$birds)
        if (is.null(rv_name_matching$order_names)) {
            rv_name_matching$order_names <- seq_len(nrow(birds_df))
        }
        names_order <- rv_name_matching$order_names
        div(class = "list-group",
            lapply(names_order, function(i) {
                id <- birds_df$taxonID[i]
                actionButton(
                    inputId = paste0("name_", id),
                    label = birds_df$scientificName[i],
                    class = "list-group-item list-group-item-action text-start"
                )
            })
        )
    }) %>% bindEvent(rv_name_matching$birds, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Render bird images grid
    output$image_matching_grid <- renderUI({
        cat("\n>>> RE-RENDERING image_matching_grid at", as.character(Sys.time()), "\n")
        birds_df <- req(rv_name_matching$birds)
        if (is.null(rv_name_matching$order_images)) {
            rv_name_matching$order_images <- seq_len(nrow(birds_df))
        }
        images_order <- rv_name_matching$order_images
        div(class = "row",
            lapply(images_order, function(i) {
                id <- birds_df$taxonID[i]
                # Don't check matched_ids here to avoid re-render on match
                div(class = "col-md-4 mb-3 position-relative",
                    id = paste0("name_image_container_", id),
                    actionButton(
                        inputId = paste0("image_", id),
                        label = tags$img(
                            src = file.path("/birdpics", paste0(id, ".jpg")),
                            class = "square-img",
                            style = "max-width: 300px; max-height: 300px;"
                        ),
                        class = "btn p-0"
                    )
                )
            })
        )
    }) %>% bindEvent(rv_name_matching$birds, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Play button for matched images in Name Matching (bottom-left overlay)
    observe({
        req(rv_name_matching$birds)
        birds_df <- rv_name_matching$birds
        lapply(birds_df$taxonID, function(id) {
            observeEvent(input[[paste0("name_play_matched_sound_", id)]], {
                urls <- get_sounds_go_to(id)
                if (length(urls) > 0) {
                    runjs(sprintf(
                        "var audio = document.getElementById('bird_sound');audio.src = '%s';audio.play();",
                        urls[1]
                    ))
                }
            })
        })
    })

    # Handle name selection
    observe({
        req(rv_name_matching$birds)
        birds_df <- rv_name_matching$birds
        
        lapply(birds_df$taxonID, function(id) {
            observeEvent(input[[paste0("name_", id)]], {
                rv_name_matching$selected_name <- id
                check_name_image_match()
            })
        })
    })

    # Handle image selection
    observe({
        req(rv_name_matching$birds)
        birds_df <- rv_name_matching$birds
        
        lapply(birds_df$taxonID, function(id) {
            observeEvent(input[[paste0("image_", id)]], {
                rv_name_matching$selected_image <- id
                check_name_image_match()
            })
        })
    })

    # Check for matches in name-image game
    check_name_image_match <- function() {
        if (!is.null(rv_name_matching$selected_name) && 
            !is.null(rv_name_matching$selected_image)) {
            if (rv_name_matching$selected_name == rv_name_matching$selected_image) {
                cat("\n>>> MATCH FOUND in name_matching at", as.character(Sys.time()), "\n")
                # Match found
                rv_name_matching$matches <- rv_name_matching$matches + 1
                cat(">>> Updated matches to:", rv_name_matching$matches, "\n")
                # Mark as matched
                if (is.null(rv_name_matching$matched_ids)) rv_name_matching$matched_ids <- character(0)
                rv_name_matching$matched_ids <- unique(c(rv_name_matching$matched_ids, rv_name_matching$selected_name))
                
                # Get matched bird info
                matched_id <- rv_name_matching$selected_name
                matched_bird <- rv_name_matching$birds[rv_name_matching$birds$taxonID == matched_id, ]
                
                # Dynamically add overlay to matched image (like Memory game)
                overlay_html <- as.character(tagList(
                    # Name overlay (top-left)
                    div(
                        style="position:absolute;top:12px;left:12px;color:#fff;font-size:0.9em;background:rgba(0,123,255,0.95);padding:3px 8px;border-radius:4px;white-space:nowrap;font-weight:bold;",
                        matched_bird$scientificName
                    ),
                    # Play button (bottom-left)
                    actionButton(
                        inputId = paste0("name_play_matched_sound_", matched_id),
                        label = tagList(icon("play"), "Play"),
                        class = "btn btn-sm btn-primary position-absolute",
                        style = "bottom:12px;left:12px;z-index:2;"
                    )
                ))
                
                session$sendCustomMessage("addOverlay", list(
                    id = paste0("name_image_container_", matched_id),
                    html = overlay_html
                ))

                # Disable and grey-out the matched name to avoid re-render flicker
                session$sendCustomMessage('addClass', list(id = paste0('name_', matched_id), class = 'disabled-item'))
                runjs(sprintf("$('#%s').prop('disabled', true);", paste0('name_', matched_id)))
                
                # Set selected bird after a tiny delay to avoid stacking reflows with overlay injection
                later::later(function() {
                    rv_name_matching$selected_bird <- matched_bird
                }, delay = 0.05)
                
                if (rv_name_matching$matches == rv_name_matching$total) {
                    session$sendCustomMessage("birdConfetti", list())
                    showNotification(
                        "🎉 Congratulations! You've matched all the birds!",
                        type = "message",
                        duration = 5
                    )
                }
            }
            # Reset selections
            rv_name_matching$selected_name <- NULL
            rv_name_matching$selected_image <- NULL
        }
    }

    # Display name matching score
    output$name_matching_score <- renderText({
        req(rv_name_matching$total)
        sprintf("Matches: %d/%d", rv_name_matching$matches, rv_name_matching$total)
    })

    # Name match bird stats panel
    output$name_match_bird_stats <- renderUI({
        req(rv_name_matching$selected_bird)
        
        html <- get_cached_info(rv_name_matching$selected_bird$taxonID)
        if (is.na(html) || is.null(html) || !nzchar(html)) {
            # Fallback: embed the Artfakta page if scraping returned nothing
            url <- paste0("https://artfakta.se/taxa/", rv_name_matching$selected_bird$taxonID, "/information")
            return(tagList(
                h5(rv_name_matching$selected_bird$scientificName),
                div(class="text-muted", "Live info (embedded from Artfakta):"),
                tags$iframe(src = url, style = "width:100%;height:360px;border:1px solid #eee;border-radius:8px;")
            ))
        }
        # Lightly clean and format the HTML (remove nav/sidebars)
        html <- filter_artfakta_content(html)
        title <- extract_artfakta_title(html, fallback = rv_name_matching$selected_bird$scientificName)
        tagList(
            h5(title),
            HTML(format_artfakta_info(html))
        )
    })

    # Handle name-match play sound
    observeEvent(input$name_match_play_sound, {
        req(rv_name_matching$selected_bird)
        
        sound_urls <- get_sounds_go_to(rv_name_matching$selected_bird$taxonID)
        if(length(sound_urls) > 0) {
            sound_url <- sound_urls[1]
            runjs(sprintf("
                var audio = document.getElementById('bird_sound');
                audio.src = '%s';
                audio.play();
            ", sound_url))
        }
    })

    observeEvent(input$name_match_stop_sound, {
        runjs("
            var audio = document.getElementById('bird_sound');
            if(audio){ audio.pause(); audio.currentTime = 0; }
        ")
    })

    # Sound-Image Matching Game Logic
    rv_sound_matching <- reactiveValues(
        birds = NULL,
        selected_sound = NULL,
        selected_image = NULL,
        selected_bird = NULL,
        matches = 0,
        total = 0,
        matched_ids = character(0),
        order = NULL,
        order_buttons = NULL
    )

    observeEvent(input$start_sound_matching, {
        available_birds <- game_birds
        n_birds <- min(6, nrow(available_birds))
        idx <- sample(nrow(available_birds), n_birds)
        rv_sound_matching$birds <- available_birds[idx, ]
        rv_sound_matching$matches <- 0
        rv_sound_matching$total <- n_birds
        rv_sound_matching$selected_sound <- NULL
        rv_sound_matching$selected_image <- NULL
        rv_sound_matching$selected_bird <- NULL
        rv_sound_matching$matched_ids <- character(0)
        rv_sound_matching$order <- NULL  # Reset order to trigger new shuffle
        rv_sound_matching$order_buttons <- NULL
    })

    output$sound_matching_buttons <- renderUI({
        cat("\n>>> RE-RENDERING sound_matching_buttons at", as.character(Sys.time()), "\n")
        birds_df <- req(rv_sound_matching$birds)
        # Freeze order of buttons to avoid reordering/flicker
        if (is.null(rv_sound_matching$order_buttons)) {
            rv_sound_matching$order_buttons <- sample(nrow(birds_df))
        }
        order <- rv_sound_matching$order_buttons
        div(class = "list-group",
            lapply(order, function(i) {
                actionButton(
                    inputId = paste0("sound_button_", birds_df$taxonID[i]),
                    label = tagList(icon("volume-up"), birds_df$scientificName[i]),
                    class = "list-group-item list-group-item-action"
                )
            })
        )
    }) %>% bindEvent(rv_sound_matching$birds, ignoreNULL = TRUE, ignoreInit = FALSE)

    output$sound_matching_images <- renderUI({
        cat("\n>>> RE-RENDERING sound_matching_images at", as.character(Sys.time()), "\n")
        birds_df <- req(rv_sound_matching$birds)
        # Freeze order like Name Matching
        if (is.null(rv_sound_matching$order)) {
            rv_sound_matching$order <- sample(nrow(birds_df))
        }
        order <- rv_sound_matching$order
        div(class = "row",
            lapply(order, function(i) {
                id <- birds_df$taxonID[i]
                # Don't check matched_ids here to avoid re-render on match
                div(class = "col-md-4 mb-3 position-relative",
                    id = paste0("sound_image_container_", id),
                    actionButton(
                        inputId = paste0("sound_image_", id),
                        label = tags$img(
                            src = file.path("/birdpics", paste0(id, ".jpg")),
                            class = "square-img",
                            style = "max-width: 300px; max-height: 300px;"
                        ),
                        class = "btn p-0"
                    )
                )
            })
        )
    }) %>% bindEvent(rv_sound_matching$birds, ignoreNULL = TRUE, ignoreInit = FALSE)

    observe({
        req(rv_sound_matching$birds)
        birds_df <- rv_sound_matching$birds
        lapply(birds_df$taxonID, function(id) {
            observeEvent(input[[paste0("play_matched_sound_", id)]], {
                urls <- get_sounds_go_to(id)
                if (length(urls)>0) {
                    runjs(sprintf(
                        "var audio = document.getElementById('bird_sound');audio.src = '%s';audio.play();",
                        urls[1]))
                }
            })
        })
    })

    observe({
        req(rv_sound_matching$birds)
        birds_df <- rv_sound_matching$birds
        lapply(birds_df$taxonID, function(id) {
            observeEvent(input[[paste0("sound_button_", id)]], {
                rv_sound_matching$selected_sound <- id
                sound_urls <- get_sounds_go_to(id)
                if (length(sound_urls) > 0) {
                    runjs(sprintf("
                        var audio = document.getElementById('bird_sound');
                        audio.src = '%s';
                        audio.play();
                    ", sound_urls[1]))
                }
                check_sound_image_match()
            })
        })
    })

    observe({
        req(rv_sound_matching$birds)
        birds_df <- rv_sound_matching$birds
        lapply(birds_df$taxonID, function(id) {
            observeEvent(input[[paste0("sound_image_", id)]], {
                rv_sound_matching$selected_image <- id
                check_sound_image_match()
            })
        })
    })

    check_sound_image_match <- function() {
        if (!is.null(rv_sound_matching$selected_sound) && !is.null(rv_sound_matching$selected_image)) {
            if (rv_sound_matching$selected_sound == rv_sound_matching$selected_image) {
                rv_sound_matching$matches <- rv_sound_matching$matches + 1
                if (is.null(rv_sound_matching$matched_ids)) rv_sound_matching$matched_ids <- character(0)
                rv_sound_matching$matched_ids <- unique(c(rv_sound_matching$matched_ids, rv_sound_matching$selected_sound))
                
                # Get matched bird info
                matched_id <- rv_sound_matching$selected_sound
                matched_bird <- rv_sound_matching$birds[rv_sound_matching$birds$taxonID == matched_id, ]
                
                # Dynamically add overlay to matched image (like Memory and Name Matching)
                overlay_html <- as.character(tagList(
                    # Name overlay (top-left)
                    div(
                        style="position:absolute;top:12px;left:12px;color:#fff;font-size:0.9em;background:rgba(0,123,255,0.95);padding:3px 8px;border-radius:4px;white-space:nowrap;font-weight:bold;",
                        matched_bird$scientificName
                    ),
                    # Play button (bottom-left)
                    actionButton(
                        inputId = paste0("play_matched_sound_", matched_id),
                        label = tagList(icon("play"), "Play"),
                        class = "btn btn-sm btn-primary position-absolute",
                        style = "bottom:12px;left:12px;z-index:2;"
                    )
                ))
                
                session$sendCustomMessage("addOverlay", list(
                    id = paste0("sound_image_container_", matched_id),
                    html = overlay_html
                ))

                # Disable and grey-out the matched sound button so it can't be selected again
                session$sendCustomMessage('addClass', list(id = paste0('sound_button_', matched_id), class = 'disabled-item'))
                runjs(sprintf("$('#%s').prop('disabled', true);", paste0('sound_button_', matched_id)))
                
                # Set selected bird after a tiny delay to avoid stacking reflows with overlay injection
                later::later(function() {
                    rv_sound_matching$selected_bird <- matched_bird
                }, delay = 0.05)
                
                if (rv_sound_matching$matches == rv_sound_matching$total) {
                    session$sendCustomMessage("birdConfetti", list())
                    showNotification(
                        "🎉 Great! You've matched all the sounds!",
                        type = "message",
                        duration = 5
                    )
                }
            }
            rv_sound_matching$selected_sound <- NULL
            rv_sound_matching$selected_image <- NULL
        }
    }

    output$sound_matching_score <- renderText({
        req(rv_sound_matching$total)
        sprintf("Matches: %d/%d", rv_sound_matching$matches, rv_sound_matching$total)
    })

    output$sound_match_bird_stats <- renderUI({
        req(rv_sound_matching$selected_bird)
        
        html <- get_cached_info(rv_sound_matching$selected_bird$taxonID)
        if (is.na(html) || is.null(html) || !nzchar(html)) {
            # Fallback: embed the Artfakta page if scraping returned nothing
            url <- paste0("https://artfakta.se/taxa/", rv_sound_matching$selected_bird$taxonID, "/information")
            return(tagList(
                h5(rv_sound_matching$selected_bird$scientificName),
                div(class="text-muted", "Live info (embedded from Artfakta):"),
                tags$iframe(src = url, style = "width:100%;height:360px;border:1px solid #eee;border-radius:8px;")
            ))
        }
        # Lightly clean and format the HTML (remove nav/sidebars)
        html <- filter_artfakta_content(html)
        title <- extract_artfakta_title(html, fallback = rv_sound_matching$selected_bird$scientificName)
        tagList(
            h5(title),
            HTML(format_artfakta_info(html))
        )
    })

    observeEvent(input$sound_match_play_sound, {
        req(rv_sound_matching$selected_bird)
        urls <- get_sounds_go_to(rv_sound_matching$selected_bird$taxonID)
        if (length(urls)>0) {
            runjs(sprintf("
                var audio = document.getElementById('bird_sound');
                audio.src = '%s';
                audio.play();
            ", urls[1]))
        }
    })

    observeEvent(input$sound_match_stop_sound, {
        runjs("
            var audio = document.getElementById('bird_sound');
            if(audio){ audio.pause(); audio.currentTime = 0; }
        ")
    })

    # Bird Browser
    rv_browser <- reactiveValues(selected = NULL, search = "")

    observe({ rv_browser$search <- tolower(ifelse(is.null(input$bird_search), "", input$bird_search)) })

    output$bird_list <- renderUI({
        req(exists("bird_df"))
        birds <- bird_df %>% select(taxonID, scientificName) %>% distinct()
        if (nchar(rv_browser$search) > 0) {
            birds <- birds %>% filter(grepl(rv_browser$search, tolower(scientificName)))
        }
        div(class = "list-group",
            lapply(seq_len(nrow(birds)), function(i){
                id <- birds$taxonID[i]
                sid <- paste0("browse_bird_", safe_id(id))
                nm <- birds$scientificName[i]
                actionLink(
                    inputId = sid,
                    label = tags$span(style = if(!is.null(rv_browser$selected) && rv_browser$selected$taxonID==id) "font-weight:bold; color:#007bff;" else "", nm),
                    class = "list-group-item list-group-item-action"
                )
            })
        )
    })

    observe({
        req(exists("bird_df"))
        birds <- bird_df %>% select(taxonID, scientificName) %>% distinct()
        lapply(seq_len(nrow(birds)), function(i){
            id <- birds$taxonID[i]
            sid <- paste0("browse_bird_", safe_id(id))
            observeEvent(input[[sid]], {
                rv_browser$selected <- birds[i, , drop = FALSE]
            }, ignoreInit = TRUE)
        })
    })

    # Clean only unwanted elements for Browser (keep hero image, keep columns)
    clean_browser_content <- function(html_str) {
        if (is.null(html_str) || is.na(html_str) || !nzchar(html_str)) return(html_str)
        doc <- tryCatch(read_html(html_str), error = function(e) NULL)
        if (is.null(doc)) return(html_str)
        root <- xml2::xml_root(doc)
        # remove nav tabs
        suppressWarnings({
            nav_tabs <- rvest::html_elements(root, "nav[role='tablist']")
            if (length(nav_tabs) > 0) for (elem in nav_tabs) xml2::xml_remove(elem)
        })
        # remove add-to-my-species button
        suppressWarnings({
            add_btn <- rvest::html_elements(root, "app-add-to-my-species")
            if (length(add_btn) > 0) for (elem in add_btn) xml2::xml_remove(elem)
        })
        as.character(root)
    }

    output$browser_bird_info <- renderUI({
        req(rv_browser$selected)
        html <- get_cached_info(rv_browser$selected$taxonID)
        if (is.na(html) || is.null(html) || !nzchar(html)) {
            url <- paste0("https://artfakta.se/taxa/", rv_browser$selected$taxonID, "/information")
            return(tags$div(
                class = "mt-2",
                tags$div(class = "text-muted", "Live info (embedded from Artfakta):"),
                tags$iframe(src = url, style = "width:100%;height:calc(100vh - 300px);border:1px solid #eee;border-radius:8px;")
            ))
        }
        # Show full content in Browser tab, but remove nav tabs and add-to-my-species button; keep main image
        cleaned <- clean_browser_content(html)
        title <- extract_artfakta_title(cleaned, fallback = rv_browser$selected$scientificName)
        tagList(
            h5(title),
            HTML(format_artfakta_info(cleaned))
        )
    })

    # Render Artfakta main-content for browser selected bird
    output$browser_bird_info_html <- renderUI({
        req(rv_browser$selected)
        html <- get_cached_info(rv_browser$selected$taxonID)
        if (is.na(html) || is.null(html)) return(NULL)
        div(style = "max-height: 420px; overflow-y: auto; border-top: 1px solid #eee; margin-top: 10px; padding-top: 10px;",
            HTML(html)
        )
    })

    output$browser_bird_image <- renderUI({
        req(rv_browser$selected)
        tags$img(
            src = file.path("/birdpics", paste0(rv_browser$selected$taxonID, ".jpg")),
            style = "max-width:100%; height:auto; border-radius:8px;",
            alt = rv_browser$selected$scientificName
        )
    })

    observeEvent(input$browser_play_sound, {
        req(rv_browser$selected)
        urls <- get_sounds_go_to(rv_browser$selected$taxonID)
        if (length(urls)>0) {
            runjs(sprintf("
                var audio = document.getElementById('bird_sound');
                audio.src = '%s';
                audio.play();
            ", urls[1]))
        }
    })

    observeEvent(input$browser_stop_sound, {
        runjs("
            var audio = document.getElementById('bird_sound');
            if(audio){ audio.pause(); audio.currentTime = 0; }
        ")
    })

    output$browser_bird_timeline <- renderPlot({
        req(rv_browser$selected)
        bd <- bird_df %>% filter(taxonID == rv_browser$selected$taxonID)
        if (nrow(bd) == 0) return(NULL)
        if (!inherits(bd$eventDate, "Date")) suppressWarnings({ bd$eventDate <- as.Date(bd$eventDate) })
        ggplot(bd, aes(x = eventDate)) +
            geom_histogram(bins = 30, fill = "steelblue", color = "white") +
            theme_minimal() +
            labs(x = "Date", y = "Observations")
    })
}