server <- function(input, output, session) {
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
    
    # Render memory cards
    output$memory_cards <- renderUI({
        req(rv$cards)
        n_cards <- nrow(rv$cards)
        n_cols <- 4  # Number of cards per row
        
        # Calculate number of rows needed
        n_rows <- ceiling(n_cards / n_cols)
        
        # Create rows
        tagList(
            lapply(1:n_rows, function(row) {
                start_idx <- (row - 1) * n_cols + 1
                end_idx <- min(row * n_cols, n_cards)
                
                div(class = "row",
                    lapply(start_idx:end_idx, function(i) {
                        card <- rv$cards[i, ]
                        is_flipped <- card$card_id %in% rv$flipped
                        is_matched <- card$card_id %in% rv$matched
                        in_exploration <- rv$exploration_mode
                        
                        div(class = "col-3",
                            div(id = paste0("card-", card$card_id),
                                class = paste("game-card",
                                            if(is_flipped || is_matched || in_exploration) "flipped" else "",
                                            if(is_matched) "matched" else "",
                                            if(in_exploration) "exploration" else ""),
                                onclick = sprintf("Shiny.setInputValue('flip_card', %d, {priority: 'event'});", 
                                               card$card_id),
                                if(in_exploration) {
                                    onmouseover = sprintf(
                                        "Shiny.setInputValue('hover_card', %d, {priority: 'event'});",
                                        card$card_id
                                    )
                                },
                                div(class = "card-inner",
                                    div(class = "card-front",
                                        icon("feather")
                                    ),
                                    div(class = "card-back",
                                        img(src = paste0("/birdpics/", card$taxonID, ".jpg"))
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
        req(rv$game_active)
        card_id <- input$flip_card
        
        # Ignore if card is already matched or if two cards are already flipped
        if (!(card_id %in% rv$matched) && length(rv$flipped) < 2 && !(card_id %in% rv$flipped)) {
            rv$flipped <- c(rv$flipped, card_id)
            
            # Update selected bird info
            selected_card <- rv$cards[rv$cards$card_id == card_id, ]
            rv$selected_bird <- selected_card
            
            # Check for match when two cards are flipped
            if (length(rv$flipped) == 2) {
                rv$moves <- rv$moves + 1
                
                # Get the two flipped cards
                cards <- rv$cards[rv$cards$card_id %in% rv$flipped, ]
                
                # Check if they match
                if (cards$taxonID[1] == cards$taxonID[2]) {
                    # Match found
                    rv$matched <- c(rv$matched, rv$flipped)
                    rv$flipped <- c()
                    
                    # Check if game is complete
                    if (length(rv$matched) == nrow(rv$cards)) {
                        showModal(modalDialog(
                            title = "Congratulations!",
                            sprintf("You won in %d moves and %d seconds!\nAll cards will remain face up for exploration.", 
                                    rv$moves, round(as.numeric(rv$elapsed)))
                        ))
                        rv$game_active <- FALSE
                        rv$exploration_mode <- TRUE
                    }
                } else {
                    # No match - flip cards back after delay
                    later::later(function() {
                        rv$flipped <- c()
                    }, 1)
                }
            }
        }
    })
    
    # Handle card hover in exploration mode
    observeEvent(input$hover_card, {
        req(rv$exploration_mode, rv$cards)
        card_id <- input$hover_card
        selected_card <- rv$cards[rv$cards$card_id == card_id, ]
        rv$selected_bird <- selected_card
    })
    
    # Play bird sound using get_sounds_go_to
    observeEvent(input$play_sound, {
        req(rv$selected_bird, input$sound_enabled)
        
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
    
    # Game statistics outputs
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
    
    # Selected bird information
    output$selected_bird_name <- renderText({
        if (!is.null(rv$selected_bird)) {
            rv$selected_bird$scientificName
        } else {
            "No bird selected"
        }
    })
    
    output$selected_bird_image <- renderUI({
        req(rv$selected_bird)
        tags$img(
            src = paste0("/birdpics/", rv$selected_bird$taxonID, ".jpg"),
            style = "max-width:100%; height:auto;",
            alt = rv$selected_bird$scientificName
        )
    })
    
    # Diagnostics output
    output$diag_text <- renderPrint({
        list(
            cwd = tryCatch(getwd(), error = function(e) NA),
            project_root = if (exists("project_root")) project_root else NA,
            image_dir = if (exists("image_dir")) image_dir else NA,
            game_birds_available = if (exists("game_birds")) nrow(game_birds) else 0,
            sample_taxonIDs = if (exists("game_birds") && nrow(game_birds) > 0) 
                head(game_birds$taxonID) else character(0)
        )
    })
}