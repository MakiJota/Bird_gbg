ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    useShinyjs(),
    
    tags$head(
        tags$script(src = "bird-confetti.js"),
        tags$script(
            type = "text/javascript",
            src = "https://cdnjs.buymeacoffee.com/1.0.0/widget.prod.min.js",
            `data-name` = "BMC-Widget",
            `data-cfasync` = "false",
            `data-id` = "RxCUuV5",
            `data-description` = "Support me on Buy me a coffee!",
            `data-message` = "Thanks for the coffee! :)",
            `data-color` = "#26B0A1",
            `data-position` = "Right",
            `data-x_margin` = "5",
            `data-y_margin` = "5"
        ),
        tags$script(HTML("
            Shiny.addCustomMessageHandler('addClass', function(message) {
                $('#' + message.id).addClass(message.class);
            });
            Shiny.addCustomMessageHandler('removeClass', function(message) {
                $('#' + message.id).removeClass(message.class);
            });
            Shiny.addCustomMessageHandler('addOverlay', function(message) {
                var card = $('#' + message.id);
                if (card.length) {
                    card.append(message.html);
                    // Bind Shiny inputs within just this new subtree to avoid global rebinds
                    Shiny.bindAll(card[0]);
                }
            });
            Shiny.addCustomMessageHandler('birdConfetti', function(message) {
                if (typeof createBirdConfetti === 'function') {
                    createBirdConfetti();
                }
            });
        ")),
        tags$style(HTML("
            .game-card {
                perspective: 1000px;
                margin: 10px;
                cursor: pointer;
                will-change: transform;
            }
            .card-inner {
                position: relative;
                width: 100%;
                height: auto;
                aspect-ratio: 1 / 1; /* make cards square */
                text-align: center;
                transition: transform 0.8s;
                transform-style: preserve-3d;
                cursor: pointer;
                border-radius: 10px;
                box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                backface-visibility: hidden;
                -webkit-backface-visibility: hidden;
                will-change: transform;
            }
            .flipped .card-inner {
                transform: rotateY(180deg);
            }
            .card-front, .card-back {
                position: absolute;
                width: 100%;
                height: 100%;
                backface-visibility: hidden;
                -webkit-backface-visibility: hidden;
                display: flex;
                align-items: center;
                justify-content: center;
                border-radius: 10px;
                padding: 10px;
            }
            .card-front {
                background-color: #2c3e50;
                color: white;
                font-size: 24px;
            }
            .card-back {
                background-color: white;
                transform: rotateY(180deg);
            }
            .card-back img {
                width: 100%;
                height: 100%;
                object-fit: cover; /* center and cover the square */
                object-position: center;
                border-radius: 8px;
            }
            /* Generic square image helper */
            .square-img {
                width: 100%;
                aspect-ratio: 1 / 1;
                object-fit: cover;
                object-position: center;
                border-radius: 8px;
            }
            
            /* Responsive card sizing based on difficulty */
            .difficulty-6 .game-card {
                margin: 10px;
            }
            .difficulty-8 .game-card {
                margin: 5px;
                max-width: 180px;
            }
            .difficulty-8 .card-inner {
                font-size: 0.85em;
            }
            .difficulty-12 .game-card {
                margin: 4px;
                max-width: 140px;
            }
            .difficulty-12 .card-inner {
                font-size: 0.75em;
            }
            .difficulty-16 .game-card {
                margin: 3px;
                max-width: 120px;
            }
            .difficulty-16 .card-inner {
                font-size: 0.7em;
            }
            
            .matched {
                /* dimming removed so overlays (name + play) show full color */
            }
            
            /* Selection highlighting for matching games */
            .list-group-item:active, .list-group-item.selected {
                border: 3px solid #007bff !important;
                background-color: #e7f3ff !important;
            }
            .btn:active, .btn.selected {
                border: 3px solid #28a745 !important;
                box-shadow: 0 0 0 0.2rem rgba(40, 167, 69, 0.5) !important;
            }
            
            .score-panel {
                background: white;
                padding: 20px;
                border-radius: 10px;
                box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                margin-bottom: 20px;
            }
            .bird-info {
                background: white;
                padding: 20px;
                border-radius: 10px;
                box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                margin-top: 20px;
            }
            .game-controls {
                margin: 20px 0;
            }
            .btn-play-sound {
                margin-top: 10px;
            }

            /* Reduce layout shifts in the game area to avoid flicker */
            .game-board {
                min-height: 650px;
            }

            /* Disabled state for matched list items (names/sounds) */
            .disabled-item {
                opacity: 0.5 !important;
                filter: grayscale(60%);
                pointer-events: none;
            }

            /* Contain heavy grid updates to avoid page-wide repaint on match */
            .grid-scroller {
                contain: content;
                will-change: transform;
                backface-visibility: hidden;
                -webkit-backface-visibility: hidden;
            }
            /* Remove button focus/active effects inside grids to avoid visual blink */
            .grid-scroller .btn:focus,
            .grid-scroller .btn:active {
                outline: none !important;
                box-shadow: none !important;
            }
            .grid-scroller .btn {
                transition: none !important;
            }
        "))
    ),
    
    # Main container with tabs
    tabsetPanel(
        # Memory Game Tab
        tabPanel("Memory Game",
            div(class = "container-fluid",
                div(class = "row",
                    # Left column - Game controls and stats
                    div(class = "col-md-3",
                        div(class = "score-panel",
                            h2("Bird Memory Game"),
                            div(class = "mb-3 text-muted", "This is a classic memory game! Flip cards to find matching pairs of birds. Test your visual memory while learning about bird species."),
                            div(class = "game-controls",
                                div(class = "btn-group mb-2",
                                    actionButton("start_game", "New Game", class = "btn-primary"),
                                    actionButton("stop_sound", "Pause Sound", 
                                               class = "btn btn-secondary", 
                                               icon = icon("pause"))
                                ),
                                br(), br(),
                                selectInput("difficulty", "Difficulty:",
                                          choices = c("Easy (6 pairs)" = 6,
                                                    "Medium (8 pairs)" = 8,
                                                    "Hard (12 pairs)" = 12,
                                                    "Expert (16 pairs)" = 16))
                            ),
                            hr(),
                            h4("Game Stats"),
                            textOutput("moves_count"),
                            textOutput("pairs_found"),
                            textOutput("time_elapsed")
                        ),
                        # Selected bird info panel
                        div(class = "bird-info",
                            h4("Selected Bird Info"),
                            textOutput("selected_bird_name"),
                            #uiOutput("bird_statistics"),
                            #div(class = "mt-3",
                            #    h5("Species information"),
                            #    uiOutput("selected_bird_info_html")
                            #)
                        )
                    ),
                    
                    # Right column - Game grid
                    div(class = "col-md-9",
                        div(class = "game-board",
                            uiOutput("memory_cards")
                        )
                    )
                )
            )
        ),
        
        # Name-Image Matching Tab
        tabPanel("Name Matching",
            div(class = "container-fluid",
                div(class = "row",
                    div(class = "col-md-4",
                        div(class = "score-panel",
                            h4("Name Matching Game"),
                            div(class = "mb-3 text-muted", "This is a nerdy name-image matching game! Match the scientific name to the correct bird image. Learn about bird taxonomy as you play."),
                            div(class = "btn-group mb-2",
                                actionButton("start_name_matching", "New Game", class = "btn-primary"),
                                actionButton("name_match_stop_sound", "Stop Sound", 
                                           class = "btn btn-secondary", 
                                           icon = icon("stop"))
                            ),
                            br(), br(),
                            textOutput("name_matching_score"),
                            hr(),
                            h5("Bird Names"),
                            div(style = "max-height: 300px; overflow-y: auto;",
                                uiOutput("name_matching_list")
                            )
                        ),
                        div(class = "bird-info mt-3",
                            h4("Selected Bird Information"),
                            div(style = "max-height: 400px; overflow-y: auto;",
                                uiOutput("name_match_bird_stats")
                            )
                        )
                    ),
                    div(class = "col-md-8",
                        h4("Bird Images"),
                        div(class = "grid-scroller", style = "max-height: 700px; overflow-y: auto; overflow-x: hidden;",
                            uiOutput("image_matching_grid")
                        )
                    )
                )
            )
        ),

        # Sound-Image Matching Tab
        tabPanel("Sound Matching",
            div(class = "container-fluid",
                div(class = "row",
                    div(class = "col-md-4",
                        div(class = "score-panel",
                            h4("Sound Matching Game"),
                            div(class = "mb-3 text-muted", "Match the bird sound to the correct image. Click play to hear the sound. Learn to identify birds by their calls!"),
                            div(class = "btn-group mb-2",
                                actionButton("start_sound_matching", "New Game", class = "btn-primary"),
                                actionButton("sound_match_stop_sound", "Stop Sound",
                                           class = "btn btn-secondary",
                                           icon = icon("stop"))
                            ),
                            br(), br(),
                            textOutput("sound_matching_score"),
                            hr(),
                            h5("Bird Sounds"),
                            div(style = "max-height: 300px; overflow-y: auto;",
                                uiOutput("sound_matching_buttons")
                            )
                        ),
                        div(class = "bird-info mt-3",
                            h4("Selected Bird Information"),
                            div(style = "max-height: 400px; overflow-y: auto;",
                                uiOutput("sound_match_bird_stats")
                            )
                        )
                    ),
                    div(class = "col-md-8",
                        h4("Bird Images"),
                        div(class = "grid-scroller", style = "max-height: 700px; overflow-y: auto; overflow-x: hidden;",
                            uiOutput("sound_matching_images")
                        )
                    )
                )
            )
        ),

        # Bird Browser Tab
        tabPanel("Bird Browser",
            div(class = "container-fluid",
                div(class = "row",
                    div(class = "col-md-4",
                        h4("Bird Species List"),
                        div(class = "mb-3 text-muted", 
                            "Browse bird species and read information from ", 
                            tags$a(href = "https://artfakta.se", target = "_blank", "Artfakta"), 
                            ". Search for a bird to see detailed species information."
                        ),
                        textInput("bird_search", "Search Birds", ""),
                        div(style = "height: calc(100vh - 300px); overflow-y: auto; border: 1px solid #dee2e6; border-radius: 8px; padding: 10px;",
                            uiOutput("bird_list")
                        )
                    ),
                    div(class = "col-md-8",
                        h4("Species Information"),
                        div(style = "height: calc(100vh - 250px); overflow-y: auto;",
                            uiOutput("browser_bird_info")
                        )
                    )
                )
            )
        )
    ),
    
    # Hidden audio element for bird sounds
    tags$audio(id = "bird_sound", type = "audio/mpeg")
)