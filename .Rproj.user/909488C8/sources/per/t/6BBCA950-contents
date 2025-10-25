library(shiny)
library(bslib)
library(shinyjs)

ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    useShinyjs(),
    
    # Header with bird illustration inspired theme
    div(class = "container-fluid",
            style = "background: #001E51; color: #F5F1E8; padding: 25px 0; margin-bottom: 25px; box-shadow: 0 4px 6px rgba(0,0,0,0.2); border-radius: 15px; position: relative;",
        div(class = "container",
                # About button in top-right corner
                div(style = "position: absolute; top: 15px; right: 20px;",
                    actionButton("about_btn", 
                               label = tagList(icon("info-circle"), " About"),
                               class = "btn btn-sm",
                               style = "background: rgba(255,255,255,0.2); color: #F5F1E8; border: 1px solid rgba(255,255,255,0.3); font-weight: 500;")
                ),
            h1(style = "margin: 0; font-weight: 700; text-align: center; text-shadow: 2px 2px 4px rgba(0,0,0,0.3); letter-spacing: 0.5px;",
               tags$img(src = "bird.png", style = "height: 40px; margin-right: 10px; vertical-align: middle;"),
               " Fåglar i Göteborg ",
               tags$span(style = "font-size: 0.5em; font-weight: normal;", "🇸🇪")
            ),
            p(style = "text-align: center; margin: 5px 0 0 0; font-size: 0.9em; opacity: 0.9; font-weight: 300;",
              "Upptäck fåglarna i Göteborg • Discover the birds of Gothenburg")
        )
    ),
    
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
            body {
                background-color: #3EBD172;
                font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            }
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
                aspect-ratio: 1 / 1;
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
                background: linear-gradient(135deg, #1A2B4A 0%, #3A4D6A 100%);
                color: #F4C542;
                font-size: 32px;
                box-shadow: inset 0 0 20px rgba(125,212,192,0.2);
                border: 2px solid rgba(109,191,216,0.3);
            }
            .card-back {
                background-color: #F5F1E8;
                transform: rotateY(180deg);
                border: 2px solid rgba(26,43,74,0.1);
            }
            .card-back img {
                width: 100%;
                height: 100%;
                object-fit: cover;
                object-position: center;
                border-radius: 8px;
            }
            
            /* Bird illustration inspired colors for UI elements */
            .score-panel {
                background: linear-gradient(135deg, #F5F1E8 0%, #ffffff 100%);
                padding: 20px;
                border-radius: 15px;
                box-shadow: 0 4px 12px rgba(26,43,74,0.15);
                margin-bottom: 20px;
                border: 3px solid #4d90a8;
            }
            .bird-info {
                background: linear-gradient(135deg, #ffffff 0%, #F5F1E8 100%);
                padding: 20px;
                border-radius: 15px;
                box-shadow: 0 4px 12px rgba(26,43,74,0.15);
                margin-top: 20px;
                border: 3px solid #4d90a8;
            }
            
            /* Buttons with illustration colors */
            .btn-primary {
                background: linear-gradient(135deg, #1A2B4A 0%, #3A4D6A 100%) !important;
                border: none !important;
                color: #F5F1E8 !important;
                box-shadow: 0 2px 4px rgba(26,43,74,0.3) !important;
                transition: all 0.3s ease !important;
                font-weight: 500;
            }
            .btn-primary:hover {
                background: linear-gradient(135deg, #3A4D6A 0%, #6DBFD8 100%) !important;
                transform: translateY(-2px);
                box-shadow: 0 4px 8px rgba(109,191,216,0.4) !important;
            }
            
            .btn-secondary {
                background: #a8a54d !important;
                border: none !important;
                color: #1A2B4A !important;
                font-weight: 500;
            }
            .btn-secondary:hover {
                background: #8f8c3d !important;
            }
            
            /* Tab styling with illustration colors */
            .nav-tabs .nav-link.active {
                background: linear-gradient(135deg, #1A2B4A 0%, #3A4D6A 100%) !important;
                color: #F5F1E8 !important;
                border: none !important;
                font-weight: 600;
            }
            .nav-tabs .nav-link {
                color: #1A2B4A;
                transition: all 0.3s ease;
                font-weight: 500;
            }
            .nav-tabs .nav-link:hover {
                background-color: rgba(109,191,216,0.15);
                color: #3A4D6A;
            }
            
            /* Square image helper */
            .square-img {
                width: 100%;
                aspect-ratio: 1 / 1;
                object-fit: cover;
                object-position: center;
                border-radius: 8px;
            }
            
            /* Responsive card sizing */
            .difficulty-6 .game-card {
                margin: 10px;
            }
            .difficulty-8 .game-card {
                margin: 5px;
                max-width: 180px;
            }
            .difficulty-12 .game-card {
                margin: 4px;
                max-width: 140px;
            }
            .difficulty-16 .game-card {
                margin: 3px;
                max-width: 120px;
            }
            
            /* Selection highlighting */
            .list-group-item:active, .list-group-item.selected {
                border: 3px solid #1A2B4A !important;
                background-color: rgba(109,191,216,0.15) !important;
            }
            .btn:active, .btn.selected {
                border: 3px solid #F4C542 !important;
                box-shadow: 0 0 0 0.2rem rgba(244,197,66,0.4) !important;
            }
            
            .game-controls {
                margin: 20px 0;
            }
            .btn-play-sound {
                margin-top: 10px;
            }
            .game-board {
                min-height: 650px;
            }
            
            /* Disabled state */
            .disabled-item {
                opacity: 0.5 !important;
                filter: grayscale(60%);
                pointer-events: none;
            }
            
            /* Performance optimizations */
            .grid-scroller {
                contain: content;
                will-change: transform;
                backface-visibility: hidden;
                -webkit-backface-visibility: hidden;
            }
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
                            uiOutput("memory_card_modules")
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
                        div(class = "score-panel",
                            h4("Bird Species List"),
                            div(class = "mb-3 text-muted", 
                                "Browse bird species and read information from ", 
                                tags$a(href = "https://artfakta.se", target = "_blank", "Artfakta"), 
                                ". Search for a bird to see detailed species information."
                            ),
                            textInput("bird_search", "Search Birds", ""),
                            div(style = "height: calc(100vh - 420px); overflow-y: auto; border: 1px solid #dee2e6; border-radius: 8px; padding: 10px;",
                                uiOutput("bird_list")
                            )
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