ui_variant_01 <- function() {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    useShinyjs(),
    tags$head(tags$style(HTML(".square-img{width:100%;aspect-ratio:1/1;object-fit:cover;border-radius:8px;} .game-board{min-height:650px}"))),
    navbarPage("Birds of Gothenburg", id = "main_nav",
      tabPanel("Memory",
        fluidRow(
          column(8,
            div(class = "game-board", uiOutput("memory_cards"))
          ),
          column(4,
            div(class = "card p-3 mb-3",
              h4("Game"),
              actionButton("start_game", "New Game", class = "btn-primary"),
              selectInput("difficulty", "Difficulty", c("Easy (6)"=6,"Medium (8)"=8,"Hard (12)"=12)),
              checkboxInput("sound_enabled","Sounds", TRUE),
              hr(), textOutput("moves_count"), textOutput("pairs_found"), textOutput("time_elapsed")
            ),
            div(class = "card p-3",
              h4(textOutput("selected_bird_name")),
              uiOutput("bird_statistics"),
              div(class="btn-group mt-2",
                actionButton("play_sound","Play",icon=icon("play")),
                actionButton("stop_sound","Stop",icon=icon("stop"))
              ),
              div(class="mt-3", h5("Species information"), uiOutput("selected_bird_info_html"))
            )
          )
        )
      ),
      tabPanel("Name Match",
        fluidRow(
          column(4, h4("Names"), uiOutput("name_matching_list")),
          column(4, h4("Images"), uiOutput("image_matching_grid")),
          column(4,
            h4("Selected"), uiOutput("name_match_bird_stats"),
            div(class="btn-group mt-2",
                actionButton("name_match_play_sound","Play",icon=icon("play")),
                actionButton("name_match_stop_sound","Stop",icon=icon("stop"))
            ),
            div(class="text-muted mt-2", textOutput("name_matching_score"))
          )
        )
      ),
      tabPanel("Sound Match",
        fluidRow(
          column(4, h4("Sounds"), uiOutput("sound_matching_buttons")),
          column(4, h4("Images"), uiOutput("sound_matching_images")),
          column(4, h4("Selected"), uiOutput("sound_match_bird_stats"),
            div(class="btn-group mt-2",
                actionButton("sound_match_play_sound","Play",icon=icon("play")),
                actionButton("sound_match_stop_sound","Stop",icon=icon("stop"))
            ),
            div(class="text-muted mt-2", textOutput("sound_matching_score"))
          )
        )
      ),
      tabPanel("Browser",
        fluidRow(
          column(4, textInput("bird_search","Search",""), uiOutput("bird_list")),
          column(8, uiOutput("browser_bird_info"), uiOutput("browser_bird_image"),
            div(class="btn-group mt-3",
              actionButton("browser_play_sound","Play",icon=icon("play")),
              actionButton("browser_stop_sound","Stop",icon=icon("stop"))
            ),
            plotOutput("browser_bird_timeline"),
            div(class="mt-3", h5("Species information"), uiOutput("browser_bird_info_html"))
          )
        )
      )
    )
  )
}
