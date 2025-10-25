ui_variant_02 <- function() {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "darkly"),
    useShinyjs(),
    tags$head(tags$style(HTML("body{background:#121417}.panel{background:#1c1f24;border-radius:10px;padding:16px}.square-img{width:100%;aspect-ratio:1/1;object-fit:cover;border-radius:10px;box-shadow:0 6px 14px rgba(0,0,0,.35)} .game-board{min-height:680px}"))),
    navbarPage("Birds • Gothenburg", id = "main_nav",
      tabPanel("Memory",
        fluidRow(
          column(9, div(class="panel", uiOutput("memory_cards"))),
          column(3,
            div(class="panel",
              h4("Stats"), textOutput("moves_count"), textOutput("pairs_found"), textOutput("time_elapsed"), hr(),
              h5(textOutput("selected_bird_name")), uiOutput("bird_statistics"),
              div(class="btn-group mt-2 w-100",
                actionButton("play_sound","Play",icon=icon("play"),class="btn-success w-50"),
                actionButton("stop_sound","Stop",icon=icon("stop"),class="btn-outline-light w-50")
              ),
              div(class="mt-3", h5("Info"), uiOutput("selected_bird_info_html"))
            )
          )
        )
      ),
      tabPanel("Name Match",
        fluidRow(
          column(3, div(class="panel", h4("Names"), uiOutput("name_matching_list"))),
          column(6, div(class="panel", h4("Images"), uiOutput("image_matching_grid"))),
          column(3, div(class="panel", h4("Selected"), uiOutput("name_match_bird_stats"),
            div(class="btn-group mt-2 w-100",
              actionButton("name_match_play_sound","Play",icon=icon("play"),class="btn-success w-50"),
              actionButton("name_match_stop_sound","Stop",icon=icon("stop"),class="btn-outline-light w-50")
            ),
            div(class="text-muted mt-2", textOutput("name_matching_score"))
          ))
        )
      ),
      tabPanel("Sound Match",
        fluidRow(
          column(3, div(class="panel", h4("Sounds"), uiOutput("sound_matching_buttons"))),
          column(6, div(class="panel", h4("Images"), uiOutput("sound_matching_images"))),
          column(3, div(class="panel", h4("Selected"), uiOutput("sound_match_bird_stats"),
            div(class="btn-group mt-2 w-100",
              actionButton("sound_match_play_sound","Play",icon=icon("play"),class="btn-success w-50"),
              actionButton("sound_match_stop_sound","Stop",icon=icon("stop"),class="btn-outline-light w-50")
            ),
            div(class="text-muted mt-2", textOutput("sound_matching_score"))
          ))
        )
      ),
      tabPanel("Browser",
        fluidRow(
          column(3, div(class="panel", textInput("bird_search","Search",""), uiOutput("bird_list"))),
          column(9, div(class="panel", uiOutput("browser_bird_info"), uiOutput("browser_bird_image"),
            div(class="btn-group mt-3",
              actionButton("browser_play_sound","Play",icon=icon("play"),class="btn-success"),
              actionButton("browser_stop_sound","Stop",icon=icon("stop"),class="btn-outline-light")
            ),
            plotOutput("browser_bird_timeline"),
            div(class="mt-3", h5("Info"), uiOutput("browser_bird_info_html"))
          ))
        )
      )
    )
  )
}
