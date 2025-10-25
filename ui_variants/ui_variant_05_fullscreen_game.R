ui_variant_05 <- function() {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "cosmo"),
    useShinyjs(),
    tags$head(tags$style(HTML(".full-center{min-height:80vh;display:flex;align-items:center;justify-content:center} .panel{background:#fff;border-radius:12px;padding:16px;box-shadow:0 8px 20px rgba(0,0,0,.08)} .square-img{width:100%;aspect-ratio:1/1;object-fit:cover;border-radius:10px} .game-board{width:100%;max-width:1000px}"))),
    div(class="full-center",
      div(class="container",
        div(class="row g-4",
          div(class="col-lg-8",
            div(class="panel",
              h3("Memory Game"),
              div(class="game-board", uiOutput("memory_cards")),
              div(class="mt-2 d-flex gap-2",
                actionButton("start_game","New Game", class="btn-primary"),
                selectInput("difficulty","Difficulty",c("Easy (6)"=6,"Medium (8)"=8,"Hard (12)"=12)),
                checkboxInput("sound_enabled","Sounds", TRUE)
              )
            )
          ),
          div(class="col-lg-4",
            div(class="panel",
              h4(textOutput("selected_bird_name")),
              uiOutput("bird_statistics"),
              div(class="btn-group mt-2",
                actionButton("play_sound","Play",icon=icon("play")),
                actionButton("stop_sound","Stop",icon=icon("stop"))
              ),
              div(class="mt-3", h5("Species information"), uiOutput("selected_bird_info_html")),
              hr(), textOutput("moves_count"), textOutput("pairs_found"), textOutput("time_elapsed")
            )
          ),
          div(class="col-12",
            div(class="panel",
              h4("Explore"),
              tabsetPanel(
                tabPanel("Name Match", fluidRow(column(6, uiOutput("name_matching_list")), column(6, uiOutput("image_matching_grid")))),
                tabPanel("Sound Match", fluidRow(column(6, uiOutput("sound_matching_buttons")), column(6, uiOutput("sound_matching_images")))),
                tabPanel("Browser", fluidRow(column(4, textInput("bird_search","Search",""), uiOutput("bird_list")), column(8, uiOutput("browser_bird_info"), uiOutput("browser_bird_image"), plotOutput("browser_bird_timeline"), uiOutput("browser_bird_info_html"))))
              )
            )
          )
        )
      )
    )
  )
}
