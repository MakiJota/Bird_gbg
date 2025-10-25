ui_variant_03 <- function() {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    useShinyjs(),
    tags$head(tags$style(HTML(".kpi{background:#fff;border-radius:10px;padding:16px;box-shadow:0 2px 6px rgba(0,0,0,.08)} .square-img{width:100%;aspect-ratio:1/1;object-fit:cover;border-radius:10px} .game-board{min-height:640px}"))),
    titlePanel("Gothenburg Birds – Learn by Sound and Sight"),
    tabsetPanel(
      tabPanel("Memory",
        fluidRow(
          column(8, div(class="kpi", uiOutput("memory_cards"))),
          column(4,
            fluidRow(
              column(12, div(class="kpi", h4("Game"), actionButton("start_game","New Game",class="btn-primary"),
                               selectInput("difficulty","Difficulty",c("Easy (6)"=6,"Medium (8)"=8,"Hard (12)"=12)),
                               checkboxInput("sound_enabled","Sounds",TRUE))),
              column(4, div(class="kpi", textOutput("moves_count"))),
              column(4, div(class="kpi", textOutput("pairs_found"))),
              column(4, div(class="kpi", textOutput("time_elapsed"))),
              column(12, div(class="kpi", h4(textOutput("selected_bird_name")), uiOutput("bird_statistics"),
                             div(class="btn-group mt-2",
                               actionButton("play_sound","Play",icon=icon("play")),
                               actionButton("stop_sound","Stop",icon=icon("stop"))
                             ),
                             div(class="mt-3", h5("Species information"), uiOutput("selected_bird_info_html"))
              ))
          )
        )
      ),
      tabPanel("Name Match",
        fluidRow(
          column(4, div(class="kpi", h4("Names"), uiOutput("name_matching_list"))),
          column(4, div(class="kpi", h4("Images"), uiOutput("image_matching_grid"))),
          column(4, div(class="kpi", h4("Selected"), uiOutput("name_match_bird_stats"),
                        div(class="btn-group mt-2",
                          actionButton("name_match_play_sound","Play",icon=icon("play")),
                          actionButton("name_match_stop_sound","Stop",icon=icon("stop"))
                        ),
                        div(class="text-muted mt-2", textOutput("name_matching_score"))
          ))
        )
      ),
      tabPanel("Sound Match",
        fluidRow(
          column(4, div(class="kpi", h4("Sounds"), uiOutput("sound_matching_buttons"))),
          column(4, div(class="kpi", h4("Images"), uiOutput("sound_matching_images"))),
          column(4, div(class="kpi", h4("Selected"), uiOutput("sound_match_bird_stats"),
                        div(class="btn-group mt-2",
                          actionButton("sound_match_play_sound","Play",icon=icon("play")),
                          actionButton("sound_match_stop_sound","Stop",icon=icon("stop"))
                        ),
                        div(class="text-muted mt-2", textOutput("sound_matching_score"))
          ))
        )
      ),
      tabPanel("Browser",
        fluidRow(
          column(4, div(class="kpi", textInput("bird_search","Search",""), uiOutput("bird_list"))),
          column(8, div(class="kpi", uiOutput("browser_bird_info"), uiOutput("browser_bird_image"),
                        div(class="btn-group mt-3",
                          actionButton("browser_play_sound","Play",icon=icon("play")),
                          actionButton("browser_stop_sound","Stop",icon=icon("stop"))
                        ),
                        plotOutput("browser_bird_timeline"),
                        div(class="mt-3", h5("Species information"), uiOutput("browser_bird_info_html"))
          ))
        )
      )
    )
  )
}
