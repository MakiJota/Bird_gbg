ui_variant_04 <- function() {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "yeti"),
    useShinyjs(),
    tags$head(tags$style(HTML(".sidebar{position:sticky; top:1rem} .square-img{width:100%;aspect-ratio:1/1;object-fit:cover;border-radius:8px} .game-board{min-height:620px}"))),
    fluidRow(
      column(3, div(class="sidebar",
        h3("Bird Browser"), textInput("bird_search","Search Gothenburg birds",""), uiOutput("bird_list"), hr(),
        h4("Quick Games"), actionButton("start_game","Memory Game"), actionButton("start_name_matching","Name Match"), actionButton("start_sound_matching","Sound Match")
      )),
      column(9,
        tabsetPanel(
          tabPanel("Details",
            h3(textOutput("selected_bird_name")), uiOutput("browser_bird_info"),
            uiOutput("browser_bird_image"),
            div(class="btn-group mt-3",
              actionButton("browser_play_sound","Play",icon=icon("play")),
              actionButton("browser_stop_sound","Stop",icon=icon("stop"))
            ),
            plotOutput("browser_bird_timeline"),
            div(class="mt-3", h5("Species information"), uiOutput("browser_bird_info_html"))
          ),
          tabPanel("Memory", div(class="game-board", uiOutput("memory_cards"))),
          tabPanel("Name Match", fluidRow(column(6, uiOutput("name_matching_list")), column(6, uiOutput("image_matching_grid")))),
          tabPanel("Sound Match", fluidRow(column(6, uiOutput("sound_matching_buttons")), column(6, uiOutput("sound_matching_images"))))
        )
      )
    )
  )
}
