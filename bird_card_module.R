# Bird Card Module for Memory Game

bird_card_UI <- function(id) {
  ns <- NS(id)
  # Use uiOutput so we can render an actionButton with an <img> (served via /birdpics)
  uiOutput(ns("card"))
}

bird_card <- function(input, output, session, card_img, card_name = NULL, taxon_id = NULL,
                      reset = reactiveValues(x = NULL), block = reactiveValues(x = NULL), gate = reactiveValues(busy = FALSE)) {
  # Derive taxon_id/name if not provided
  if (is.null(taxon_id)) {
    taxon_id <- sub("\\.jpg$", "", basename(card_img))
  }
  if (is.null(card_name)) {
    card_name <- taxon_id
  }

  card_status <- reactiveValues(show = FALSE, img = card_img, found = FALSE)

  # Toggle card on button click (front/back)
  observeEvent(input$card_btn, {
    # Ignore clicks if this card is already found or if a pair is being processed
    if (!card_status$found && !isTRUE(gate$busy)) {
      card_status$show <- !card_status$show
    }
  })

  observeEvent(block$x, {
    if (card_img %in% block$x) {
      card_status$found <- TRUE
      # Keep matched cards revealed
      card_status$show <- TRUE
    }
  })

  observeEvent(reset$x, {
    if (card_img %in% reset$x & !card_status$found) {
      card_status$show <- FALSE
    }
  })

  output$card <- renderUI({
    ns <- session$ns
    # Wrapper that controls flip state via CSS classes
    wrapper_class <- paste("game-card", if (isTRUE(card_status$show)) "flipped" else "")
    div(class = wrapper_class, style = "width:200px;",
        # Single button for clicks; content holds both faces for flip animation
        actionButton(
          inputId = ns("card_btn"),
          class = "btn p-0",
          style = "border:none;background:none;padding:0;width:100%;",
          label = div(class = "card-inner",
                      # Front face
                      div(class = "card-front",
                          icon("feather")
                      ),
                      # Back face with image and, if found, overlays
                      div(class = "card-back",
                          div(style = "position:relative;width:100%;height:100%;",
                              tags$img(src = paste0("birdpics/", card_img), class = "square-img"),
                              if (card_status$found) tagList(
                                # Name overlay (top-left)
                                div(style = "position:absolute;top:8px;left:8px;color:#fff;font-size:0.85em;background:rgba(0,123,255,0.95);padding:4px 6px;border-radius:4px;white-space:nowrap;font-weight:bold;z-index:2;",
                                    card_name
                                ),
                                # Play button (bottom-left)
                                actionButton(
                                  inputId = ns("play_btn"),
                                  label = tagList(icon("play"), "Play"),
                                  class = "btn btn-sm btn-primary",
                                  style = "position:absolute;bottom:8px;left:8px;z-index:2;"
                                )
                              )
                          )
                      )
          )
        )
    )
  })

  # Local sound play for matched cards
  observeEvent(input$play_btn, {
    # Use get_sound_url to prioritize local files over web scraping
    sound_url <- get_sound_url(taxon_id)
    if (!is.null(sound_url)) {
      shinyjs::runjs(sprintf(
        "var audio = document.getElementById('bird_sound');audio.src = '%s';audio.play();",
        sound_url
      ))
    } else {
      showNotification("Bird sound not available", type = "message", duration = 3)
    }
  })

  return(card_status)
}
