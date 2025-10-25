# UI Variants for Bird Game

This folder contains five alternative UI layouts you can try without changing server logic.
All variants keep the same output/input IDs as the current app, so your existing `server.R` works.

## How to try a variant

Option A (quick):
1. Open a variant file and copy the function call at the bottom to create `ui`.
2. In your app launcher (app.R) or at the R console, source the variant and set `ui <- ui_variant_X()`.

```r
source("04_BirdGame/ui_variants/ui_variant_01_minimal_cards.R")
ui <- ui_variant_01()
shiny::runApp("04_BirdGame")
```

Option B (replace temporarily):
- Replace the contents of `04_BirdGame/ui.R` with the body of `ui_variant_X()`.

## Variants
- ui_variant_01_minimal_cards: Clean, airy layout. Big board left, compact info right. Light theme (flatly).
- ui_variant_02_dark_gallery: Dark mode with photo gallery vibe. Emphasis on images and sounds.
- ui_variant_03_dashboard_panels: Dashboard layout with cards for stats, info, and timelines.
- ui_variant_04_split_browser: Browser-first layout with focus on species list and details.
- ui_variant_05_fullscreen_game: Immersive, centered board with overlay info and minimal chrome.

> Note: You can switch Bootswatch themes by replacing `bs_theme(bootswatch = ...)`.
