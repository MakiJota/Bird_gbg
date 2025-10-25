# 🐦 Fåglar i Göteborg - Bird Game

An interactive Shiny application for learning about bird species in Gothenburg, Sweden. Play memory games, match bird names and sounds, and explore detailed species information.

## 🎥 Demo

See the app in action:

![App Demo](birdy.mp4)

## 🌐 Live Application

Try the live app here: **[https://urbant.shinyapps.io/BirdyGame/](https://urbant.shinyapps.io/BirdyGame/)**

## 🎮 Game Modes

### Memory Game
Classic memory card game with bird images. Flip cards to find matching pairs while learning about local bird species.

### Name Matching
Match scientific names to bird images. A nerdy challenge to learn bird taxonomy!

### Sound Matching
Identify birds by their calls. Listen to bird sounds and match them to the correct images.

### Bird Browser
Browse and explore detailed information about bird species from [Artfakta.se](https://artfakta.se).

## 🚀 Deployment

### Local Development

1. **Install required R packages:**
```r
install.packages(c("shiny", "shinyjs", "bslib", "dplyr", "ggplot2", "rvest", "chromote", "purrr"))
```

2. **Run the app:**
```r
shiny::runApp()
```

### Deploy to shinyapps.io

1. **Prepare sound files** (if not already compressed):
```r
Rscript compress_sounds.R
```

2. **Deploy:**
```r
library(rsconnect)
rsconnect::deployApp(appDir = ".", appName = "birds-in-gothenburg")
```

The app automatically uses:
- Local MP3 files from `www/cuack/` when available
- Local bird images from `www/bird_pics/`
- Chromote for web scraping only in local development (not needed on deployment)

## 📁 Project Structure

```
04_BirdGame/
├── server.R                    # Server logic
├── ui.R                        # User interface
├── global.R                    # Global variables and setup
├── aux_functions.R             # Helper functions (sound/image fetching)
├── bird_card_module.R          # Memory card module
├── download_sounds.R           # Script to download bird sounds
├── compress_sounds.R           # Script to compress MP3 files
├── www/
│   ├── cuack/                  # Compressed bird sound files (MP3)
│   ├── bird_pics/              # Bird images (JPG)
│   ├── bird.png                # App icon
│   ├── JonathanCohen_386x386.jpg
│   └── bird-confetti.js        # Confetti animation
└── game_birds.csv              # Bird species data
```

## 🔊 Sound Management

### Download Sounds
Download bird sounds from Artfakta.se:
```r
source("download_sounds.R")
```

### Compress Sounds
Reduce file sizes for faster deployment (70-85% reduction):
```r
Rscript compress_sounds.R
```

Original files are automatically backed up to `www/cuack_backup/`.

**Compression settings:**
- Format: MP3
- Bitrate: 64 kbps
- Channels: Mono
- Sample rate: 22050 Hz

## 🛠️ Technical Details

### Sound System
The app uses a dual fallback system:
1. **Primary:** Local MP3 files in `www/cuack/{taxonID}.mp3`
2. **Fallback:** Web scraping via chromote (local development only)

### Data Source
- Bird occurrence data from Gothenburg
- Species information from [Artfakta.se](https://artfakta.se)
- Dataset DOI: [10.1038/s41597-025-05481-z](https://doi.org/10.1038/s41597-025-05481-z)

### Dependencies
- **shiny** - Web application framework
- **shinyjs** - JavaScript integration
- **bslib** - Bootstrap themes
- **chromote** - Web scraping (local only)
- **rvest** - HTML parsing
- **av** - Audio compression

## 📊 Game Data

Bird species data is stored in `game_birds.csv` with the following fields:
- `taxonID` - Unique species identifier
- `scientificName` - Scientific name
- `vernacularName` - Common name (Swedish)
- Image and sound URLs

## 🎨 Design

Color palette inspired by bird illustrations:
- Deep blue: `#001E51`, `#1A2B4A`
- Light teal: `#6DBFD8`
- Cream: `#F5F1E8`
- Accent gold: `#F4C542`

## 👤 Author

**Jonathan Cohen**

Data analytics and digitization specialist working with urban data insights.

- 🌐 [Website](https://jonathan-cohen-urbandataresearch.netlify.app/)
- 💼 [LinkedIn](https://www.linkedin.com/in/jonathan-cohen-data/)
- 📧 [Email](mailto:cohen.jona@gmail.com)

## 📝 License

This project is part of urban bird research in Gothenburg. For more information about the dataset, visit the publication link above.

## 🙏 Acknowledgments

- Bird data from Artdatabanken ([Artfakta.se](https://artfakta.se))
- Bootstrap themes via bslib
- Shiny framework by RStudio

---

**Enjoy learning about Gothenburg's birds! 🦜**
