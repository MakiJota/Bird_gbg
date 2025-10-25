
library(dplyr)
library(hms)
library(stringr)

# Load data
find_occurrence_file <- function() {
        candidates <- c(
                "occurrence.txt",  # Look in current directory first (for deployment)
                file.path("00_data", "occurrence.txt"),
                file.path("..", "00_data", "occurrence.txt"),
                file.path("..", "..", "00_data", "occurrence.txt"),
                file.path(getwd(), "00_data", "occurrence.txt"),
                "./00_data/occurrence.txt"
        )
        for (p in unique(candidates)) {
                if (!is.na(p) && nzchar(p) && file.exists(p)) return(normalizePath(p))
        }
        return(NULL)
}

occ_file <- find_occurrence_file()
if (is.null(occ_file)) {
        warning("Occurrence file not found in expected locations; creating empty bird_df so app can continue.")
        bird_df <- data.frame(
                scientificName = character(),
                taxonID = character(),
                eventTime = character(),
                eventDate = as.Date(character()),
                siteName = character(),
                stringsAsFactors = FALSE
        )
} else {
        bird_df <- read.csv(occ_file, stringsAsFactors = FALSE)

        if (nrow(bird_df) > 0) {
                bird_df <- bird_df %>%
                        # Select relevant columns (if they exist)
                        select(-one_of(c("basisOfRecord", "BirdNETClass", "BirdNETConfidence",
                                                          "expertValidated", "isIsolated", "reclassified",
                                                          "detectionDistanceInMeters", "bcl",
                                                          "countryCode", "country")), everything()) %>%
                        mutate(
                                scientificName = str_remove_all(scientificName, " \\(.*?\\)"),
                                eventDate = as.Date(eventDate),
                                # Extract hour as integer (handles various eventTime formats and NA)
                                eventHour = as.integer(str_extract(eventTime, "^(\\d{1,2})")),
                                taxonID = str_extract(taxonID, "\\d{6}?"),

                                # Correct site names
                                siteName = case_when(
                                        siteName == "Domkyrkanplan Park" ~ "Domkyrkoplan",
                                        siteName == "Fridkullagatan 18" ~ "Fridkullagatan/Tapetseraregatan",
                                        siteName == "Lorensberg park" ~ "Lorensbergsparken",
                                        siteName == "Söderlingska Park" ~ "Gathenhielmska parken",
                                        TRUE ~ siteName
                                )
                        )

                # Only relocate eventHour if the column exists
                if ("eventHour" %in% names(bird_df) && "eventTime" %in% names(bird_df)) {
                        bird_df <- relocate(bird_df, eventHour, .after = eventTime)
                }
        }
}
