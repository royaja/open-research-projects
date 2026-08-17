
# Collect speeches from the Norwegian Parliament

# This script is the first part of a project examining how Norwegian
# politicians talk about access to healthcare. It collects plenary records
# from the Norwegian Parliament's open data services, extracts main speeches
# and replies, adds information about the meetings, and saves the raw data
# for the analysis and visualization script.

# The data cover speeches published between July 28, 2025, and July 28, 2026.

# Chatgpt was used to help restructure part of the code. 

# Code developed by R. A. Jacobsen

# Load packages
library(tidyverse)
library(stortingscrape)
library(rvest)

# Define the period
date_from <- as.Date("2025-07-28")
date_to <- as.Date("2026-07-28")

sessions <- c(
  "2024-2025",
  "2025-2026"
)

# Create the folder for the raw data
dir.create(
  "data/raw",
  recursive = TRUE,
  showWarnings = FALSE
)

# Collect parliamentary records
referater_plenum <-
  map_dfr(
    sessions,
    function(session) {
      get_session_publications(
        sessionid = session,
        type = "referat",
        good_manners = 0.7
      )
    }
  ) |>
  mutate(
    publication_date = as.Date(publication_date)
  ) |>
  filter(
    publication_date >= date_from,
    publication_date <= date_to,
    str_starts(
      publication_id,
      "refs-"
    )
  ) |>
  distinct(
    publication_id,
    .keep_all = TRUE
  ) |>
  arrange(
    publication_date
  )


# Create an empty table for records that cannot be processed
empty_speeches <-
  tibble(
    publication_id = character(),
    speech_number = integer(),
    speech_type = character(),
    speaker_raw = character(),
    text = character()
  )


# Download one parliamentary record
download_record <- function(publication_id) {
  
  tryCatch(
    get_publication(
      publicationid = publication_id,
      good_manners = 0.7
    ),
    error = function(error_message) {
      warning(
        paste(
          "Could not download",
          publication_id,
          "-",
          conditionMessage(error_message)
        )
      )
      
      NULL
    }
  )
}


# Extract speeches from one parliamentary record
extract_speeches <- function(document, publication_id) {
  
  if (is.null(document)) {
    return(empty_speeches)
  }
  
  speech_nodes <-
    document |>
    html_elements(
      "hovedinnlegg, replikk"
    )
  
  if (length(speech_nodes) == 0) {
    return(empty_speeches)
  }
  
  map_dfr(
    seq_along(speech_nodes),
    function(speech_number) {
      
      speech_node <- speech_nodes[[speech_number]]
      
      speaker_node <-
        speech_node |>
        html_element(
          "navn"
        )
      
      speaker_raw <-
        if (inherits(speaker_node, "xml_missing")) {
          NA_character_
        } else {
          speaker_node |>
            html_text2() |>
            str_squish()
        }
      
      speech_text <-
        speech_node |>
        html_elements(
          "a"
        ) |>
        html_text2() |>
        str_squish() |>
        paste(
          collapse = " "
        ) |>
        str_squish()
      
      tibble(
        publication_id = publication_id,
        speech_number = speech_number,
        speech_type = xml2::xml_name(speech_node),
        speaker_raw = speaker_raw,
        text = speech_text
      )
    }
  ) |>
    filter(
      !is.na(text),
      text != ""
    )
}


# Download and extract all speeches
all_speeches <-
  map_dfr(
    referater_plenum$publication_id,
    function(publication_id) {
      
      document <-
        download_record(
          publication_id
        )
      
      extract_speeches(
        document = document,
        publication_id = publication_id
      )
    }
  )


# Add dates and information about the meetings
record_metadata <-
  referater_plenum |>
  select(
    any_of(
      c(
        "publication_id",
        "publication_date",
        "publication_title",
        "session_id"
      )
    )
  )

all_speeches <-
  all_speeches |>
  left_join(
    record_metadata,
    by = "publication_id"
  ) |>
  relocate(
    any_of(
      c(
        "publication_id",
        "publication_date",
        "publication_title",
        "session_id",
        "speech_number",
        "speech_type",
        "speaker_raw",
        "text"
      )
    )
  ) |>
  arrange(
    publication_date,
    publication_id,
    speech_number
  )


# Save the data
saveRDS(
  all_speeches,
  "data/raw/all_speeches_2025-07-28_2026-07-28.rds"
)

write_csv(
  all_speeches,
  "data/raw/all_speeches_2025-07-28_2026-07-28.csv"
)

write_csv(
  referater_plenum,
  "data/raw/parliamentary_records_2025-07-28_2026-07-28.csv"
)

