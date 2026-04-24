# UK airport delay patterns, 2023-2025
# Script 01: Load and clean
# Source: UK Civil Aviation Authority, Annual Punctuality Statistics

# load libraries

library(tidyverse)
library(janitor)
library(lubridate)

# set path

data_path <-
  "C:/Users/roja006/OneDrive - Kristiania/[00] Inbox/UK Flight Data"

# list files

files <-
  c(
    "2023_Annual_Punctuality_Statistics_Full_Analysis.csv",
    "2024_Annual_Punctuality_Statistics_Full_Analysis.csv",
    "2025_Annual_Punctuality_Statistics_Full_Analysis.csv"
  )

# load data

uk_flights_raw <-
  map_dfr(
    files,
    \(file_name) {
      read_csv(
        file.path(data_path, file_name),
        show_col_types = FALSE
      ) |>
        clean_names() |>
        mutate(source_file = file_name)
    }
  )

# inspect data

names(uk_flights_raw)
glimpse(uk_flights_raw)

# wrangle data

uk_flights <-
  uk_flights_raw |>
  mutate(
    run_date = dmy_hm(run_date),
    reporting_period = as.integer(reporting_period),

    reporting_airport = str_squish(str_to_title(reporting_airport)),
    origin_destination_country = str_squish(str_to_title(origin_destination_country)),
    origin_destination = str_squish(str_to_title(origin_destination)),
    airline_name = str_squish(str_to_title(airline_name)),

    scheduled_charter = case_when(
      scheduled_charter == "S" ~ "Scheduled",
      scheduled_charter == "C" ~ "Charter",
      TRUE ~ scheduled_charter
    )
  ) |>
  mutate(
    across(
      c(
        number_flights_matched,
        actual_flights_unmatched,
        number_flights_cancelled,
        flights_more_than_15_minutes_early_percent,
        flights_15_minutes_early_to_1_minute_early_percent,
        flights_0_to_15_minutes_late_percent,
        flights_between_16_and_30_minutes_late_percent,
        flights_between_31_and_60_minutes_late_percent,
        flights_between_61_and_120_minutes_late_percent,
        flights_between_121_and_180_minutes_late_percent,
        flights_between_181_and_360_minutes_late_percent,
        flights_more_than_360_minutes_late_percent,
        flights_unmatched_percent,
        flights_cancelled_percent,
        average_delay_mins
      ),
      as.numeric
    )
  ) |>
  mutate(
    pct_on_time_15 =
      flights_15_minutes_early_to_1_minute_early_percent +
      flights_0_to_15_minutes_late_percent,

    pct_late_over_15 =
      flights_between_16_and_30_minutes_late_percent +
      flights_between_31_and_60_minutes_late_percent +
      flights_between_61_and_120_minutes_late_percent +
      flights_between_121_and_180_minutes_late_percent +
      flights_between_181_and_360_minutes_late_percent +
      flights_more_than_360_minutes_late_percent,

    pct_total =
      flights_more_than_15_minutes_early_percent +
      flights_15_minutes_early_to_1_minute_early_percent +
      flights_0_to_15_minutes_late_percent +
      flights_between_16_and_30_minutes_late_percent +
      flights_between_31_and_60_minutes_late_percent +
      flights_between_61_and_120_minutes_late_percent +
      flights_between_121_and_180_minutes_late_percent +
      flights_between_181_and_360_minutes_late_percent +
      flights_more_than_360_minutes_late_percent +
      flights_unmatched_percent +
      flights_cancelled_percent
  )

# check percentage totals sum close to 100

pct_check <-
  uk_flights |>
  summarise(
    min_pct_total  = min(pct_total, na.rm = TRUE),
    mean_pct_total = mean(pct_total, na.rm = TRUE),
    max_pct_total  = max(pct_total, na.rm = TRUE)
  )

pct_check

# check for duplicate route-airport-airline-year combinations

duplicate_check <-
  uk_flights |>
  count(
    reporting_period,
    reporting_airport,
    origin_destination,
    airline_name,
    scheduled_charter
  ) |>
  filter(n > 1)

duplicate_check

# analysis sample: scheduled flights with at least 20 matched flights

analysis_data <-
  uk_flights |>
  filter(
    scheduled_charter == "Scheduled",
    number_flights_matched >= 20
  )
