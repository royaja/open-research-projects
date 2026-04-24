# UK airport delay patterns, 2023-2025
# Script 02: Aggregate
# Depends on: 01_load_and_clean.R (analysis_data must be in environment)

# load libraries

library(tidyverse)
library(scales)

# airport-year summary

airport_summary <-
  analysis_data |>
  group_by(reporting_period, reporting_airport) |>
  summarise(
    flights =
      sum(number_flights_matched, na.rm = TRUE),

    avg_delay =
      weighted.mean(
        average_delay_mins,
        w = number_flights_matched,
        na.rm = TRUE
      ),

    pct_late_over_15 =
      weighted.mean(
        pct_late_over_15,
        w = number_flights_matched,
        na.rm = TRUE
      ),

    pct_cancelled =
      weighted.mean(
        flights_cancelled_percent,
        w = number_flights_matched,
        na.rm = TRUE
      ),

    pct_on_time_15 =
      weighted.mean(
        pct_on_time_15,
        w = number_flights_matched,
        na.rm = TRUE
      ),

    .groups = "drop"
  )

# airport-airline summary

airport_airline_summary <-
  analysis_data |>
  group_by(reporting_period, reporting_airport, airline_name) |>
  summarise(
    flights =
      sum(number_flights_matched, na.rm = TRUE),

    avg_delay =
      weighted.mean(
        average_delay_mins,
        w = number_flights_matched,
        na.rm = TRUE
      ),

    pct_late_over_15 =
      weighted.mean(
        pct_late_over_15,
        w = number_flights_matched,
        na.rm = TRUE
      ),

    pct_cancelled =
      weighted.mean(
        flights_cancelled_percent,
        w = number_flights_matched,
        na.rm = TRUE
      ),

    .groups = "drop"
  )

# 2023-to-2025 delay change (airports with >= 5,000 flights in both years)

airport_change <-
  airport_summary |>
  filter(
    reporting_period %in% c(2023, 2025),
    flights >= 5000
  ) |>
  select(
    reporting_period,
    reporting_airport,
    avg_delay
  ) |>
  pivot_wider(
    names_from  = reporting_period,
    values_from = avg_delay,
    names_prefix = "delay_"
  ) |>
  mutate(
    change_2023_2025 = delay_2025 - delay_2023
  ) |>
  filter(!is.na(change_2023_2025))

# top 10 airports by average delay in 2025

top_airports_delay_2025 <-
  airport_summary |>
  filter(
    reporting_period == 2025,
    flights >= 5000
  ) |>
  arrange(desc(avg_delay)) |>
  slice_head(n = 10) |>
  mutate(
    reporting_airport = fct_reorder(reporting_airport, avg_delay)
  )

# top 10 airports by cancellation rate in 2025

top_airports_cancel_2025 <-
  airport_summary |>
  filter(
    reporting_period == 2025,
    flights >= 5000
  ) |>
  arrange(desc(pct_cancelled)) |>
  slice_head(n = 10) |>
  mutate(
    reporting_airport = fct_reorder(reporting_airport, pct_cancelled)
  )

# top 10 airport-airline pairs by average delay in 2025

top_airport_airline_2025 <-
  airport_airline_summary |>
  filter(
    reporting_period == 2025,
    flights >= 300
  ) |>
  mutate(
    airport_airline = paste(reporting_airport, airline_name, sep = " - ")
  ) |>
  arrange(desc(avg_delay)) |>
  slice_head(n = 10) |>
  mutate(
    airport_airline = fct_reorder(airport_airline, avg_delay)
  )

# top 10 airports by improvement in average delay from 2023 to 2025

top_airport_change <-
  airport_change |>
  arrange(change_2023_2025) |>
  slice_head(n = 10) |>
  mutate(
    reporting_airport = fct_reorder(reporting_airport, change_2023_2025),
    label_hjust = if_else(change_2023_2025 >= 0, -0.15, 1.15)
  )

# inspect tables

top_airports_delay_2025
top_airports_cancel_2025
top_airport_airline_2025
top_airport_change
