# UK airport delay patterns, 2023-2025
# Script 03: Analyse and plot
# Depends on: 01_load_and_clean.R and 02_aggregate.R

# load libraries

library(tidyverse)
library(scales)
library(broom)
library(ggrepel)

# define colours

col_delay   <- "#6B8A9B"
col_cancel  <- "#A9B7A3"
col_accent  <- "#C78F6B"
col_dark    <- "#2F4858"
col_improve <- "#5E8C6A"

# shared theme

theme_delays <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#E6E6E6", linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      axis.title  = element_text(color = "#2F2F2F", size = 10),
      axis.text   = element_text(color = "#4A4A4A", size = 9.5),
      plot.title  = element_text(color = "#1F1F1F", face = "bold", size = 13),
      plot.subtitle = element_text(color = "#5C5C5C", size = 10.5),
      legend.title = element_text(color = "#2F2F2F", size = 10),
      legend.text  = element_text(color = "#4A4A4A", size = 9)
    )
}

# set output path

output_path <- "figures"
dir.create(output_path, showWarnings = FALSE)

# figure 1: airports with the longest average delays in 2025

figure_1 <-
  ggplot(
    top_airports_delay_2025,
    aes(x = reporting_airport, y = avg_delay)
  ) +
  geom_col(fill = col_delay, width = 0.72) +
  geom_text(
    aes(label = number(avg_delay, accuracy = 0.1)),
    hjust = -0.15, size = 3.2, color = "#3F3F3F"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(
    title    = "Airports with the longest average delays in 2025",
    subtitle = "Scheduled flights only, weighted by matched flights",
    x = NULL,
    y = "Average delay (minutes)"
  ) +
  theme_delays()

figure_1

ggsave(
  file.path(output_path, "figure_1_airport_delays_2025.png"),
  figure_1,
  width = 8, height = 5.5, dpi = 300, bg = "white"
)

# figure 2: airports with the most cancellations in 2025

figure_2 <-
  ggplot(
    top_airports_cancel_2025,
    aes(x = reporting_airport, y = pct_cancelled)
  ) +
  geom_col(fill = col_cancel, width = 0.72) +
  geom_text(
    aes(label = number(pct_cancelled, accuracy = 0.1)),
    hjust = -0.15, size = 3.2, color = "#3F3F3F"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(
    title    = "Airports with the most cancellations in 2025",
    subtitle = "Scheduled flights only, weighted by matched flights",
    x = NULL,
    y = "Cancelled flights (%)"
  ) +
  theme_delays()

figure_2

ggsave(
  file.path(output_path, "figure_2_airport_cancellations_2025.png"),
  figure_2,
  width = 8, height = 5.5, dpi = 300, bg = "white"
)

# figure 3: airport-airline pairs with the longest delays in 2025

figure_3 <-
  ggplot(
    top_airport_airline_2025,
    aes(x = airport_airline, y = avg_delay)
  ) +
  geom_col(fill = col_accent, width = 0.72) +
  geom_text(
    aes(label = number(avg_delay, accuracy = 0.1)),
    hjust = -0.15, size = 3.1, color = "#3F3F3F"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(
    title    = "Airport-airline pairs with the longest delays in 2025",
    subtitle = "Scheduled flights only, minimum 300 matched flights",
    x = NULL,
    y = "Average delay (minutes)"
  ) +
  theme_delays()

figure_3

ggsave(
  file.path(output_path, "figure_3_airport_airline_delays_2025.png"),
  figure_3,
  width = 8.5, height = 5.5, dpi = 300, bg = "white"
)

# figure 4: delays vs cancellations scatter in 2025

figure_4 <-
  airport_summary |>
  filter(reporting_period == 2025, flights >= 5000) |>
  ggplot(aes(x = pct_cancelled, y = avg_delay, size = flights)) +
  geom_point(
    color = col_dark, fill = col_delay,
    alpha = 0.75, shape = 21, stroke = 0.3
  ) +
  geom_smooth(
    method = "lm", se = FALSE,
    color = "#8A8A8A", linewidth = 0.7
  ) +
  geom_text_repel(
    aes(label = reporting_airport),
    size = 3.0, color = "#3F3F3F", max.overlaps = 12
  ) +
  scale_size_continuous(
    range = c(3, 11),
    labels = label_number(big.mark = ",")
  ) +
  labs(
    title    = "Delays and cancellations across airports in 2025",
    subtitle = "Bigger points represent more matched flights",
    x    = "Cancelled flights (%)",
    y    = "Average delay (minutes)",
    size = "Matched flights"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.35),
    axis.title   = element_text(color = "#2F2F2F", size = 10),
    axis.text    = element_text(color = "#4A4A4A", size = 9.5),
    plot.title   = element_text(color = "#1F1F1F", face = "bold", size = 13),
    plot.subtitle = element_text(color = "#5C5C5C", size = 10.5),
    legend.title = element_text(color = "#2F2F2F", size = 10),
    legend.text  = element_text(color = "#4A4A4A", size = 9)
  )

figure_4

ggsave(
  file.path(output_path, "figure_4_delay_vs_cancellations_2025.png"),
  figure_4,
  width = 9, height = 6.5, dpi = 300, bg = "white"
)

# figure 5: improvement in average delay from 2023 to 2025

figure_5 <-
  ggplot(
    top_airport_change,
    aes(x = reporting_airport, y = change_2023_2025)
  ) +
  geom_col(fill = col_improve, width = 0.72) +
  geom_hline(yintercept = 0, color = "#BBBBBB", linewidth = 0.5) +
  geom_text(
    aes(
      label  = number(change_2023_2025, accuracy = 0.1),
      hjust  = label_hjust
    ),
    size = 3.1, color = "#3F3F3F"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title    = "Where delays improved most from 2023 to 2025",
    subtitle = "Airports with at least 5,000 matched flights",
    x = NULL,
    y = "Change in average delay (minutes; negative means improvement)"
  ) +
  theme_delays()

figure_5

ggsave(
  file.path(output_path, "figure_5_delay_change_2023_2025.png"),
  figure_5,
  width = 8.5, height = 5.5, dpi = 300, bg = "white"
)

# regression model

model_data <-
  analysis_data |>
  filter(reporting_period == 2025) |>
  mutate(
    log_flights = log1p(number_flights_matched),
    international_route = if_else(
      origin_destination_country == "United Kingdom", 0, 1
    )
  ) |>
  select(
    average_delay_mins,
    reporting_airport,
    airline_name,
    log_flights,
    flights_cancelled_percent,
    international_route
  ) |>
  drop_na()

delay_model <-
  lm(
    average_delay_mins ~ reporting_airport +
      airline_name +
      log_flights +
      flights_cancelled_percent +
      international_route,
    data = model_data
  )

summary(delay_model)

# tidy results and model fit metrics

model_results <-
  tidy(delay_model) |>
  arrange(desc(abs(estimate)))

model_performance <-
  glance(delay_model) |>
  transmute(
    r_squared     = r.squared,
    adj_r_squared = adj.r.squared,
    rmse          = sigma
  )

model_results
model_performance

# export model outputs

write_csv(model_results,     file.path(output_path, "model_results.csv"))
write_csv(model_performance, file.path(output_path, "model_performance.csv"))

# augmented data for figures 6 and 7

model_augmented <-
  augment(delay_model, data = model_data)

# figure 6: predicted vs observed delays

figure_6 <-
  ggplot(
    model_augmented,
    aes(x = .fitted, y = average_delay_mins)
  ) +
  geom_point(
    color = col_delay, alpha = 0.45, size = 1.4
  ) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "#8A8A8A", linewidth = 0.7
  ) +
  labs(
    title    = "Predicted and observed delays line up fairly closely",
    subtitle = "Each point is an airport-airline-route observation in 2025",
    x = "Predicted delay (minutes)",
    y = "Observed delay (minutes)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.35),
    axis.title   = element_text(color = "#2F2F2F", size = 10),
    axis.text    = element_text(color = "#4A4A4A", size = 9.5),
    plot.title   = element_text(color = "#1F1F1F", face = "bold", size = 13),
    plot.subtitle = element_text(color = "#5C5C5C", size = 10.5)
  )

figure_6

ggsave(
  file.path(output_path, "figure_6_predicted_vs_observed_delay.png"),
  figure_6,
  width = 7.5, height = 6, dpi = 300, bg = "white"
)

# figure 7: largest positive airport effects from the model

airport_effects <-
  model_results |>
  filter(str_starts(term, "reporting_airport")) |>
  mutate(
    reporting_airport = str_remove(term, "^reporting_airport"),
    reporting_airport = fct_reorder(reporting_airport, estimate)
  ) |>
  filter(estimate > 0) |>
  arrange(desc(estimate)) |>
  slice_head(n = 10) |>
  mutate(
    reporting_airport = fct_reorder(reporting_airport, estimate)
  )

figure_7 <-
  ggplot(
    airport_effects,
    aes(x = reporting_airport, y = estimate)
  ) +
  geom_col(fill = col_accent, width = 0.72) +
  geom_text(
    aes(label = number(estimate, accuracy = 0.1)),
    hjust = -0.15, size = 3.2, color = "#3F3F3F"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Some airports still stand out in the model",
    subtitle = "Largest positive airport effects after adjustment",
    x = NULL,
    y = "Estimated additional delay (minutes)"
  ) +
  theme_delays()

figure_7

ggsave(
  file.path(output_path, "figure_7_model_airport_effects.png"),
  figure_7,
  width = 8, height = 5.5, dpi = 300, bg = "white"
)
