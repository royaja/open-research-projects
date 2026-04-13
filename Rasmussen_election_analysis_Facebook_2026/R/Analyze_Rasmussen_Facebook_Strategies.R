# Public repository version
# The underlying Facebook dataset is not included in this archive.
# To rerun the script, place the local source file in the working directory
# and update the input path below as needed.

# Analyze Facebook posts from the Danish election
# Posts published by Lars L??kke Rasmussen
# Period: February 28 to March 24, 2026

# load packages
library(tidyverse)
library(scales)
library(readxl)

# load data
rasmussen_coded <-
  read_xlsx("Facebook_Rasmussen_Coded_Danmark.xlsx")

# prepare data
strategy_levels <- c(
  "Midten som ansvar og balanse",
  "Valgkamplogistikk eller annet",
  "Statsmannen i urolig tid",
  "Samling mot splittelse",
  "Den personlige"
)

reaction_vars <- c(
  "likes",
  "sad",
  "care",
  "love",
  "haha",
  "wow",
  "angry"
)

rasmussen_coded <-
  rasmussen_coded %>%
  mutate(
    reactions_total = rowSums(across(all_of(reaction_vars)), na.rm = TRUE),
    engagement_total = reactions_total + coalesce(comments, 0) + coalesce(shares, 0),
    strategy_label = case_when(
      strategy_label == "valgkamplogistikk eller annet" ~ "Valgkamplogistikk eller annet",
      TRUE ~ strategy_label
    )
  )

strategy_data <-
  rasmussen_coded %>%
  filter(!is.na(strategy_label))

# figure 1: overperformance by strategy
plot_data <-
  strategy_data %>%
  group_by(strategy_label) %>%
  summarise(
    n_posts = n(),
    total_engagement = sum(engagement_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share_posts = n_posts / sum(n_posts),
    share_engagement = total_engagement / sum(total_engagement),
    diff_pp = (share_engagement - share_posts) * 100,
    strategy_label = factor(strategy_label, levels = strategy_levels),
    direction = if_else(diff_pp >= 0, "Overpresterer", "Underpresterer"),
    label = paste0(
      if_else(diff_pp > 0, "+", ""),
      round(diff_pp, 1),
      " pp"
    ),
    label_x = if_else(diff_pp >= 0, diff_pp + 0.18, diff_pp - 0.18),
    label_hjust = if_else(diff_pp >= 0, 0, 1)
  )

plot_overperformance <-
  ggplot(plot_data, aes(x = diff_pp, y = strategy_label, fill = direction)) +
  geom_col(width = 0.62) +
  geom_vline(xintercept = 0, linewidth = 0.9, color = "#9CA3AF") +
  geom_text(
    aes(x = label_x, label = label, hjust = label_hjust),
    size = 4,
    color = "#1F2937"
  ) +
  scale_fill_manual(
    values = c(
      "Overpresterer" = "#4E3D8F",
      "Underpresterer" = "#B8B2CF"
    )
  ) +
  scale_x_continuous(
    limits = c(-4.5, 8.5),
    breaks = c(-4, 0, 4, 8),
    labels = function(x) paste0(x, " pp"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Hvilke fortellertyper overpresterer?",
    subtitle = "Forskjellen mellom andel av samlet engasjement og andel av alle poster",
    x = "Forskjell i prosentpoeng",
    y = NULL
  ) +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#111827"),
    plot.subtitle = element_text(
      size = 11.5,
      color = "#4B5563",
      margin = margin(b = 10)
    ),
    axis.text.y = element_text(size = 11.5, color = "#111827"),
    axis.text.x = element_text(size = 10.5, color = "#4B5563"),
    axis.title.x = element_text(
      size = 11.5,
      color = "#111827",
      margin = margin(t = 8)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.5),
    legend.position = "none",
    plot.margin = margin(15, 30, 10, 10)
  )

plot_overperformance

ggsave(
  "figur_rasmussen_overprestasjon.png",
  plot = plot_overperformance,
  width = 9,
  height = 5.6,
  dpi = 400,
  bg = "white"
)


# figure 2: number of posts by strategy
strategy_posts <-
  strategy_data %>%
  group_by(strategy_label) %>%
  summarise(
    n_posts = n(),
    .groups = "drop"
  ) %>%
  mutate(
    strategy_label = factor(strategy_label, levels = strategy_levels),
    label = paste0(n_posts, " poster")
  ) %>%
  arrange(n_posts)

plot_posts <-
  ggplot(strategy_posts, aes(x = n_posts, y = fct_reorder(strategy_label, n_posts))) +
  geom_col(fill = "#7C8A96", width = 0.62) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 4,
    color = "#1F2937"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Hvilke fortellertyper brukte han mest?",
    subtitle = "Antall Facebook-poster i hver fortellertype",
    x = "Antall poster",
    y = NULL
  ) +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#111827"),
    plot.subtitle = element_text(
      size = 11.5,
      color = "#4B5563",
      margin = margin(b = 10)
    ),
    axis.text.y = element_text(size = 11.5, color = "#111827"),
    axis.text.x = element_text(size = 10.5, color = "#4B5563"),
    axis.title.x = element_text(
      size = 11.5,
      color = "#111827",
      margin = margin(t = 8)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.5),
    plot.margin = margin(15, 30, 10, 10)
  )

plot_posts

ggsave(
  "figur_rasmussen_antall_poster_strategi.png",
  plot = plot_posts,
  width = 9,
  height = 5.6,
  dpi = 400,
  bg = "white"
)

# summary table
strategy_summary <-
  strategy_data %>%
  group_by(strategy_label) %>%
  summarise(
    n_posts = n(),
    total_engagement = sum(engagement_total, na.rm = TRUE),
    median_engagement = median(engagement_total, na.rm = TRUE),
    mean_engagement = mean(engagement_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share_posts = n_posts / sum(n_posts),
    share_total_engagement = total_engagement / sum(total_engagement),
    overperformance_pp = (share_total_engagement - share_posts) * 100
  ) %>%
  arrange(desc(total_engagement))

write_csv(
  strategy_summary,
  "tabell_rasmussen_strategioppsummering.csv"
)


