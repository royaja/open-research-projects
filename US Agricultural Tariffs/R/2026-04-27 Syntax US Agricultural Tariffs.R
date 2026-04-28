
# TidyTuesday Week 17 
# In this weeks TidyTuesday challenge, data comes from the USITC Tariff 
# Database, which contains tariff rates for all products imported into 
# the United States from 1997-2025. 
# The data focuses on agricultural and food products (HTS Chapters 1-24), 
# covering everything from live animals to beverages.

# I will focus on telling a story with policy implications.
# Two datasets will be loaded: tariff_agricultural.csv and agreements.csv
# An interesting variable in the dataset is ad_val_rate. 
# Ad valorem (percentage) portion of the duty rate (0.05 = 5%).

# Code developed by: R.A.Jacobsen 

# Load library 
library(tidyverse)
library(showtext)

# font set to Lato
sysfonts::font_add_google("Lato", "lato")
showtext_auto()

# Read dataset from Github
agreements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/agreements.csv')
tariff_agricultural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/tariff_agricultural.csv')


# Wrangle

# I focus on 2025 since it is the most recent year.
# Only ad valorem and duty-free lines are used because specific 
# per-unit duties cannot be compared as percentages without extra data.

year_start <- as.Date("2025-01-01")
year_end   <- as.Date("2025-12-31")
min_lines  <- 5

agreement_lookup <- tribble(
  ~agreement,    ~agreement_label,
  "mfn",         "MFN",
  "usmca",       "USMCA",
  "chile",       "Chile",
  "korea",       "Korea",
  "australia",   "Australia",
  "singapore",   "Singapore",
  "dr_cafta",    "DR-CAFTA"
)

chapter_names <- tibble(
  chapter = str_pad(1:24, width = 2, pad = "0"),
  chapter_name = c(
    "Live animals", "Meat", "Fish and seafood", "Dairy, eggs and honey",
    "Other animal products", "Live trees and plants", "Vegetables",
    "Fruit and nuts", "Coffee, tea and spices", "Cereals", "Milling products",
    "Oil seeds and grains", "Gums and resins", "Vegetable materials",
    "Fats and oils", "Prepared meat and fish", "Sugars", "Cocoa",
    "Cereal preparations", "Fruit and vegetable preparations",
    "Misc. edible preparations", "Beverages", "Food industry residues", "Tobacco"
  )
)

# USMCA has many missing rate type values. When there is no specific 
# duty attached, I treat these as duty-free.

tariffs_clean <- 
  tariff_agricultural %>%
  mutate(
    begin_effective_date = as.Date(begin_effective_date),
    end_effective_date   = as.Date(end_effective_date)
  ) %>%
  filter(
    begin_effective_date <= year_end,
    end_effective_date   >= year_start,
    agreement %in% agreement_lookup$agreement,
    is.na(specific_rate) | specific_rate == 0
  ) %>%
  mutate(
    rate_used = case_when(
      rate_type_code == "7" & !is.na(ad_val_rate) ~ ad_val_rate,
      rate_type_code == "0" ~ 0,
      agreement == "usmca" & is.na(rate_type_code) & is.na(ad_val_rate) ~ 0,
      TRUE ~ NA_real_
    ),
    chapter = str_sub(hts8, 1, 2)
  ) %>%
  filter(!is.na(rate_used), rate_used >= 0, rate_used <= 1) %>%
  left_join(agreement_lookup, by = "agreement") %>%
  left_join(chapter_names, by = "chapter") %>%
  filter(!is.na(chapter_name))

# Some HTS8 lines appear more than once, so I collapse them first.

tariff_line_rates <- 
  tariffs_clean %>%
  group_by(hts8, chapter, chapter_name, agreement, agreement_label) %>%
  summarise(rate = mean(rate_used, na.rm = TRUE), .groups = "drop")

chapter_rates <- 
  tariff_line_rates %>%
  group_by(chapter, chapter_name, agreement, agreement_label) %>%
  summarise(
    avg_rate = mean(rate, na.rm = TRUE),
    n_lines  = n_distinct(hts8),
    .groups  = "drop"
  ) %>%
  filter(n_lines >= min_lines)

# Sorting chapters by MFN rate makes the heatmap easier to read,
# so the protected chapters are not scattered randomly.

chapter_order <- 
  chapter_rates %>%
  filter(agreement == "mfn") %>%
  arrange(avg_rate) %>%
  mutate(chapter_label = paste0(chapter, " ", chapter_name)) %>%
  pull(chapter_label)

heatmap_data <- 
  chapter_rates %>%
  mutate(
    chapter_label = factor(
      paste0(chapter, " ", chapter_name),
      levels = chapter_order
    ),
    agreement_label = factor(
      agreement_label,
      levels = agreement_lookup$agreement_label
    )
  )

# MFN rate minus average preferential rate.

mfn_by_chapter <- 
  chapter_rates %>%
  filter(agreement == "mfn") %>%
  select(chapter, chapter_name, mfn_rate = avg_rate)

preferential_by_chapter <- 
  chapter_rates %>%
  filter(agreement != "mfn") %>%
  group_by(chapter, chapter_name) %>%
  summarise(
    pref_rate    = mean(avg_rate, na.rm = TRUE),
    n_agreements = n_distinct(agreement),
    .groups      = "drop"
  )

discount_by_chapter <- 
  mfn_by_chapter %>%
  inner_join(preferential_by_chapter, by = c("chapter", "chapter_name")) %>%
  mutate(
    discount      = mfn_rate - pref_rate,
    discount_pp   = discount * 100,
    chapter_label = paste0(chapter, " ", chapter_name)
  ) %>%
  filter(n_agreements >= 3, mfn_rate > 0.005, discount > 0) %>%
  arrange(desc(discount))

# Each agreement is compared with the MFN rate for the same HTS8 line.

mfn_by_line <- 
  tariff_line_rates %>%
  filter(agreement == "mfn") %>%
  select(hts8, mfn_rate = rate)

discount_by_agreement <- 
  tariff_line_rates %>%
  filter(agreement != "mfn") %>%
  inner_join(mfn_by_line, by = "hts8") %>%
  mutate(discount = mfn_rate - rate) %>%
  group_by(agreement, agreement_label) %>%
  summarise(
    avg_mfn_rate       = mean(mfn_rate, na.rm = TRUE),
    avg_agreement_rate = mean(rate, na.rm = TRUE),
    avg_discount       = mean(discount, na.rm = TRUE),
    discount_pp        = avg_discount * 100,
    n_lines            = n_distinct(hts8),
    .groups            = "drop"
  ) %>%
  filter(n_lines >= 50) %>%
  arrange(desc(avg_discount))


# Plot
# According to Google Gemini (27.04.2026), the red color found in charts from
# The Economist uses the hex code: #E3120B. This will be applied to the plots. 

heatmap_plot_data <- 
  heatmap_data %>%
  mutate(
    chapter_label = factor(
      str_remove(as.character(chapter_label), "^\\d{2}\\s+"),
      levels = str_remove(levels(heatmap_data$chapter_label), "^\\d{2}\\s+")
    ),
    rate_label = case_when(
      avg_rate < 0.001 ~ "0%",
      TRUE ~ paste0(round(avg_rate * 100, 1), "%")
    ),
    light_text = avg_rate > 0.05
  )


plot1 <- 
  ggplot(
    heatmap_plot_data,
    aes(
      x = agreement_label,
      y = chapter_label,
      fill = avg_rate
    )
  ) +
  geom_tile(
    color = "white",
    linewidth = 1.1
  ) +
  geom_text(
    aes(
      label = rate_label,
      color = light_text
    ),
    family = "lato",
    size = 3.1
  ) +
  scale_fill_gradient(
    low = "#F4EFE9",
    high = "#E3120B",
    name = NULL,
    labels = scales::label_percent(scale = 100),
    na.value = "#F8F6F2",
    limits = c(0, NA)
  ) +
  scale_color_manual(
    values = c(
      `TRUE` = "white",
      `FALSE` = "#3A3A3A"
    ),
    guide = "none"
  ) +
  scale_x_discrete(
    position = "top"
  ) +
  labs(
    title = "US agricultural tariffs are concentrated under MFN rates",
    subtitle = "Average comparable tariff rates by HTS chapter and selected trade agreement, 2025",
    caption = "R.A. Jacobsen | @AulieRoy | Source: USITC Tariff Database",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(
    base_family = "lato",
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "#1A1A1A",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "#5A5A5A",
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#8A8A8A",
      hjust = 0,
      margin = margin(t = 16),
      lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(
      fill = "#FBFAF7",
      color = NA
    ),
    panel.background = element_rect(
      fill = "#FBFAF7",
      color = NA
    ),
    panel.grid = element_blank(),
    axis.text.x.top = element_text(
      size = 11,
      color = "#3A3A3A",
      face = "plain",
      margin = margin(b = 8)
    ),
    axis.text.y = element_text(
      size = 10,
      color = "#3A3A3A",
      margin = margin(r = 4)
    ),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 25, 15, 20)
  )

plot1

# Plot 2: Where do trade agreements reduce tariffs? 
plot2_data <- 
  discount_by_chapter %>%
  arrange(desc(discount_pp)) %>%
  mutate(
    chapter_label = factor(chapter_name, levels = rev(chapter_name)),
    highlight     = row_number() <= 3
  )


plot2 <- 
  ggplot(plot2_data, aes(x = discount_pp, y = chapter_label)) +
  geom_col(
    aes(fill = highlight),
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(round(discount_pp, 1), "%")),
    family = "lato",
    size   = 3.3,
    color  = "#3A3A3A",
    hjust  = -0.25
  ) +
  scale_fill_manual(
    values = c(
      `TRUE`  = "#E3120B",
      `FALSE` = "#F2A6A2"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Trade agreements bite hardest where MFN tariffs are highest",
    subtitle = "Average tariff discount under US trade agreements vs. MFN, by HTS chapter, 2025",
    caption  = "R.A. Jacobsen | @AulieRoy | Source: USITC Tariff Database",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(
    base_family = "lato",
    base_size   = 12
  ) +
  theme(
    plot.title = element_text(
      size   = 18,
      face   = "bold",
      color  = "#1A1A1A",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size   = 12,
      color  = "#5A5A5A",
      margin = margin(b = 22)
    ),
    plot.caption = element_text(
      size       = 9,
      color      = "#8A8A8A",
      hjust      = 0,
      margin     = margin(t = 16),
      lineheight = 1.2
    ),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.background       = element_rect(fill = "#FBFAF7", color = NA),
    panel.background      = element_rect(fill = "#FBFAF7", color = NA),
    panel.grid.major.y    = element_blank(),
    panel.grid.minor      = element_blank(),
    panel.grid.major.x    = element_line(color = "#EAE6DF", linewidth = 0.3),
    axis.text.x = element_text(
      size  = 10,
      color = "#5A5A5A"
    ),
    axis.text.y = element_text(
      size   = 11,
      color  = "#3A3A3A",
      margin = margin(r = 6)
    ),
    axis.ticks  = element_blank(),
    plot.margin = margin(20, 25, 15, 20)
  )

plot2

# Plot 3: Which agreements offers the largest average tariff reductions? 

plot3_data <- 
  discount_by_agreement %>%
  arrange(desc(discount_pp)) %>%
  mutate(
    agreement_label = factor(agreement_label, levels = rev(agreement_label)),
    highlight       = row_number() <= 3
  )


plot3 <- 
  ggplot(plot3_data, aes(x = discount_pp, y = agreement_label)) +
  geom_col(
    aes(fill = highlight),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(discount_pp, 1), "%")),
    family = "lato",
    size   = 3.3,
    color  = "#3A3A3A",
    hjust  = -0.25
  ) +
  scale_fill_manual(
    values = c(
      `TRUE`  = "#E3120B",
      `FALSE` = "#F2A6A2"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title    = "US trade agreements deliver remarkably similar tariff cuts",
    subtitle = "Average ad valorem tariff reduction vs. MFN, by US trade agreement, 2025",
    caption  = "R.A. Jacobsen | @AulieRoy | Source: USITC Tariff Database",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(
    base_family = "lato",
    base_size   = 12
  ) +
  theme(
    plot.title = element_text(
      size   = 18,
      face   = "bold",
      color  = "#1A1A1A",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size   = 12,
      color  = "#5A5A5A",
      margin = margin(b = 22)
    ),
    plot.caption = element_text(
      size       = 9,
      color      = "#8A8A8A",
      hjust      = 0,
      margin     = margin(t = 16),
      lineheight = 1.2
    ),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.background       = element_rect(fill = "#FBFAF7", color = NA),
    panel.background      = element_rect(fill = "#FBFAF7", color = NA),
    panel.grid.major.y    = element_blank(),
    panel.grid.minor      = element_blank(),
    panel.grid.major.x    = element_line(color = "#EAE6DF", linewidth = 0.3),
    axis.text.x = element_text(
      size  = 10,
      color = "#5A5A5A"
    ),
    axis.text.y = element_text(
      size   = 11,
      color  = "#3A3A3A",
      margin = margin(r = 6)
    ),
    axis.ticks  = element_blank(),
    plot.margin = margin(20, 25, 15, 20)
  )

plot3
