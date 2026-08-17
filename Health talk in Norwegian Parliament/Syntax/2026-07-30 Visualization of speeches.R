

# What Norwegian politicians mean by access to healthcare

# This script is part of a project examining how access to healthcare is
# discussed in speeches from the Norwegian Parliament. It identifies six
# dimensions of healthcare access using a predefined Norwegian keyword
# dictionary, compares attention across the left, center, and right, and
# creates the figures and output files used in the project summary.

# The analysis is descriptive. A speech may be assigned to more than one
# dimension, and the results show patterns of political attention rather
# than the actual quality or availability of healthcare services.

# Chatgpt was used to help restructure part of the code. 

# Code developed by R. A. Jacobsen

# Load packages
library(tidyverse)

# Load speeches
speeches <-
  readRDS(
    "data/raw/all_speeches_2025-07-28_2026-07-28.rds"
  )

health_speeches <-
  readRDS(
    "C:/Users/roja006/Documents/data/processed/health_speeches.rds"
  )

# Define the analysis period
date_from <- as.Date("2026-01-01")
date_to <-
  max(
    as.Date(speeches$publication_date),
    na.rm = TRUE
  )

# Prepare the speech data
party_pattern <-
  "(?<=\\()(A|H|FrP|SV|Sp|V|KrF|R|MDG|PF|Uav)(?=\\))"
prepare_speeches <- function(data) {
  data |>
    mutate(
      publication_date = as.Date(
        publication_date
      ),
      speech_id = paste(
        publication_id,
        speech_number,
        sep = "_"
      ),
      speaker_raw = str_squish(
        speaker_raw
      ),
      text = replace_na(
        str_squish(
          text
        ),
        ""
      ),
      party = str_extract(
        speaker_raw,
        party_pattern
      ),
      speaker = speaker_raw |>
        str_remove(
          "\\s*\\((A|H|FrP|SV|Sp|V|KrF|R|MDG|PF|Uav)\\)"
        ) |>
        str_remove(
          "\\s*\\[?\\d{2}:\\d{2}:\\d{2}\\]?\\s*:?\\s*$"
        ) |>
        str_remove(
          "\\s*:\\s*$"
        ) |>
        str_squish(),
      bloc = case_when(
        party %in% c(
          "A",
          "SV",
          "R"
        ) ~ "Left",
        party %in% c(
          "Sp",
          "V",
          "KrF",
          "MDG"
        ) ~ "Center",
        party %in% c(
          "H",
          "FrP"
        ) ~ "Right",
        TRUE ~ NA_character_
      )
    ) |>
    filter(
      publication_date >= date_from,
      publication_date <= date_to
    )
}
speeches <-
  prepare_speeches(
    speeches
  )
health_speeches <-
  prepare_speeches(
    health_speeches
  )

# Define the dimensions of healthcare access
waiting_terms <-
  c(
    "ventetid",
    "ventelist",
    "helsek\u00F8",
    "behandlingsk\u00F8",
    "fristbrudd",
    "behandlingskapasitet",
    "manglende kapasitet",
    "for liten kapasitet"
  )

geography_terms <-
  c(
    "likeverdig helsetjenest",
    "likeverdig tilbud",
    "geografisk forskjell",
    "geografisk ulikhet",
    "distrikt",
    "lokalsykehus",
    "n\u00E6rsykehus",
    "reiseavstand",
    "uavhengig av bosted",
    "desentral",
    "f\u00F8detilbud",
    "f\u00F8deavdeling"
  )

financial_terms <-
  c(
    "egenandel",
    "frikort",
    "brukerbetaling",
    "betalingsbarriere",
    "r\u00E5d til behandling",
    "r\u00E5d til helsehjelp",
    "betaling for helsehjelp",
    "\u00F8konomisk barriere"
  )

rights_terms <-
  c(
    "pasientrett",
    "rett til behandling",
    "rett til helsehjelp",
    "rett til n\u00F8dvendig helsehjelp",
    "fritt behandlingsvalg",
    "valgfrihet",
    "brukermedvirkning",
    "samvalg"
  )

primary_care_terms <-
  c(
    "fastlege",
    "legevakt",
    "kommunehelsetjenest",
    "prim\u00E6rhelsetjenest",
    "allmennlege",
    "pasientforl\u00F8p",
    "samhandling",
    "kontinuitet"
  )

personnel_terms <-
  c(
    "helsepersonellmangel",
    "mangel p\u00E5 helsepersonell",
    "sykepleiermangel",
    "mangel p\u00E5 sykepleier",
    "legemangel",
    "fastlegemangel",
    "bemanningsproblem",
    "bemanningsutfordring",
    "underbemanning",
    "rekrutteringsproblem",
    "rekrutteringsutfordring",
    "rekruttere helsepersonell",
    "beholde helsepersonell"
  )

# Check whether a speech contains at least one term
contains_any <- function(text, terms) {
  map(
    terms,
    function(term) {
      str_detect(
        text,
        fixed(
          term,
          ignore_case = TRUE
        )
      )
    }
  ) |>
    reduce(
      `|`
    )
}

# Classify speeches by dimensions of healthcare access
health_speeches <-
  health_speeches |>
  mutate(
    text = replace_na(
      as.character(
        text
      ),
      ""
    ),
    access_waiting = contains_any(
      text,
      waiting_terms
    ),
    access_geography = contains_any(
      text,
      geography_terms
    ),
    access_financial = contains_any(
      text,
      financial_terms
    ),
    access_rights = contains_any(
      text,
      rights_terms
    ),
    access_primary_care = contains_any(
      text,
      primary_care_terms
    ),
    access_personnel = contains_any(
      text,
      personnel_terms
    ),
    access_to_healthcare =
      access_waiting |
      access_geography |
      access_financial |
      access_rights |
      access_primary_care |
      access_personnel
  )


access_speeches <-
  health_speeches |>
  filter(
    access_to_healthcare
  )
speeches <-
  speeches |>
  mutate(
    access_to_healthcare =
      speech_id %in% access_speeches$speech_id
  )

# Create a long dataset of the access dimensions
dimension_labels <-
  c(
    access_waiting =
      "Waiting times and capacity",
    access_geography =
      "Geography and local services",
    access_financial =
      "Financial barriers",
    access_rights =
      "Rights and choice",
    access_primary_care =
      "Primary care and continuity",
    access_personnel =
      "Staff shortages and recruitment"
  )
access_variables <-
  names(
    dimension_labels
  )
access_long <-
  access_speeches |>
  select(
    speech_id,
    publication_id,
    publication_date,
    speaker,
    party,
    bloc,
    all_of(
      access_variables
    )
  ) |>
  pivot_longer(
    cols = all_of(
      access_variables
    ),
    names_to = "dimension_code",
    values_to = "mentioned"
  ) |>
  filter(
    mentioned
  ) |>
  mutate(
    dimension = unname(
      dimension_labels[dimension_code]
    )
  )

# Create the output folder
dir.create(
  "output/story",
  recursive = TRUE,
  showWarnings = FALSE
)

# Define the colors and theme
white <- "#FFFFFF"
ink <- "#18252D"
grid <- "#D9D9D9"
accent <- "#D4573F"
blue <- "#46758B"
bloc_colors <-
  c(
    "Left" = "#B54B4B",
    "Center" = "#B58B34",
    "Right" = "#3F708C"
  )
label_separator <- " | "
story_theme <-
  theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  theme(
    text = element_text(
      color = ink
    ),
    plot.background = element_rect(
      fill = white,
      color = NA
    ),
    panel.background = element_rect(
      fill = white,
      color = NA
    ),
    axis.title = element_text(
      size = 11,
      color = ink
    ),
    axis.text = element_text(
      size = 10.5,
      color = ink
    ),
    panel.grid.major = element_line(
      color = grid,
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(
      color = ink
    ),
    legend.text = element_text(
      color = ink
    ),
    plot.margin = margin(
      20,
      32,
      20,
      20
    )
  )
theme_set(
  story_theme
)

# Figure 1
total_access_speeches <-
  n_distinct(
    access_speeches$speech_id
  )
dimension_summary <-
  access_long |>
  count(
    dimension,
    name = "speeches",
    sort = TRUE
  ) |>
  mutate(
    share =
      100 * speeches / total_access_speeches,
    label = paste0(
      speeches,
      " speeches",
      label_separator,
      round(
        share
      ),
      "%"
    )
  )
plot_dimensions <-
  dimension_summary |>
  mutate(
    dimension = fct_reorder(
      dimension,
      speeches
    ),
    highlight =
      dimension == "Geography and local services"
  ) |>
  ggplot(
    aes(
      x = dimension,
      y = speeches,
      fill = highlight
    )
  ) +
  geom_col(
    width = 0.68
  ) +
  geom_text(
    aes(
      label = label
    ),
    hjust = -0.08,
    color = ink,
    size = 3.7
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = accent,
      "FALSE" = ink
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.28
      )
    )
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

# Figure 2
bloc_order <-
  c(
    "Left",
    "Center",
    "Right"
  )
bloc_attention <-
  speeches |>
  filter(
    bloc %in% bloc_order
  ) |>
  group_by(
    bloc
  ) |>
  summarise(
    all_speeches = n(),
    access_speeches = sum(
      access_to_healthcare,
      na.rm = TRUE
    ),
    access_share =
      100 * access_speeches / all_speeches,
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(
      sprintf(
        "%.1f%%",
        access_share
      ),
      label_separator,
      "n = ",
      access_speeches
    )
  )
plot_blocs <-
  bloc_attention |>
  mutate(
    bloc = factor(
      bloc,
      levels = rev(
        bloc_order
      )
    )
  ) |>
  ggplot(
    aes(
      y = bloc,
      x = access_share
    )
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = access_share,
      yend = bloc
    ),
    color = grid,
    linewidth = 2.2
  ) +
  geom_point(
    aes(
      color = bloc
    ),
    size = 6
  ) +
  geom_text(
    aes(
      label = label
    ),
    nudge_x = 0.45,
    hjust = 0,
    fontface = "bold",
    color = ink,
    size = 4
  ) +
  scale_color_manual(
    values = bloc_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    labels = scales::label_percent(
      scale = 1,
      accuracy = 1
    ),
    limits = c(
      0,
      max(
        bloc_attention$access_share
      ) + 2.5
    )
  ) +
  labs(
    x = "Share of speeches",
    y = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )

# Figure 3
dimension_order <-
  c(
    "Waiting times and capacity",
    "Staff shortages and recruitment",
    "Primary care and continuity",
    "Geography and local services",
    "Rights and choice",
    "Financial barriers"
  )
bloc_dimensions <-
  access_long |>
  filter(
    bloc %in% bloc_order
  ) |>
  count(
    bloc,
    dimension,
    name = "speeches"
  ) |>
  complete(
    bloc = bloc_order,
    dimension = dimension_order,
    fill = list(
      speeches = 0
    )
  ) |>
  group_by(
    bloc
  ) |>
  mutate(
    bloc_share =
      100 * speeches / sum(speeches)
  ) |>
  ungroup()
overall_dimensions <-
  access_long |>
  filter(
    bloc %in% bloc_order
  ) |>
  count(
    dimension,
    name = "speeches"
  ) |>
  mutate(
    overall_share =
      100 * speeches / sum(speeches)
  ) |>
  select(
    dimension,
    overall_share
  )
bloc_dimensions <-
  bloc_dimensions |>
  left_join(
    overall_dimensions,
    by = "dimension"
  ) |>
  mutate(
    difference_pp =
      bloc_share - overall_share,
    label = sprintf(
      "%+.1f pp",
      difference_pp
    ),
    bloc = factor(
      bloc,
      levels = bloc_order
    ),
    dimension = factor(
      dimension,
      levels = rev(
        dimension_order
      )
    )
  )
heat_limit <-
  max(
    abs(
      bloc_dimensions$difference_pp
    ),
    na.rm = TRUE
  )
plot_heatmap <-
  ggplot(
    bloc_dimensions,
    aes(
      x = bloc,
      y = dimension,
      fill = difference_pp
    )
  ) +
  geom_tile(
    color = white,
    linewidth = 2
  ) +
  geom_text(
    aes(
      label = label
    ),
    color = ink,
    fontface = "bold",
    size = 3.8
  ) +
  scale_fill_gradient2(
    low = blue,
    mid = white,
    high = accent,
    midpoint = 0,
    limits = c(
      -heat_limit,
      heat_limit
    ),
    oob = scales::squish
  ) +
  scale_x_discrete(
    position = "top"
  ) +
  guides(
    fill = guide_colorbar(
      title = "Difference in percentage points",
      title.position = "top",
      barwidth = 10,
      barheight = 0.7
    )
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

# Figure 4
dimension_pairs <-
  combn(
    access_variables,
    2,
    simplify = FALSE
  )
combination_summary <-
  map_dfr(
    dimension_pairs,
    function(pair) {
      speeches_in_combination <-
        sum(
          access_speeches[[pair[1]]] &
            access_speeches[[pair[2]]],
          na.rm = TRUE
        )
      tibble(
        dimension_1 = unname(
          dimension_labels[pair[1]]
        ),
        dimension_2 = unname(
          dimension_labels[pair[2]]
        ),
        combination = paste(
          unname(
            dimension_labels[pair[1]]
          ),
          "&",
          unname(
            dimension_labels[pair[2]]
          )
        ),
        speeches = speeches_in_combination
      )
    }
  ) |>
  filter(
    speeches > 0
  ) |>
  mutate(
    share =
      100 * speeches / total_access_speeches
  ) |>
  slice_max(
    speeches,
    n = 7,
    with_ties = FALSE
  ) |>
  arrange(
    desc(
      speeches
    )
  ) |>
  mutate(
    label = paste0(
      speeches,
      " speeches",
      label_separator,
      sprintf(
        "%.1f%%",
        share
      )
    )
  )
plot_combinations <-
  combination_summary |>
  mutate(
    combination = str_wrap(
      combination,
      width = 36
    ),
    combination = fct_reorder(
      combination,
      speeches
    )
  ) |>
  ggplot(
    aes(
      x = combination,
      y = speeches
    )
  ) +
  geom_col(
    width = 0.68,
    fill = ink
  ) +
  geom_text(
    aes(
      label = label
    ),
    hjust = -0.08,
    color = ink,
    size = 3.7
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.30
      )
    )
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

# Figure 5
top_days <-
  access_speeches |>
  count(
    publication_date,
    name = "access_speeches"
  ) |>
  slice_max(
    access_speeches,
    n = 10,
    with_ties = FALSE
  ) |>
  mutate(
    date_label = format(
      publication_date,
      "%d %b"
    ),
    date_label = fct_reorder(
      date_label,
      access_speeches
    )
  )
plot_days <-
  ggplot(
    top_days,
    aes(
      y = date_label,
      x = access_speeches
    )
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = access_speeches,
      yend = date_label
    ),
    color = grid,
    linewidth = 1.5
  ) +
  geom_point(
    color = accent,
    size = 4.8
  ) +
  geom_text(
    aes(
      label = access_speeches
    ),
    nudge_x = 3,
    hjust = 0,
    color = ink,
    fontface = "bold",
    size = 3.8
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  labs(
    x = "Access-related speeches",
    y = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )

# Export speeches from the sitting days with the most attention
spike_speeches <-
  access_speeches |>
  semi_join(
    top_days |>
      select(
        publication_date
      ),
    by = "publication_date"
  ) |>
  arrange(
    publication_date,
    publication_id,
    speech_number
  ) |>
  select(
    publication_date,
    publication_id,
    publication_title,
    speaker,
    party,
    bloc,
    speech_type,
    text
  )

# Save the figures
ggsave(
  "output/story/Figure_1_access_dimensions.png",
  plot = plot_dimensions,
  width = 10,
  height = 6.5,
  dpi = 320,
  bg = "white"
)
ggsave(
  "output/story/Figure_2_attention_by_bloc.png",
  plot = plot_blocs,
  width = 10,
  height = 6,
  dpi = 320,
  bg = "white"
)
ggsave(
  "output/story/Figure_3_bloc_emphasis_heatmap.png",
  plot = plot_heatmap,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)
ggsave(
  "output/story/Figure_4_access_dimension_combinations.png",
  plot = plot_combinations,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)
ggsave(
  "output/story/Figure_5_peak_sitting_days.png",
  plot = plot_days,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)

# Save the output data
write_csv(
  dimension_summary,
  "output/story/access_dimension_summary.csv"
)
write_csv(
  bloc_attention,
  "output/story/access_attention_by_bloc.csv"
)
write_csv(
  bloc_dimensions,
  "output/story/access_dimensions_by_bloc.csv"
)
write_csv(
  combination_summary,
  "output/story/access_dimension_combinations.csv"
)
write_csv(
  top_days,
  "output/story/top_access_days.csv"
)
write_csv(
  spike_speeches,
  "output/story/speeches_from_peak_days.csv"
)
