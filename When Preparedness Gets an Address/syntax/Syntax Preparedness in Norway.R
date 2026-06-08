
# This project builds on three datasets downloaded from GeoNorge
# The three datasets are as follows: (1) Offentlige tilfluktsrom, 
# (2) Befolkning p?? rutenett 1000 m og (3) Administrative enheter, kommuner. 
# Aim of this analysis is to look into preparedness in Norway, focusing on 
# public shelters and population patterns. 

# Code was developed by R.A.Jacobsen 

# Load libraries
library(tidyverse)
library(sf)
library(janitor)
library(stringi)
library(showtext)

options(scipen = 999)

font_add_google("Lato", "Lato")
showtext_auto()
font_family <- "Lato"

dir.create("figures", showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)


# Load data
tilfluktsrom <- st_read(
  "C:/Users/roja006/OneDrive - Kristiania/[00] Inbox/TilfluktsromOffentlige.json",
  quiet = TRUE
) %>%
  clean_names()

befolkning <- st_read(
  "C:/Users/roja006/OneDrive - Kristiania/[00] Inbox/Befolkning_0000_Norge_25833_BefolkningsstatistikkRutenett1km2025_GML.gml",
  quiet = TRUE
) %>%
  clean_names()

kommune <- st_read(
  "C:/Users/roja006/OneDrive - Kristiania/[00] Inbox/Basisdata_0000_Norge_25833_Kommune_GeoJSON.geojson",
  quiet = TRUE
) %>%
  clean_names()


# Wrangle 
tilfluktsrom <- tilfluktsrom[!st_is_empty(tilfluktsrom), ]
befolkning <- befolkning[!st_is_empty(befolkning), ]
kommune <- kommune[!st_is_empty(kommune), ]

if (is.na(st_crs(tilfluktsrom))) st_crs(tilfluktsrom) <- 25833
if (is.na(st_crs(befolkning))) st_crs(befolkning) <- 25833
if (is.na(st_crs(kommune))) st_crs(kommune) <- 25833

tilfluktsrom <- st_transform(tilfluktsrom, 25833)
befolkning <- st_transform(befolkning, 25833)
kommune <- st_transform(kommune, 25833)


# Municipality variables.
kommune_nr_col <- {
  if ("kommunenummer" %in% names(kommune)) "kommunenummer"
  else if ("kommunenr" %in% names(kommune)) "kommunenr"
  else if ("kommune_nr" %in% names(kommune)) "kommune_nr"
  else if ("knr" %in% names(kommune)) "knr"
  else stop("Could not find municipality number column.")
}

kommune_navn_col <- {
  if ("kommunenavn" %in% names(kommune)) "kommunenavn"
  else if ("kommune_navn" %in% names(kommune)) "kommune_navn"
  else if ("navn" %in% names(kommune)) "navn"
  else if ("navn_bokmal" %in% names(kommune)) "navn_bokmal"
  else stop("Could not find municipality name column.")
}

kommune <- 
  kommune %>%
  mutate(
    kommune_nr = as.character(.data[[kommune_nr_col]]),
    kommune_navn = as.character(.data[[kommune_navn_col]]),
    kommune_navn_ascii = stri_trans_general(kommune_navn, "Latin-ASCII")
  ) %>%
  filter(!startsWith(kommune_nr, "21"))


# Population variable.
befolkning <- 
  befolkning %>%
  mutate(
    pop = parse_number(as.character(pop_tot))
  ) %>%
  filter(!is.na(pop), pop > 0)


tilfluktsrom <- 
  st_join(
  tilfluktsrom,
  kommune %>%
    select(kommune_nr, kommune_navn, kommune_navn_ascii),
  left = FALSE
)


# Make one point per populated grid cell.

befolkning_pts <- st_point_on_surface(befolkning)

befolkning_pts <- 
  st_join(
  befolkning_pts,
  kommune %>%
    select(kommune_nr, kommune_navn, kommune_navn_ascii),
  left = FALSE
)

nearest_idx <- 
  st_nearest_feature(befolkning_pts, tilfluktsrom)

befolkning_pts <- 
  befolkning_pts %>%
  mutate(
    nearest_shelter_id = nearest_idx,
    dist_to_shelter_m = as.numeric(
      st_distance(
        st_geometry(befolkning_pts),
        st_geometry(tilfluktsrom[nearest_idx, ]),
        by_element = TRUE
      )
    ),
    distance_band = factor(
      case_when(
        dist_to_shelter_m <= 500  ~ "0-500 m",
        dist_to_shelter_m <= 1000 ~ "500 m-1 km",
        dist_to_shelter_m <= 2000 ~ "1-2 km",
        TRUE                      ~ ">2 km"
      ),
      levels = c("0-500 m", "500 m-1 km", "1-2 km", ">2 km")
    ),
    within_500m = dist_to_shelter_m <= 500,
    within_1km = dist_to_shelter_m <= 1000,
    within_2km = dist_to_shelter_m <= 2000
  )

national_summary <- 
  befolkning_pts %>%
  st_drop_geometry() %>%
  summarise(
    total_population = sum(pop, na.rm = TRUE),
    population_within_500m = sum(pop[within_500m], na.rm = TRUE),
    population_within_1km = sum(pop[within_1km], na.rm = TRUE),
    population_within_2km = sum(pop[within_2km], na.rm = TRUE),
    share_within_500m = population_within_500m / total_population,
    share_within_1km = population_within_1km / total_population,
    share_within_2km = population_within_2km / total_population,
    weighted_mean_distance_m = sum(pop * dist_to_shelter_m, na.rm = TRUE) / sum(pop, na.rm = TRUE)
  )

national_summary

write_csv(
  national_summary,
  "data/processed/national_shelter_proximity_summary.csv"
)

tilfluktsrom_kommune <- 
  tilfluktsrom %>%
  st_drop_geometry() %>%
  count(
    kommune_nr,
    kommune_navn,
    kommune_navn_ascii,
    name = "n_shelters"
  )

kommune_metrics <- 
  befolkning_pts %>%
  st_drop_geometry() %>%
  group_by(kommune_nr, kommune_navn, kommune_navn_ascii) %>%
  summarise(
    total_pop = sum(pop, na.rm = TRUE),
    pop_within_500m = sum(if_else(within_500m, pop, 0), na.rm = TRUE),
    pop_within_1km = sum(if_else(within_1km, pop, 0), na.rm = TRUE),
    pop_within_2km = sum(if_else(within_2km, pop, 0), na.rm = TRUE),
    weighted_mean_distance_m = sum(pop * dist_to_shelter_m, na.rm = TRUE) / sum(pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_within_500m = 100 * pop_within_500m / total_pop,
    pct_within_1km = 100 * pop_within_1km / total_pop,
    pct_within_2km = 100 * pop_within_2km / total_pop
  ) %>%
  left_join(
    tilfluktsrom_kommune,
    by = c("kommune_nr", "kommune_navn", "kommune_navn_ascii")
  ) %>%
  mutate(
    n_shelters = if_else(is.na(n_shelters), 0L, n_shelters),
    shelters_per_10000 = 10000 * n_shelters / total_pop
  )

kommune_map <- 
  kommune %>%
  left_join(
    kommune_metrics,
    by = c("kommune_nr", "kommune_navn", "kommune_navn_ascii")
  )

distance_distribution <- 
  befolkning_pts %>%
  st_drop_geometry() %>%
  group_by(distance_band) %>%
  summarise(
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share = pop / sum(pop),
    label = scales::percent(share, accuracy = 0.1),
    highlight = if_else(distance_band == ">2 km", "highlight", "other")
  )

top_kommuner_within_1km <- 
  kommune_metrics %>%
  arrange(desc(pct_within_1km)) %>%
  select(
    kommune_navn,
    total_pop,
    n_shelters,
    pct_within_1km,
    pct_within_2km,
    weighted_mean_distance_m
  ) %>%
  slice_head(n = 20)

large_municipalities_low_proximity <- 
  kommune_metrics %>%
  filter(total_pop >= 20000) %>%
  arrange(pct_within_1km) %>%
  select(
    kommune_navn,
    kommune_navn_ascii,
    total_pop,
    n_shelters,
    pct_within_1km,
    pct_within_2km,
    weighted_mean_distance_m
  ) %>%
  slice_head(n = 20)

write_csv(
  distance_distribution,
  "data/processed/national_distance_distribution.csv"
)

write_csv(
  top_kommuner_within_1km,
  "data/processed/top_kommuner_hoy_tilfluktsromnaerhet.csv"
)

write_csv(
  large_municipalities_low_proximity,
  "data/processed/store_kommuner_lav_tilfluktsromnaerhet_top20.csv"
)

write_csv(
  kommune_metrics,
  "data/processed/kommune_shelter_proximity_metrics.csv"
)


# Plot
kommune_plot <- 
  kommune %>%
  st_simplify(dTolerance = 500, preserveTopology = TRUE)

fig1 <- 
  ggplot() +
  geom_sf(
    data = kommune_plot,
    fill = "grey96",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = tilfluktsrom,
    color = "#C44E52",
    size = 1.1,
    alpha = 0.75,
    shape = 16
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Registered public shelters in Norway",
    subtitle = "Locations from the open DSB/GeoNorge dataset"
  ) +
  theme_void(base_family = font_family) +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "grey40",
      margin = margin(b = 16)
    ),
    plot.margin = margin(20, 20, 16, 20),
    plot.background = element_rect(fill = "white", color = NA)
  )

fig1

fig2_ymax <- ceiling(max(distance_distribution$share, na.rm = TRUE) * 1.18 * 10) / 10

fig2 <- 
  ggplot(
  distance_distribution,
  aes(x = distance_band, y = share, fill = highlight)
) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    vjust = -0.45,
    size = 4.4,
    fontface = "bold",
    family = font_family
  ) +
  scale_fill_manual(
    values = c("other" = "grey72", "highlight" = "#C44E52")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::pretty_breaks(n = 6),
    limits = c(0, fig2_ymax),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "How close do people live to a registered public shelter?",
    subtitle = "Population share by distance to the nearest registered public shelter",
    x = NULL,
    y = "Share of population"
  ) +
  theme_minimal(base_family = font_family, base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 11.5,
      color = "grey40",
      margin = margin(b = 14)
    ),
    axis.title.y = element_text(
      size = 12,
      color = "grey30",
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(
      size = 11.5,
      face = "bold",
      color = "grey15"
    ),
    axis.text.y = element_text(
      size = 11,
      color = "grey30"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey88"),
    plot.margin = margin(14, 16, 12, 14),
    plot.background = element_rect(fill = "white", color = NA)
  )

fig2


fig3 <- 
  ggplot(kommune_map) +
  geom_sf(
    aes(fill = pct_within_1km),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey88",
    labels = function(x) paste0(round(x), "%"),
    guide = guide_colorbar(
      barwidth = 0.8,
      barheight = 10,
      ticks = FALSE,
      title.position = "top"
    )
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Registered shelter proximity varies across municipalities",
    subtitle = "Share of each municipality's population within 1 km of the nearest registered public shelter",
    fill = "Within\n1 km"
  ) +
  theme_void(base_family = font_family) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 10.5,
      hjust = 0.5,
      color = "grey40",
      margin = margin(b = 14)
    ),
    legend.position = c(0.88, 0.55),
    legend.title = element_text(
      size = 9.5,
      face = "bold",
      color = "grey20"
    ),
    legend.text = element_text(
      size = 9,
      color = "grey30"
    ),
    plot.margin = margin(16, 16, 14, 16),
    plot.background = element_rect(fill = "white", color = NA)
  )

fig3


policy_municipalities <- 
  kommune_metrics %>%
  filter(total_pop >= 20000) %>%
  arrange(pct_within_1km) %>%
  slice_head(n = 15) %>%
  mutate(
    municipality_label = reorder(kommune_navn, -pct_within_1km),
    pct_label = paste0(round(pct_within_1km, 1), "%"),
    pop_label = scales::comma(total_pop)
  )

fig4_xmax <- max(policy_municipalities$pct_within_1km / 100, na.rm = TRUE) * 1.35

fig4 <- 
  ggplot(
  policy_municipalities,
  aes(x = pct_within_1km / 100, y = municipality_label)
) +
  geom_segment(
    aes(x = 0, xend = pct_within_1km / 100, yend = municipality_label),
    color = "grey82",
    linewidth = 0.7
  ) +
  geom_point(
    aes(size = total_pop),
    color = "#C44E52",
    alpha = 0.88,
    shape = 16
  ) +
  geom_text(
    aes(label = pct_label),
    hjust = -0.5,
    size = 3.6,
    fontface = "bold",
    family = font_family,
    color = "grey20"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, fig4_xmax),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_size_continuous(
    labels = scales::comma_format(),
    range = c(2.5, 9),
    guide = guide_legend(
      title = "Population",
      title.position = "top",
      override.aes = list(color = "#C44E52", alpha = 0.7)
    )
  ) +
  labs(
    title = "Large municipalities with low registered shelter proximity",
    subtitle = "Municipalities with at least 20,000 residents, ranked by share of population within 1 km",
    x = "Share of population within 1 km",
    y = NULL,
    size = "Population"
  ) +
  theme_minimal(base_family = font_family, base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 11.2,
      color = "grey40",
      margin = margin(b = 14)
    ),
    axis.title.x = element_text(
      size = 11.5,
      color = "grey30",
      margin = margin(t = 8)
    ),
    axis.text.y = element_text(
      size = 11,
      face = "bold",
      color = "grey15"
    ),
    axis.text.x = element_text(
      size = 10.5,
      color = "grey30"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
    legend.position = "right",
    legend.title = element_text(
      size = 9.5,
      face = "bold",
      color = "grey20"
    ),
    legend.text = element_text(
      size = 9,
      color = "grey30"
    ),
    plot.margin = margin(14, 18, 12, 14),
    plot.background = element_rect(fill = "white", color = NA)
  )

fig4
