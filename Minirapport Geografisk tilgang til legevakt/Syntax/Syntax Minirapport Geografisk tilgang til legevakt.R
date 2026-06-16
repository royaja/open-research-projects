
# Geografisk tilgang til legevakt som lokal akuttberedskap
# This project explores geographic access to legevakt in Vestfold and Buskerud.
# The analysis uses processed GeoPackage files for faster loading.
# Code developed by R. A. Jacobsen

# Load libraries
library(tidyverse)
library(sf)
library(janitor)
library(ggrepel)
library(ragg)
library(scales)
library(patchwork)


# Project paths
project_dir <- 
  "C:/Users/roja006/Documents/Legevakt prosjekt"

data_dir <- 
  file.path(project_dir, "data")

processed_dir <- 
  file.path(project_dir, "data_processed")

output_dir <- 
  file.path(project_dir, "output")

figure_dir <- 
  file.path(output_dir, "figures")

table_dir <- 
  file.path(output_dir, "tables")

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Settings
crs_analysis <- 
  25833

crs_wgs84 <- 
  4326

pop_col <- 
  "pop_tot"

font_family <- 
  "sans"

source_caption <- 
  paste0(
    "Data: SSB befolkningsrutenett 2024, Kartverket kommunegrenser, NORCE legevaktregister 2025.\n",
    "Legevaktkoordinater er manuelt innhentet fra Google Maps. Avstand er m\u00e5lt i luftlinje."
  )

colors_dist <- 
  c(
    "0-10 km" = "#27AE60",
    "10-20 km" = "#F59E0B",
    "20-40 km" = "#D4520A",
    "Over 40 km" = "#B91C1C"
  )


# Load data
kommuner_region <- 
  st_read(
    file.path(processed_dir, "kommuner_buskerud_vestfold.gpkg"),
    quiet = TRUE
  ) |> 
  st_transform(
    crs_analysis
  )

population_grid_region <- 
  st_read(
    file.path(processed_dir, "population_grid_buskerud_vestfold.gpkg"),
    quiet = TRUE
  ) |> 
  st_transform(
    crs_analysis
  )


# Legevakt data
legevakt_region <- 
  tribble(
    ~fylke, ~legevakt, ~lat, ~lon,
    "Vestfold", "Horten kommunale legevakt", 59.414009676470236, 10.47040648177181,
    "Vestfold", "Larvik legevakt", 59.052873716351016, 10.035766583594988,
    "Vestfold", "Sandefjord legevakt", 59.133823442864006, 10.213369241272407,
    "Vestfold", "T\u00f8nsbergregionen legevakt", 59.284955374741806, 10.397079692661878,
    "Buskerud", "Drammen legevakt", 59.74769744354611, 10.189368824125316,
    "Buskerud", "Gol og Hemsedal legevakt", 60.70681802528951, 8.949101620649524,
    "Buskerud", "Hol kommunale legevakt", 60.54442207012073, 8.206552363485157,
    "Buskerud", "Kongsberg interkommunale legevakt", 59.67009400291621, 9.65567527014809,
    "Buskerud", "Nore og Uvdal legevakt", 60.267079401391385, 8.94682463950717,
    "Buskerud", "Ringerike interkommunale legevakt", 60.14750046831514, 10.257647895319588,
    "Buskerud", "\u00d8vre Hallingdal legevakt / Hallingdal nattlegevakt", 60.63029942775166, 8.566517519948887
  ) |> 
  arrange(
    fylke,
    legevakt
  )


# Table 1
legevakt_table <- 
  legevakt_region |> 
  select(
    fylke,
    legevakt
  )

write_csv(
  legevakt_table,
  file.path(table_dir, "table_1_legevakter.csv")
)

write_csv(
  legevakt_region,
  file.path(data_dir, "legevakt_region_coordinates.csv")
)


legevakt_sf <- 
  legevakt_region |> 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = crs_wgs84,
    remove = FALSE
  ) |> 
  st_transform(
    crs_analysis
  )

st_write(
  legevakt_sf,
  file.path(processed_dir, "legevakt_points.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)


# Population points
if (!"grid_id" %in% names(population_grid_region)) {
  population_grid_region <- 
    population_grid_region |> 
    mutate(
      grid_id = row_number()
    )
}

population_points_region <- 
  population_grid_region |> 
  mutate(
    population = as.numeric(.data[[pop_col]])
  ) |> 
  filter(
    !is.na(population),
    population > 0
  ) |> 
  st_point_on_surface()


# Add municipality information
nearest_kommune <- 
  st_nearest_feature(
    population_points_region,
    kommuner_region
  )

population_points_region <- 
  population_points_region |> 
  mutate(
    kommunenummer = kommuner_region$kommunenummer[nearest_kommune],
    kommunenavn = kommuner_region$kommunenavn[nearest_kommune],
    fylke = kommuner_region$fylke[nearest_kommune]
  )


# Distance to nearest legevakt
nearest_legevakt_index <- 
  st_nearest_feature(
    population_points_region,
    legevakt_sf
  )

distance_to_nearest <- 
  st_distance(
    population_points_region,
    legevakt_sf[nearest_legevakt_index, ],
    by_element = TRUE
  )

population_access <- 
  population_points_region |> 
  mutate(
    nearest_legevakt = legevakt_sf$legevakt[nearest_legevakt_index],
    distance_to_legevakt_m = as.numeric(distance_to_nearest),
    distance_to_legevakt_km = distance_to_legevakt_m / 1000,
    distance_category = case_when(
      distance_to_legevakt_km <= 10 ~ "0-10 km",
      distance_to_legevakt_km <= 20 ~ "10-20 km",
      distance_to_legevakt_km <= 40 ~ "20-40 km",
      distance_to_legevakt_km > 40 ~ "Over 40 km",
      TRUE ~ NA_character_
    ),
    distance_category = factor(
      distance_category,
      levels = c(
        "0-10 km",
        "10-20 km",
        "20-40 km",
        "Over 40 km"
      )
    )
  )


# Table 2
access_summary_distance <- 
  population_access |> 
  st_drop_geometry() |> 
  group_by(
    distance_category
  ) |> 
  summarise(
    population = sum(
      population,
      na.rm = TRUE
    ),
    grid_cells = n(),
    .groups = "drop"
  ) |> 
  mutate(
    share = population / sum(population),
    cumulative_share = cumsum(share),
    share_percent = round(
      share * 100,
      1
    ),
    cumulative_share_percent = round(
      cumulative_share * 100,
      1
    )
  )

write_csv(
  access_summary_distance,
  file.path(table_dir, "table_2_population_by_distance.csv")
)


# Municipality-level distance summary
municipality_summary_distance <- 
  population_access |> 
  st_drop_geometry() |> 
  group_by(
    fylke,
    kommunenummer,
    kommunenavn
  ) |> 
  summarise(
    total_population = sum(
      population,
      na.rm = TRUE
    ),
    mean_distance_km = weighted.mean(
      x = distance_to_legevakt_km,
      w = population,
      na.rm = TRUE
    ),
    median_distance_km = median(
      distance_to_legevakt_km,
      na.rm = TRUE
    ),
    max_distance_km = max(
      distance_to_legevakt_km,
      na.rm = TRUE
    ),
    population_over_20km = sum(
      population[distance_to_legevakt_km > 20],
      na.rm = TRUE
    ),
    population_over_40km = sum(
      population[distance_to_legevakt_km > 40],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |> 
  mutate(
    share_over_20km = round(
      100 * population_over_20km / total_population,
      1
    ),
    share_over_40km = round(
      100 * population_over_40km / total_population,
      1
    ),
    mean_distance_km = round(
      mean_distance_km,
      1
    ),
    median_distance_km = round(
      median_distance_km,
      1
    ),
    max_distance_km = round(
      max_distance_km,
      1
    )
  ) |> 
  arrange(
    desc(mean_distance_km)
  )

write_csv(
  municipality_summary_distance,
  file.path(table_dir, "municipality_summary_distance.csv")
)


# Furthest populated grid cells
population_access_wgs84 <- 
  population_access |> 
  st_transform(
    crs_wgs84
  )

population_coordinates <- 
  st_coordinates(
    population_access_wgs84
  ) |> 
  as_tibble() |> 
  rename(
    lon = X,
    lat = Y
  )

furthest_points <- 
  population_access_wgs84 |> 
  st_drop_geometry() |> 
  bind_cols(
    population_coordinates
  ) |> 
  arrange(
    desc(distance_to_legevakt_km)
  ) |> 
  select(
    fylke,
    kommunenummer,
    kommunenavn,
    grid_id,
    population,
    nearest_legevakt,
    distance_to_legevakt_km,
    lon,
    lat
  ) |> 
  mutate(
    distance_to_legevakt_km = round(
      distance_to_legevakt_km,
      1
    ),
    lon = round(
      lon,
      5
    ),
    lat = round(
      lat,
      5
    )
  ) |> 
  slice_head(
    n = 30
  )

write_csv(
  furthest_points,
  file.path(table_dir, "furthest_points_distance.csv")
)


# Save
population_access_clean <- 
  population_access |> 
  select(
    grid_id,
    population,
    kommunenummer,
    kommunenavn,
    fylke,
    nearest_legevakt,
    distance_to_legevakt_m,
    distance_to_legevakt_km,
    distance_category
  )

st_write(
  population_access_clean,
  file.path(processed_dir, "population_access_distance.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)


# Figure 1
pct_10km <- 
  population_access |> 
  st_drop_geometry() |> 
  summarise(
    pct = round(
      100 * sum(
        population[distance_to_legevakt_km <= 10],
        na.rm = TRUE
      ) / sum(
        population,
        na.rm = TRUE
      )
    )
  ) |> 
  pull(
    pct
  )

pct_20km <- 
  population_access |> 
  st_drop_geometry() |> 
  summarise(
    pct = round(
      100 * sum(
        population[distance_to_legevakt_km <= 20],
        na.rm = TRUE
      ) / sum(
        population,
        na.rm = TRUE
      )
    )
  ) |> 
  pull(
    pct
  )

distance_distribution_data <- 
  population_access |> 
  st_drop_geometry() |> 
  mutate(
    distance_bin_km = floor(distance_to_legevakt_km / 2) * 2,
    color_cat = case_when(
      distance_bin_km < 10 ~ "0-10 km",
      distance_bin_km < 20 ~ "10-20 km",
      distance_bin_km < 40 ~ "20-40 km",
      TRUE ~ "Over 40 km"
    ),
    color_cat = factor(
      color_cat,
      levels = c(
        "0-10 km",
        "10-20 km",
        "20-40 km",
        "Over 40 km"
      )
    )
  ) |> 
  group_by(
    distance_bin_km,
    color_cat
  ) |> 
  summarise(
    population = sum(
      population,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

max_population_histogram <- 
  max(
    distance_distribution_data$population,
    na.rm = TRUE
  )

figure_1 <- 
  ggplot(
    distance_distribution_data,
    aes(
      x = distance_bin_km,
      y = population,
      fill = color_cat
    )
  ) +
  geom_col(
    width = 1.8,
    color = "white",
    linewidth = 0.25
  ) +
  geom_vline(
    xintercept = 20,
    linetype = "dashed",
    linewidth = 0.4,
    color = "#9CA3AF"
  ) +
  geom_vline(
    xintercept = 40,
    linetype = "dashed",
    linewidth = 0.4,
    color = "#9CA3AF"
  ) +
  annotate(
    "text",
    x = 20.6,
    y = max_population_histogram * 0.90,
    label = "20 km",
    hjust = 0,
    size = 3.2,
    color = "#6B7280"
  ) +
  annotate(
    "text",
    x = 40.6,
    y = max_population_histogram * 0.90,
    label = "40 km",
    hjust = 0,
    size = 3.2,
    color = "#6B7280"
  ) +
  annotate(
    "label",
    x = 10.4,
    y = max_population_histogram * 0.86,
    label = paste0(
      pct_10km,
      " %\nbor innenfor 10 km"
    ),
    color = "#166534",
    fill = "white",
    label.size = 0,
    fontface = "bold",
    size = 3.7,
    hjust = 0,
    lineheight = 1.15
  ) +
  annotate(
    "label",
    x = 13.8,
    y = max_population_histogram * 0.40,
    label = paste0(
      pct_20km,
      " % bor\ninnenfor 20 km"
    ),
    color = "#111827",
    fill = "white",
    label.size = 0,
    size = 3.4,
    hjust = 0.5,
    lineheight = 1.15
  ) +
  scale_fill_manual(
    values = colors_dist,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      50,
      by = 10
    ),
    labels = function(x) paste0(
      x,
      " km"
    ),
    expand = expansion(
      mult = c(0.01, 0.06)
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = " "
    ),
    expand = expansion(
      mult = c(0, 0.12)
    )
  ) +
  labs(
    title = "Fordelingen av befolkningens avstand til n\u00e6rmeste legevakt",
    subtitle = "Antall innbyggere, populasjonsvektet, per 2 km-intervall i Vestfold og Buskerud",
    x = "Luftlinjeavstand til n\u00e6rmeste legevakt",
    y = "Antall innbyggere",
    caption = source_caption
  ) +
  theme_minimal(
    base_family = font_family,
    base_size = 10
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#E5E7EB",
      linewidth = 0.3
    ),
    axis.text = element_text(
      color = "#374151"
    ),
    axis.title = element_text(
      color = "#374151"
    ),
    plot.title = element_text(
      color = "#111827",
      face = "bold",
      size = 16,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      color = "#4B5563",
      size = 10,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      color = "#6B7280",
      size = 7,
      lineheight = 1.15,
      hjust = 0,
      margin = margin(t = 10)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = grid::unit(
      0.45,
      "cm"
    ),
    legend.text = element_text(
      size = 9,
      color = "#374151"
    ),
    plot.margin = margin(
      18,
      24,
      14,
      18
    )
  )

ggsave(
  filename = file.path(figure_dir, "figure_1_distance_distribution.png"),
  plot = figure_1,
  width = 8.5,
  height = 5.5,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)


# Figure 2
municipality_map_data <- 
  kommuner_region |> 
  mutate(
    kommunenummer = as.character(
      kommunenummer
    )
  ) |> 
  left_join(
    municipality_summary_distance |> 
      mutate(
        kommunenummer = as.character(
          kommunenummer
        )
      ) |> 
      select(
        kommunenummer,
        total_population,
        mean_distance_km,
        median_distance_km,
        max_distance_km,
        share_over_20km,
        share_over_40km
      ),
    by = "kommunenummer"
  )

municipality_label_data <- 
  municipality_map_data |> 
  filter(
    !is.na(mean_distance_km)
  ) |> 
  slice_max(
    mean_distance_km,
    n = 8,
    with_ties = FALSE
  ) |> 
  st_point_on_surface()

municipality_label_coordinates <- 
  st_coordinates(
    municipality_label_data
  ) |> 
  as_tibble() |> 
  rename(
    label_x = X,
    label_y = Y
  )

municipality_label_data <- 
  municipality_label_data |> 
  st_drop_geometry() |> 
  bind_cols(
    municipality_label_coordinates
  ) |> 
  mutate(
    label = paste0(
      kommunenavn,
      "\n",
      mean_distance_km,
      " km"
    )
  )

figure_2 <- 
  ggplot() +
  geom_sf(
    data = municipality_map_data,
    aes(
      fill = mean_distance_km
    ),
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = legevakt_sf,
    shape = 21,
    fill = "white",
    color = "black",
    size = 2,
    stroke = 0.8
  ) +
  geom_label_repel(
    data = municipality_label_data,
    aes(
      x = label_x,
      y = label_y,
      label = label
    ),
    inherit.aes = FALSE,
    size = 2.8,
    family = font_family,
    fontface = "bold",
    fill = "white",
    label.size = 0.25,
    label.r = grid::unit(
      0.10,
      "lines"
    ),
    label.padding = grid::unit(
      0.16,
      "lines"
    ),
    min.segment.length = 0,
    segment.color = "#6B7280",
    segment.linewidth = 0.25,
    force = 3,
    seed = 42,
    max.overlaps = Inf
  ) +
  scale_fill_gradient(
    low = "#D6E8F5",
    high = "#1A3A6B",
    na.value = "#F3F4F6",
    name = "Gjennomsnittlig\navstand (km)",
    guide = guide_colorbar(
      barwidth = 0.7,
      barheight = 8,
      title.position = "top"
    )
  ) +
  coord_sf(
    crs = st_crs(crs_analysis),
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = "Gjennomsnittlig avstand til n\u00e6rmeste legevakt",
    subtitle = "Befolkningsvektet luftlinjeavstand per kommune.\nDe 8 kommunene med h\u00f8yest gjennomsnitt er navngitt.",
    caption = source_caption
  ) +
  theme_void(
    base_family = font_family,
    base_size = 10
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.title = element_text(
      color = "#111827",
      face = "bold",
      size = 16,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      color = "#4B5563",
      size = 10,
      lineheight = 1.10,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      color = "#6B7280",
      size = 7,
      lineheight = 1.15,
      hjust = 0,
      margin = margin(t = 10)
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 9,
      color = "#374151"
    ),
    legend.text = element_text(
      size = 8,
      color = "#374151"
    ),
    plot.margin = margin(
      18,
      18,
      14,
      18
    )
  )

ggsave(
  filename = file.path(figure_dir, "figure_2_municipality_mean_distance_map.png"),
  plot = figure_2,
  width = 8.5,
  height = 9,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)


# Figure 3
theme_bar <- 
  theme_minimal(
    base_family = font_family,
    base_size = 10
  ) %+replace%
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "#E5E7EB",
      linewidth = 0.3
    ),
    axis.text = element_text(
      color = "#374151",
      size = 9
    ),
    axis.title = element_text(
      color = "#374151",
      size = 9
    ),
    plot.subtitle = element_text(
      color = "#6B7280",
      size = 9,
      margin = margin(b = 10)
    ),
    plot.margin = margin(
      10,
      28,
      10,
      10
    )
  )

top15_20km <- 
  municipality_summary_distance |> 
  arrange(
    desc(share_over_20km)
  ) |> 
  slice_head(
    n = 15
  ) |> 
  mutate(
    kommunenavn = fct_reorder(
      kommunenavn,
      share_over_20km
    )
  )

panel_20km <- 
  ggplot(
    top15_20km,
    aes(
      x = share_over_20km / 100,
      y = kommunenavn
    )
  ) +
  geom_col(
    fill = "#D4520A",
    width = 0.68
  ) +
  geom_text(
    aes(
      label = paste0(
        share_over_20km,
        "%"
      )
    ),
    hjust = -0.15,
    size = 3.2,
    color = "#374151"
  ) +
  scale_x_continuous(
    breaks = c(
      0,
      0.30,
      0.60,
      0.90
    ),
    labels = percent_format(
      accuracy = 1
    ),
    limits = c(
      0,
      1.22
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  labs(
    title = "Andel over 20 km",
    subtitle = "Topp 15 kommuner",
    x = "Andel av befolkningen",
    y = NULL
  ) +
  theme_bar +
  theme(
    plot.title = element_text(
      color = "#D4520A",
      face = "bold",
      size = 14,
      margin = margin(b = 2)
    )
  )

over40_municipalities <- 
  municipality_summary_distance |> 
  filter(
    share_over_40km > 0
  ) |> 
  arrange(
    desc(share_over_40km)
  ) |> 
  mutate(
    kommunenavn = fct_reorder(
      kommunenavn,
      share_over_40km
    )
  )

panel_40km <- 
  ggplot(
    over40_municipalities,
    aes(
      x = share_over_40km / 100,
      y = kommunenavn
    )
  ) +
  geom_col(
    fill = "#B91C1C",
    width = 0.45
  ) +
  geom_text(
    aes(
      label = paste0(
        share_over_40km,
        "%"
      )
    ),
    hjust = -0.15,
    size = 3.2,
    color = "#374151"
  ) +
  scale_x_continuous(
    breaks = c(
      0,
      0.025,
      0.05,
      0.075
    ),
    labels = percent_format(
      accuracy = 0.1
    ),
    limits = c(
      0,
      0.10
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  labs(
    title = "Andel over 40 km",
    subtitle = "Kun kommuner med befolkning over 40 km",
    x = "Andel av befolkningen",
    y = NULL
  ) +
  theme_bar +
  theme(
    plot.title = element_text(
      color = "#B91C1C",
      face = "bold",
      size = 14,
      margin = margin(b = 2)
    )
  )

figure_3 <- 
  (panel_20km + panel_40km) +
  plot_layout(
    widths = c(
      2,
      1
    )
  ) +
  plot_annotation(
    title = "Kommuner med st\u00f8rst andel av befolkningen langt fra n\u00e6rmeste legevakt",
    subtitle = "Luftlinjeavstand i Vestfold og Buskerud. Hvert panel har egen x-akse.",
    caption = source_caption,
    theme = theme(
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.title = element_text(
        family = font_family,
        face = "bold",
        color = "#111827",
        size = 16,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        family = font_family,
        color = "#4B5563",
        size = 10,
        margin = margin(b = 14)
      ),
      plot.caption = element_text(
        family = font_family,
        color = "#6B7280",
        size = 7,
        lineheight = 1.15,
        hjust = 0,
        margin = margin(t = 10)
      )
    )
  )

ggsave(
  filename = file.path(figure_dir, "figure_3_municipality_share_over_20_40km.png"),
  plot = figure_3,
  width = 12,
  height = 8,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)


# Figure 4
long_distance_points <- 
  population_access |> 
  filter(
    distance_to_legevakt_km > 20
  ) |> 
  mutate(
    long_distance_category = case_when(
      distance_to_legevakt_km <= 40 ~ "20-40 km",
      TRUE ~ "Over 40 km"
    ),
    long_distance_category = factor(
      long_distance_category,
      levels = c(
        "20-40 km",
        "Over 40 km"
      )
    )
  )


# Legevakt labels

legevakt_label_coordinates <- 
  st_coordinates(
    legevakt_sf
  ) |> 
  as_tibble() |> 
  rename(
    label_x = X,
    label_y = Y
  )

legevakt_label_data <- 
  legevakt_sf |> 
  st_drop_geometry() |> 
  bind_cols(
    legevakt_label_coordinates
  ) |> 
  mutate(
    legevakt_label = case_when(
      str_detect(legevakt, "Hallingdal") ~ "\u00d8vre Hallingdal legevakt/\nHallingdal nattlegevakt",
      str_detect(legevakt, "T\u00f8nsberg") ~ "T\u00f8nsbergregionen legevakt",
      TRUE ~ legevakt
    )
  )


figure_4 <- 
  ggplot() +
  geom_sf(
    data = kommuner_region,
    fill = "#F5F5F5",
    color = "#D1D5DB",
    linewidth = 0.3
  ) +
  geom_sf(
    data = long_distance_points,
    aes(
      color = long_distance_category
    ),
    size = 0.45,
    alpha = 0.82,
    shape = 16
  ) +
  geom_sf(
    data = legevakt_sf,
    shape = 21,
    fill = "white",
    color = "black",
    size = 2.0,
    stroke = 0.80
  ) +
  geom_label_repel(
    data = legevakt_label_data,
    aes(
      x = label_x,
      y = label_y,
      label = legevakt_label
    ),
    inherit.aes = FALSE,
    family = font_family,
    size = 2.35,
    color = "#111827",
    fill = "white",
    label.size = 0.20,
    label.r = grid::unit(
      0.08,
      "lines"
    ),
    label.padding = grid::unit(
      0.12,
      "lines"
    ),
    box.padding = 0.25,
    point.padding = 0.15,
    min.segment.length = 0,
    segment.color = "#6B7280",
    segment.linewidth = 0.25,
    force = 2.5,
    seed = 42,
    max.overlaps = Inf
  ) +
  scale_color_manual(
    values = c(
      "20-40 km" = "#F59E0B",
      "Over 40 km" = "#C0392B"
    ),
    name = "Luftlinjeavstand til n\u00e6rmeste legevakt"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = 4,
        alpha = 1
      ),
      title.position = "top"
    )
  ) +
  coord_sf(
    crs = st_crs(crs_analysis),
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = "Befolkede ruter med lang avstand til n\u00e6rmeste legevakt",
    subtitle = "Ruter med avstand p\u00e5 mer enn 20 og 40 km i Vestfold og Buskerud",
    caption = source_caption
  ) +
  theme_void(
    base_family = font_family,
    base_size = 10
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.title = element_text(
      color = "#111827",
      face = "bold",
      size = 16,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      color = "#4B5563",
      size = 10,
      lineheight = 1.10,
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      color = "#6B7280",
      size = 7,
      lineheight = 1.15,
      hjust = 0,
      margin = margin(t = 10)
    ),
    legend.position = "bottom",
    legend.title = element_text(
      size = 9,
      color = "#374151",
      face = "bold"
    ),
    legend.text = element_text(
      size = 9,
      color = "#374151"
    ),
    plot.margin = margin(
      18,
      18,
      14,
      18
    )
  )

ggsave(
  filename = file.path(figure_dir, "figure_4_long_distance_population_grid_map.png"),
  plot = figure_4,
  width = 8.5,
  height = 10,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)


