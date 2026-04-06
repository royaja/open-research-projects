# make figures from the public paragraph-level release

library(tidyverse)
library(ragg)

dir.create("figures", recursive = TRUE, showWarnings = FALSE)

data_public <-
  read_csv(
    "data/coded_paragraphs.csv",
    show_col_types = FALSE
  )

data_public <-
  data_public %>%
  mutate(
    primary_topic_en = str_squish(primary_topic_en),
    primary_function_en = str_squish(primary_function_en),
    category = str_squish(category)
  ) %>%
  filter(
    !is.na(primary_function_en),
    !is.na(primary_topic_en)
  ) %>%
  filter(
    primary_function_en != "Other / administrative / not codable",
    primary_topic_en != "Other / administrative / not codable"
  ) %>%
  mutate(
    category = factor(
      category,
      levels = c("Press release", "Speech")
    ),
    primary_function_en = factor(
      primary_function_en,
      levels = c(
        "Economic assessment / diagnosis",
        "Policy explanation / justification",
        "Outlook / forward guidance",
        "Risk / uncertainty",
        "Institutional accountability / mandate"
      )
    ),
    primary_topic_en = factor(
      primary_topic_en,
      levels = c(
        "Inflation and prices",
        "Interest rates and monetary stance",
        "Economic activity and labor market",
        "Exchange rate and external conditions",
        "Financial stability and banking"
      )
    )
  )

function_total <-
  data_public %>%
  count(primary_function_en) %>%
  mutate(
    pct = n / sum(n) * 100
  ) %>%
  arrange(pct)

topic_total <-
  data_public %>%
  count(primary_topic_en) %>%
  mutate(
    pct = n / sum(n) * 100
  ) %>%
  arrange(pct)

function_topic <-
  data_public %>%
  count(primary_function_en, primary_topic_en) %>%
  group_by(primary_function_en) %>%
  mutate(
    pct = n / sum(n) * 100
  ) %>%
  ungroup()

plot_function_total <-
  ggplot(
    function_total,
    aes(
      x = pct,
      y = forcats::fct_reorder(primary_function_en, pct),
      fill = primary_function_en
    )
  ) +
  geom_col(
    width = 0.72,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = paste0(round(pct, 1), " %")),
    hjust = -0.1,
    size = 3.7,
    family = "sans"
  ) +
  scale_fill_manual(
    values = c(
      "Economic assessment / diagnosis" = "#B8CEC2",
      "Policy explanation / justification" = "#D9C5A8",
      "Outlook / forward guidance" = "#BCCCE0",
      "Risk / uncertainty" = "#E6CACA",
      "Institutional accountability / mandate" = "#D3CDE6"
    )
  ) +
  coord_cartesian(xlim = c(0, max(function_total$pct) + 8)) +
  labs(
    x = "Percent",
    y = NULL,
    title = "Communicative function overall"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "grey20"),
    plot.title = element_text(size = 13, face = "bold")
  )

plot_topic_total <-
  ggplot(
    topic_total,
    aes(
      x = pct,
      y = forcats::fct_reorder(primary_topic_en, pct),
      fill = primary_topic_en
    )
  ) +
  geom_col(
    width = 0.72,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = paste0(round(pct, 1), " %")),
    hjust = -0.1,
    size = 3.7,
    family = "sans"
  ) +
  scale_fill_manual(
    values = c(
      "Inflation and prices" = "#D9C5A8",
      "Interest rates and monetary stance" = "#B8CEC2",
      "Economic activity and labor market" = "#BCCCE0",
      "Exchange rate and external conditions" = "#E6CACA",
      "Financial stability and banking" = "#D3CDE6"
    )
  ) +
  coord_cartesian(xlim = c(0, max(topic_total$pct) + 8)) +
  labs(
    x = "Percent",
    y = NULL,
    title = "Main topic overall"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "grey20"),
    plot.title = element_text(size = 13, face = "bold")
  )

plot_function_topic <-
  ggplot(
    function_topic,
    aes(
      x = primary_topic_en,
      y = primary_function_en,
      fill = pct
    )
  ) +
  geom_tile(
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = round(pct, 1)),
    size = 3.4,
    colour = "grey15",
    family = "sans"
  ) +
  scale_fill_gradient(
    low = "#F7F7F7",
    high = "#9EB6C8"
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Percent",
    title = "Link between function and topic"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(
      size = 10,
      colour = "grey20",
      angle = 30,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10, colour = "grey20"),
    plot.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  "figures/plot_function_total.png",
  plot_function_total,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white",
  device = ragg::agg_png
)

ggsave(
  "figures/plot_topic_total.png",
  plot_topic_total,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white",
  device = ragg::agg_png
)

ggsave(
  "figures/plot_function_topic.png",
  plot_function_topic,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white",
  device = ragg::agg_png
)
