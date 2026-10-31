

# When Politicians Say "We": What Their Facebook Posts Reveal
# This project examines Facebook posts by Norwegian party leaders from 1 July to 19 October 2026.
# It explores pronoun use, communication strategies, vocabulary similarity, and political attention.
# The analyses are descriptive and do not establish causal relationships.

# Load libraries

library(tidyverse)
library(readxl)
library(janitor)
library(tidytext)
library(stopwords)
library(widyr)
library(ggfittext)
library(MetBrewer)


# Load data

facebook <-
  read_xlsx("C:/Users/roja006/OneDrive - Kristiania/[01] Kristiania/[30] Dissemination Articles/2026-10-23 Substack Politicians Posts/Norwegian Party Leaders Facebook Posts.xlsx") |>
  clean_names()


# Wrangle

facebook <-
  facebook |>
  distinct(id, .keep_all = TRUE) |>
  mutate(
    politician = str_squish(post_owner_name),
    text = replace_na(as.character(text), ""),
    created_at = as.Date(str_sub(as.character(creation_time), 1, 10)),
    shares = as.numeric(statistics_share_count),
    across(
      c(
        criticism,
        negative_emotions,
        dramatization,
        jw_people_centrism,
        jw_anti_elitism,
        asg_issue_frame,
        symbolic_presentation,
        input_seeking,
        mobilisering
      ),
      ~ as.integer(as.character(.x))
    )
  ) |>
  filter(
    created_at >= as.Date("2026-07-01"),
    created_at <= as.Date("2026-10-19"),
    !is.na(politician),
    politician != "Erna Solberg",
    text != ""
  ) |>
  add_count(politician, name = "n_posts") |>
  filter(n_posts >= 20)

words <-
  facebook |>
  select(id, politician, text) |>
  unnest_tokens(word, text) |>
  filter(str_detect(word, "^[[:alpha:]]+$"))


# Figure 1: We or Me?

we_me_df <-
  words |>
  mutate(
    pronoun = case_when(
      word %in% c("vi", "oss") ~ "Collective",
      word %in% c("jeg", "meg", "min", "mitt", "mine") ~ "Individual",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(pronoun)) |>
  count(politician, pronoun, name = "n") |>
  group_by(politician) |>
  mutate(
    share = n / sum(n),
    n_pronouns = sum(n),
    collective_share = sum(n[pronoun == "Collective"]) / sum(n)
  ) |>
  ungroup() |>
  mutate(
    politician = fct_reorder(politician, collective_share),
    pronoun = factor(
      pronoun,
      levels = c("Collective", "Individual")
    )
  )

we_me_plot <-
  we_me_df |>
  ggplot(
    aes(
      x = politician,
      y = share,
      fill = pronoun
    )
  ) +
  geom_col(
    width = 0.7,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    aes(
      label = scales::percent(share, accuracy = 1)
    ),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    colour = "white",
    fontface = "bold",
    size = 3.8
  ) +
  geom_text(
    data = we_me_df |>
      distinct(politician, n_pronouns),
    aes(
      x = politician,
      y = 1.03,
      label = paste0("n = ", n_pronouns)
    ),
    inherit.aes = FALSE,
    hjust = 0,
    colour = "grey40",
    size = 3.4
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c(
      "Collective" = "#356D8D",
      "Individual" = "#965439"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1.15),
    breaks = seq(0, 1, 0.25),
    labels = scales::label_percent(),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Share of selected first-person pronouns",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(colour = "#202124"),
    plot.margin = margin(12, 20, 12, 12)
  )

we_me_plot


# Figure 2: When Politicians Say We or I

voice_counts <-
  words |>
  filter(
    word %in% c(
      "vi", "oss", "v??r", "v??rt", "v??re",
      "jeg", "meg", "min", "mitt", "mine"
    )
  ) |>
  mutate(
    voice = if_else(
      word %in% c("vi", "oss", "v??r", "v??rt", "v??re"),
      "Collective",
      "Individual"
    )
  ) |>
  count(id, voice) |>
  pivot_wider(
    names_from = voice,
    values_from = n,
    values_fill = 0
  )

voice_posts <-
  facebook |>
  left_join(voice_counts, by = "id") |>
  mutate(
    Collective = replace_na(Collective, 0L),
    Individual = replace_na(Individual, 0L),
    voice = case_when(
      Collective > Individual ~ "Collective",
      Individual > Collective ~ "Individual",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(voice))

voice_message_df <-
  voice_posts |>
  select(
    voice,
    criticism,
    negative_emotions,
    dramatization,
    jw_people_centrism,
    jw_anti_elitism,
    symbolic_presentation
  ) |>
  pivot_longer(
    cols = -voice,
    names_to = "feature",
    values_to = "coded"
  ) |>
  filter(!is.na(coded)) |>
  group_by(feature, voice) |>
  summarise(
    percentage = 100 * mean(coded == 1),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = voice,
    values_from = percentage
  ) |>
  mutate(
    difference = Collective - Individual,
    feature = recode(
      feature,
      criticism = "Criticism",
      negative_emotions = "Negative emotions",
      dramatization = "Dramatization",
      jw_people_centrism = "People-centrism",
      jw_anti_elitism = "Anti-elitism",
      symbolic_presentation = "Symbolic presentation"
    )
  )

voice_message_plot <-
  voice_message_df |>
  ggplot(
    aes(y = fct_reorder(feature, difference))
  ) +
  geom_segment(
    aes(
      x = Individual,
      xend = Collective,
      yend = fct_reorder(feature, difference)
    ),
    colour = "grey70",
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      x = Individual,
      colour = "Individual-dominant"
    ),
    size = 3.8
  ) +
  geom_point(
    aes(
      x = Collective,
      colour = "Collective-dominant"
    ),
    size = 3.8
  ) +
  geom_text(
    aes(
      x = pmax(Collective, Individual) + 3,
      label = sprintf("%+.0f pp", difference)
    ),
    hjust = 0,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_colour_manual(
    values = c(
      "Collective-dominant" = "#356D8D",
      "Individual-dominant" = "#965439"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    labels = scales::label_percent(scale = 1)
  ) +
  coord_cartesian(
    xlim = c(0, 112)
  ) +
  labs(
    x = "Share of posts containing each feature",
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(colour = "#202124")
  )

voice_message_plot


# Figure 3: Who Sounds Most Alike?

tfidf_words <-
  words |>
  anti_join(
    tibble(word = stopwords::stopwords("no")),
    by = "word"
  ) |>
  filter(str_detect(word, "^[[:alpha:]]{2,}$")) |>
  count(politician, word, name = "n") |>
  group_by(word) |>
  filter(sum(n) >= 3) |>
  ungroup() |>
  bind_tf_idf(word, politician, n) |>
  filter(tf_idf > 0)

similarity_pairs <-
  tfidf_words |>
  pairwise_similarity(
    politician,
    word,
    tf_idf
  ) |>
  transmute(
    politician_1 = item1,
    politician_2 = item2,
    similarity = similarity
  )

similarity_pairs <-
  bind_rows(
    similarity_pairs,
    similarity_pairs |>
      transmute(
        politician_1 = politician_2,
        politician_2 = politician_1,
        similarity
      )
  ) |>
  distinct(
    politician_1,
    politician_2,
    .keep_all = TRUE
  )

politicians <-
  tfidf_words |>
  distinct(politician) |>
  arrange(politician) |>
  pull(politician)

similarity_df <-
  crossing(
    politician_1 = politicians,
    politician_2 = politicians
  ) |>
  left_join(
    similarity_pairs,
    by = c("politician_1", "politician_2")
  ) |>
  mutate(
    similarity = if_else(
      politician_1 == politician_2,
      1,
      replace_na(similarity, 0)
    )
  )

similarity_matrix <-
  similarity_df |>
  pivot_wider(
    names_from = politician_2,
    values_from = similarity
  ) |>
  tibble::column_to_rownames("politician_1") |>
  as.matrix()

politician_order <-
  rownames(similarity_matrix)[
    hclust(
      as.dist(1 - similarity_matrix),
      method = "average"
    )$order
  ]

heatmap_df <-
  similarity_df |>
  mutate(
    display_similarity = if_else(
      politician_1 == politician_2,
      NA_real_,
      similarity
    ),
    politician_1 = factor(
      politician_1,
      levels = politician_order
    ),
    politician_2 = factor(
      politician_2,
      levels = rev(politician_order)
    )
  )

max_similarity <-
  max(
    heatmap_df$display_similarity,
    na.rm = TRUE
  )

similarity_plot <-
  heatmap_df |>
  ggplot(
    aes(
      x = politician_1,
      y = politician_2,
      fill = display_similarity
    )
  ) +
  geom_tile(
    colour = "white",
    linewidth = 1
  ) +
  ggfittext::geom_fit_text(
    aes(
      label = if_else(
        is.na(display_similarity),
        "???",
        sprintf("%.2f", display_similarity)
      ),
      colour = display_similarity > max_similarity * 0.55
    ),
    grow = FALSE,
    reflow = FALSE,
    min.size = 2.5,
    fontface = "bold",
    show.legend = FALSE,
    contrast = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "FALSE" = "grey20",
      "TRUE" = "white"
    ),
    na.value = "grey50"
  ) +
  scale_fill_gradient(
    low = "#F1F6FA",
    high = "#356D8D",
    limits = c(0, max_similarity),
    na.value = "grey93",
    name = "Cosine similarity"
  ) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 40,
      hjust = 1
    ),
    axis.text.y = element_text(
      colour = "#202124"
    )
  )

similarity_plot


# Figure 4: Communication Strategies

strategy_df <-
  facebook |>
  select(
    politician,
    negative_emotions,
    dramatization,
    criticism,
    mobilisering,
    symbolic_presentation,
    input_seeking
  ) |>
  pivot_longer(
    cols = -politician,
    names_to = "strategy",
    values_to = "coded"
  ) |>
  filter(coded == 1) |>
  count(politician, strategy, name = "n") |>
  group_by(politician) |>
  mutate(
    share = n / sum(n)
  ) |>
  ungroup() |>
  mutate(
    strategy = recode(
      strategy,
      negative_emotions = "Negative emotions",
      dramatization = "Dramatization",
      criticism = "Criticism",
      mobilisering = "Mobilisation",
      symbolic_presentation = "Symbolic presentation",
      input_seeking = "Input seeking"
    ),
    strategy = factor(
      strategy,
      levels = c(
        "Input seeking",
        "Symbolic presentation",
        "Mobilisation",
        "Criticism",
        "Dramatization",
        "Negative emotions"
      )
    ),
    label_colour = if_else(
      strategy %in% c(
        "Symbolic presentation",
        "Input seeking"
      ),
      "#1F2D3A",
      "white"
    )
  )

strategy_colours <-
  set_names(
    MetBrewer::met.brewer("Hokusai3", 7)[2:7],
    c(
      "Input seeking",
      "Symbolic presentation",
      "Mobilisation",
      "Criticism",
      "Dramatization",
      "Negative emotions"
    )
  )

strategy_plot <-
  strategy_df |>
  ggplot(
    aes(
      x = politician,
      y = share,
      fill = strategy
    )
  ) +
  geom_col(
    width = 0.76,
    colour = "white",
    linewidth = 0.5
  ) +
  ggfittext::geom_fit_text(
    aes(
      label = if_else(
        share >= 0.03,
        scales::percent(share, accuracy = 1),
        ""
      ),
      colour = label_colour
    ),
    position = "stack",
    place = "middle",
    grow = FALSE,
    reflow = FALSE,
    min.size = 2.5,
    fontface = "bold",
    show.legend = FALSE,
    contrast = FALSE
  ) +
  scale_colour_identity() +
  coord_flip() +
  scale_fill_manual(
    values = strategy_colours
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Share of communication strategies (%)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      colour = "#243447"
    ),
    axis.text.x = element_text(
      size = 11,
      colour = "#5B6B7A"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "#E6EBF0",
      linewidth = 0.4
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 10.5
    ),
    legend.key.size = grid::unit(
      0.55,
      "cm"
    )
  )

strategy_plot


# Figure 5: The Political Attention Gap

attention_posts <-
  facebook |>
  filter(!is.na(shares)) |>
  group_by(politician) |>
  arrange(
    desc(shares),
    .by_group = TRUE
  ) |>
  mutate(
    top_shared = row_number() <= ceiling(0.20 * n())
  ) |>
  ungroup()

attention_df <-
  attention_posts |>
  select(
    top_shared,
    criticism,
    negative_emotions,
    dramatization,
    jw_people_centrism,
    asg_issue_frame,
    jw_anti_elitism,
    input_seeking,
    symbolic_presentation
  ) |>
  pivot_longer(
    cols = -top_shared,
    names_to = "feature",
    values_to = "coded"
  ) |>
  filter(!is.na(coded)) |>
  group_by(feature) |>
  summarise(
    all_posts = 100 * mean(coded == 1),
    most_shared = 100 * mean(coded[top_shared] == 1),
    .groups = "drop"
  ) |>
  mutate(
    difference = most_shared - all_posts,
    feature = recode(
      feature,
      criticism = "Criticism",
      negative_emotions = "Negative emotions",
      dramatization = "Dramatization",
      jw_people_centrism = "People-centrism",
      asg_issue_frame = "Issue framing",
      jw_anti_elitism = "Anti-elitism",
      input_seeking = "Input seeking",
      symbolic_presentation = "Symbolic presentation"
    )
  )

attention_plot <-
  attention_df |>
  ggplot(
    aes(y = fct_reorder(feature, difference))
  ) +
  geom_segment(
    aes(
      x = all_posts,
      xend = most_shared,
      yend = fct_reorder(feature, difference)
    ),
    colour = "grey70",
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      x = all_posts,
      colour = "All posts"
    ),
    size = 3.8
  ) +
  geom_point(
    aes(
      x = most_shared,
      colour = "Most shared posts"
    ),
    size = 3.8
  ) +
  geom_text(
    aes(
      x = pmax(all_posts, most_shared) + 2,
      label = sprintf("%+.0f pp", difference)
    ),
    hjust = 0,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_colour_manual(
    values = c(
      "All posts" = "#A6A6A6",
      "Most shared posts" = "#356D8D"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    labels = scales::label_percent(scale = 1)
  ) +
  coord_cartesian(
    xlim = c(0, 105)
  ) +
  labs(
    x = "Share of posts containing each communication feature",
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(colour = "#202124")
  )

attention_plot


# Save figures

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  "figures/figure1_pronoun_shares.png",
  plot = we_me_plot,
  width = 11,
  height = 6,
  dpi = 350,
  bg = "white"
)

ggsave(
  "figures/figure2_pronoun_features.png",
  plot = voice_message_plot,
  width = 11,
  height = 6,
  dpi = 350,
  bg = "white"
)

ggsave(
  "figures/figure3_lexical_similarity.png",
  plot = similarity_plot,
  width = 11,
  height = 9,
  dpi = 350,
  bg = "white"
)

ggsave(
  "figures/figure4_strategy_distribution.png",
  plot = strategy_plot,
  width = 12.5,
  height = 6.8,
  dpi = 350,
  bg = "white"
)

ggsave(
  "figures/figure5_attention_gap.png",
  plot = attention_plot,
  width = 11,
  height = 6,
  dpi = 350,
  bg = "white"
)


