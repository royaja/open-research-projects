
# Dette prosjektet ser p?? hva som har blitt publisert p?? Doffin.
# Doffin er den nasjonale databasen for offentlige anskaffelser i Norge.
# Offentlige virksomheter bruker portalen til ?? lyse ut anbud og oppdrag.
# Her analyseres kunngj??ringer knyttet til evaluering, analyse og utredning.
# Det tilgjengelige materialet i denne analysen dekker perioden april 2024 til mai 2025.
# Kode av R.A. Jacobsen

library(tidyverse)
library(lubridate)
library(showtext)
library(sysfonts)
library(scales)

font_add_google("Lato", "lato")
showtext_auto()

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

doffin <-
  read_csv(
    "data/processed/doffin_eval_candidates.csv",
    show_col_types = FALSE
  )

doffin <-
  doffin %>%
  mutate(
    publication_date = coalesce(
      parse_date_time(publication_date, orders = c("dmy", "ymd")) |> as_date(),
      str_extract(body_text, "\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b") |> dmy(),
      str_extract(card_text, "\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b") |> dmy()
    )
  ) %>%
  filter(!is.na(publication_date)) %>%
  filter(publication_date <= as.Date("2025-05-31")) %>%
  mutate(
    month = floor_date(publication_date, "month"),
    assignment_type = str_replace_all(assignment_type, "_og_", " + "),
    assignment_type = str_replace_all(assignment_type, "_", " "),
    assignment_type = str_to_sentence(assignment_type),
    policy_field = str_replace_all(policy_field, "_", " "),
    policy_field = str_to_sentence(policy_field),
    policy_field = case_when(
      policy_field == "Arbeid velferd" ~ "Arbeid og velferd",
      policy_field == "Justis beredskap" ~ "Justis og beredskap",
      policy_field == "Klima" ~ "Klima og energi",
      TRUE ~ policy_field
    ),
    contracting_authority = str_squish(contracting_authority),
    contracting_authority = na_if(contracting_authority, ""),
    contracting_authority = if_else(
      is.na(contracting_authority),
      "Mangler",
      contracting_authority
    ),
    method_notes = replace_na(method_notes, "")
  )


theme_doffin <- function() {
  theme_minimal(base_family = "lato", base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "grey10"),
      plot.subtitle = element_text(size = 11, color = "grey35", margin = margin(b = 10)),
      axis.title.x = element_text(color = "grey15", margin = margin(t = 8)),
      axis.title.y = element_text(color = "grey15", margin = margin(r = 10)),
      axis.text.x = element_text(color = "grey35"),
      axis.text.y = element_text(color = "grey25"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#d9d9d9", linewidth = 0.35),
      panel.grid.major.y = element_line(color = "#d9d9d9", linewidth = 0.35),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 20, 10, 10)
    )
}

# Figur 1
doffin <-
  doffin %>%
  mutate(
    publication_date = coalesce(
      parse_date_time(publication_date, orders = c("dmy", "ymd")) |> as_date(),
      str_extract(body_text, "\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b") |> dmy(),
      str_extract(card_text, "\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b") |> dmy()
    ),
    month = floor_date(publication_date, "month")
  )

data_1_tid <-
  doffin %>%
  filter(!is.na(month)) %>%
  count(month) %>%
  filter(month <= as.Date("2025-05-01"))

plot_1_tid <-
  ggplot(data_1_tid, aes(x = month, y = n)) +
  geom_line(linewidth = 1, color = "#7e9c97") +
  geom_point(size = 2.6, color = "#7e9c97") +
  scale_x_date(
    breaks = seq(as.Date("2024-04-01"), as.Date("2025-05-01"), by = "2 months"),
    date_labels = "%m.%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = c(60, 80, 100, 120),
    limits = c(55, 120),
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Utlysninger over tid",
    subtitle = "Antall Doffin-utlysninger med evaluering, analyse eller utredning, april 2024 til mai 2025",
    x = NULL,
    y = "Antall"
  ) +
  theme_doffin() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# figur 2
data_2_type <-
  doffin %>%
  mutate(
    assignment_type = str_replace_all(assignment_type, "_", " + "),
    assignment_type = str_to_sentence(assignment_type)
  ) %>%
  count(assignment_type, sort = TRUE) %>%
  mutate(assignment_type = fct_reorder(assignment_type, n))

plot_2_type <-
  ggplot(data_2_type, aes(x = n, y = assignment_type)) +
  geom_col(width = 0.72, fill = "#b7cec7") +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 3.7,
    family = "lato",
    color = "grey25"
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "De fleste bruker ett begrep",
    x = "Antall",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_doffin() +
  theme(
    panel.grid.major.y = element_blank()
  )

plot_2_type

# Figur 3 
data_3_felt <-
  doffin %>%
  count(policy_field, sort = TRUE) %>%
  mutate(policy_field = fct_reorder(policy_field, n))

plot_3_felt <-
  ggplot(data_3_felt, aes(x = n, y = policy_field)) +
  geom_col(width = 0.72, fill = "#aebfc6") +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 3.7,
    family = "lato",
    color = "grey25"
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Kategoriene hvor utlysningene samler seg",
    x = "Antall",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_doffin() +
  theme(
    panel.grid.major.y = element_blank()
  )

# Figur 4 
data_4_metode <-
  doffin %>%
  filter(method_requirements == "ja", method_notes != "") %>%
  separate_rows(method_notes, sep = ",\\s*") %>%
  mutate(method_notes = str_to_sentence(method_notes)) %>%
  count(method_notes, sort = TRUE) %>%
  mutate(method_notes = fct_reorder(method_notes, n))

plot_4_metode <-
  ggplot(data_4_metode, aes(x = n, y = method_notes)) +
  geom_col(width = 0.72, fill = "#d6dccd") +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 3.7,
    family = "lato",
    color = "grey25"
  ) +
  scale_x_continuous(
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Metoder som nevnes i utlysningene",
    subtitle = "En utlysning kan nevne flere metoder",
    x = "Antall metodeomtaler",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_doffin() +
  theme(
    panel.grid.major.y = element_blank()
  )

plot_1_tid
plot_2_type
plot_3_felt
plot_4_metode

