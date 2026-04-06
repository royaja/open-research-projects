# clean local material and prepare the public data files

library(tidyverse)

dir.create("data/intermediate", recursive = TRUE, showWarnings = FALSE)

paragraphs_raw <-
  read_csv(
    "data/raw/collected_paragraphs.csv",
    show_col_types = FALSE
  )

lines_to_remove <-
  c(
    "Regional Network report 4/25",
    "About Norges Bank's regional network",
    "News & events",
    "For more information, please see the Q&A.",
    "Annual report 2025",
    "Regional Network report 1/26",
    "Read the report",
    "Financial Stability Report 2025 H2",
    "Regional Network report 2/25",
    "About the Regional Network",
    "Report: Regional Network 1-2025",
    "Annual report 2024",
    "Download the presentation (pdf)",
    "Facts and history",
    "Chart data (xlsx)"
  )

paragraphs_clean <-
  paragraphs_raw %>%
  mutate(
    published_date = as.Date(published_date),
    category = case_when(
      category == "press_release" ~ "Press release",
      category == "speech" ~ "Speech",
      TRUE ~ category
    )
  ) %>%
  mutate(
    across(c(title, paragraph_text, paragraph_type, url), str_squish)
  ) %>%
  filter(
    !is.na(paragraph_text),
    paragraph_text != ""
  ) %>%
  filter(
    !paragraph_text %in% lines_to_remove
  ) %>%
  filter(
    !str_detect(paragraph_text, "^Regional Network:"),
    !str_detect(paragraph_text, "^News & events$"),
    !str_detect(paragraph_text, "^\\[[0-9]+\\]"),
    !str_detect(paragraph_text, "\\(pdf\\)$"),
    !str_detect(paragraph_text, "\\(xlsx\\)$"),
    !str_detect(paragraph_text, "^The Annual address .*\\(in Norwegian\\)$"),
    !str_detect(paragraph_text, "^Annual address .*\\(pdf\\)$")
  ) %>%
  mutate(
    paragraph_nwords = str_count(paragraph_text, "\\S+")
  ) %>%
  filter(
    paragraph_nwords >= 3
  ) %>%
  mutate(
    doc_id = str_replace_all(url, "[^A-Za-z0-9]", "_"),
    paragraph_uid = paste0(doc_id, "_p", paragraph_id),
    year = as.integer(format(published_date, "%Y")),
    month = as.integer(format(published_date, "%m"))
  ) %>%
  distinct()

document_inventory <-
  paragraphs_clean %>%
  group_by(
    doc_id,
    category,
    url,
    title,
    published_date,
    year,
    month
  ) %>%
  summarise(
    n_paragraphs = n(),
    .groups = "drop"
  ) %>%
  distinct()

write_csv(
  paragraphs_clean,
  "data/intermediate/paragraphs_clean.csv"
)

write_csv(
  document_inventory,
  "data/intermediate/document_inventory.csv"
)

coded_local <-
  read_csv(
    "data/local/coded_paragraphs_full.csv",
    locale = locale(encoding = "ISO-8859-1"),
    show_col_types = FALSE
  )

coded_public <-
  coded_local %>%
  select(
    -matches("^Unnamed"),
    -paragraph_text,
    -published_line
  )

write_csv(
  coded_public,
  "data/coded_paragraphs.csv"
)

write_csv(
  document_inventory,
  "data/document_inventory.csv"
)
