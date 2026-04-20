
# Dette skriptet bygger et analyseklar datasett fra innsamlede Doffin-data.
# Datasettet brukes i analysen av utlysninger om evaluering, analyse og utredning.
# Materialet i prosjektet dekker perioden april 2024 til mai 2025.
# Kode av R.A. Jacobsen.

library(tidyverse)

clean_text <- function(x) {
  x |>
    replace_na("") |>
    str_replace_all("\\u00a0", " ") |>
    str_replace_all("[[:space:]]+", " ") |>
    str_trim() |>
    na_if("")
}

extract_publication_date <- function(text) {
  x <- str_extract(text, "\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b")
  ifelse(is.na(x), NA_character_, x)
}

extract_notice_type <- function(text) {
  x <- str_match(
    text,
    regex("Type kunngjoring\\s+(.+?)(Referanse|Status|Anbudstype|$)", ignore_case = TRUE)
  )[, 2]

  clean_text(x)
}

extract_contracting_authority <- function(text) {
  x <- str_match(
    text,
    regex("Oppdragsgiver\\s*:?\\s*(.+?)(Publisert|Type kunngjoring|Anskaffelse|$)", ignore_case = TRUE)
  )[, 2]

  clean_text(x)
}

extract_method_notes <- function(text) {
  txt <- text |>
    replace_na("") |>
    str_to_lower()

  terms <- c(
    "intervju",
    "survey",
    "registerdata",
    "dokumentanalyse",
    "kvantitativ",
    "kvalitativ"
  )

  terms_found <- terms[str_detect(txt, terms)]

  if (length(terms_found) == 0) {
    return(NA_character_)
  }

  paste(unique(terms_found), collapse = ", ")
}

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

doffin_raw <- read_csv("data/raw/doffin_eval_dataset_raw.csv", show_col_types = FALSE)

doffin_text <-
  doffin_raw |>
  mutate(
    title = clean_text(title),
    card_text = clean_text(card_text),
    body_text = clean_text(body_text),
    analysis_text = str_to_lower(str_c(coalesce(title, ""), coalesce(body_text, ""), sep = " "))
  )

doffin_dataset <-
  doffin_text |>
  mutate(
    publication_date = coalesce(
      extract_publication_date(body_text),
      extract_publication_date(card_text)
    ),
    notice_type = coalesce(
      extract_notice_type(body_text),
      extract_notice_type(card_text)
    ),
    contracting_authority = coalesce(
      extract_contracting_authority(body_text),
      extract_contracting_authority(card_text)
    )
  ) |>
  mutate(
    likely_relevant = case_when(
      str_detect(analysis_text, "evaluering|analyse|utredning") &
        !str_detect(analysis_text, "revisjon|regnskap|advokat|programvare|it-drift|reklame") ~ "ja",
      str_detect(analysis_text, "evaluering|analyse|utredning") ~ "usikker",
      TRUE ~ "nei"
    )
  ) |>
  mutate(
    assignment_type = case_when(
      str_detect(analysis_text, "evaluering") & str_detect(analysis_text, "analyse") ~ "evaluering_analyse",
      str_detect(analysis_text, "evaluering") & str_detect(analysis_text, "utredning") ~ "evaluering_utredning",
      str_detect(analysis_text, "evaluering") ~ "evaluering",
      str_detect(analysis_text, "analyse") & str_detect(analysis_text, "utredning") ~ "analyse_utredning",
      str_detect(analysis_text, "analyse") ~ "analyse",
      str_detect(analysis_text, "utredning") ~ "utredning",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    policy_field = case_when(
      str_detect(analysis_text, "helse|sykehus|omsorg|pasient") ~ "helse",
      str_detect(analysis_text, "skole|utdanning|barnehage") ~ "utdanning",
      str_detect(analysis_text, "nav|arbeid|velferd|trygd") ~ "arbeid_velferd",
      str_detect(analysis_text, "justis|beredskap|politi|domstol") ~ "justis_beredskap",
      str_detect(analysis_text, "innovasjon|bedrift|industri") ~ "innovasjon",
      str_detect(analysis_text, "klima|natur|energi") ~ "klima",
      str_detect(analysis_text, "utenriks|bistand|internasjonal|eu") ~ "utenriks_utvikling",
      str_detect(analysis_text, "kultur|kunst|museum|medie|artist|musikk|kirke") ~ "kultur",
      TRUE ~ "annet"
    )
  ) |>
  mutate(
    method_requirements = case_when(
      str_detect(analysis_text, "intervju|survey|registerdata|dokumentanalyse|kvantitativ|kvalitativ") ~ "ja",
      TRUE ~ "nei"
    ),
    method_notes = map_chr(body_text, extract_method_notes),
    notes = NA_character_
  ) |>
  select(
    notice_id,
    title,
    publication_date,
    notice_type,
    contracting_authority,
    assignment_type,
    policy_field,
    method_requirements,
    method_notes,
    search_terms,
    pages_found,
    link,
    likely_relevant,
    notes,
    card_text,
    body_text
  )

write_csv(doffin_dataset, "data/processed/doffin_eval_dataset_full.csv")

doffin_candidates <-
  doffin_dataset |>
  filter(likely_relevant %in% c("ja", "usikker"))

write_csv(doffin_candidates, "data/processed/doffin_eval_candidates.csv")

doffin_review <-
  doffin_candidates |>
  mutate(
    include_manual = NA_character_,
    policy_field_manual = NA_character_,
    notes_manual = NA_character_
  )

write_csv(doffin_review, "data/processed/doffin_eval_review_file.csv")
