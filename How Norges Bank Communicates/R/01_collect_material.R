# collect recent speeches and press releases from Norges Bank

library(xml2)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

sitemap_url <-
  "https://www.norges-bank.no/sitemap.xml"

n_per_category <-
  40

crawl_delay_seconds <-
  5

safe_read_xml <-
  possibly(
    function(x) read_xml(x),
    otherwise = NULL
  )

safe_read_html <-
  possibly(
    function(x) read_html(x),
    otherwise = NULL
  )

extract_date_from_url <-
  function(url) {
    date_str <-
      str_extract(url, "\\d{4}-\\d{2}-\\d{2}")

    as.Date(date_str)
  }

extract_published_line <-
  function(page_text) {
    str_extract(
      page_text,
      "Published\\s+[0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4}(\\s+[0-9]{2}:[0-9]{2})?"
    )
  }

extract_published_date <-
  function(published_line) {
    date_part <-
      str_extract(
        published_line,
        "[0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4}"
      )

    suppressWarnings(
      as.Date(date_part, format = "%d %B %Y")
    )
  }

extract_published_time <-
  function(published_line) {
    str_extract(published_line, "[0-9]{2}:[0-9]{2}$")
  }

is_valid_text_line <-
  function(x) {
    !is.na(x) &
      x != "" &
      !str_detect(x, "^Published\\s") &
      !str_detect(x, "^Print$") &
      !str_detect(x, "^Press release$") &
      !str_detect(x, "^Speech$") &
      !str_detect(x, "^Contact:?$") &
      !str_detect(x, "^Press telephone:") &
      !str_detect(x, "^Email:") &
      !str_detect(x, "^Did you find what you were looking for") &
      !str_detect(x, "^Thank you for your feedback") &
      !str_detect(x, "^More information$") &
      !str_detect(x, "^Download presentation") &
      !str_detect(x, "^Chart:") &
      !str_detect(x, "^Javascript is disabled$") &
      !str_detect(
        x,
        "^Due to this, parts of the content on this site will not be displayed\\.?$"
      )
  }

sitemap_index <-
  safe_read_xml(sitemap_url)

if (is.null(sitemap_index)) {
  stop("Could not read sitemap index.")
}

child_sitemaps <-
  sitemap_index %>%
  xml_find_all(".//*[local-name()='loc']") %>%
  xml_text()

child_sitemaps <-
  child_sitemaps[
    str_detect(child_sitemaps, "content\\.xml")
  ]

get_urls_from_sitemap <-
  function(sitemap_url) {
    doc <-
      safe_read_xml(sitemap_url)

    if (is.null(doc)) {
      return(
        tibble(
          sitemap = character(),
          loc = character(),
          lastmod = character()
        )
      )
    }

    locs <-
      doc %>%
      xml_find_all(".//*[local-name()='url']/*[local-name()='loc']") %>%
      xml_text()

    lastmods <-
      doc %>%
      xml_find_all(".//*[local-name()='url']/*[local-name()='lastmod']") %>%
      xml_text()

    if (length(lastmods) < length(locs)) {
      lastmods <-
        c(
          lastmods,
          rep(NA_character_, length(locs) - length(lastmods))
        )
    }

    tibble(
      sitemap = sitemap_url,
      loc = locs,
      lastmod = lastmods
    )
  }

all_urls <-
  map_dfr(child_sitemaps, get_urls_from_sitemap)

candidate_urls <-
  all_urls %>%
  transmute(
    url = loc,
    lastmod = lastmod,
    category = case_when(
      str_detect(url, "/en/news-events/news/Press-releases/") ~ "press_release",
      str_detect(url, "/en/news-events/news/Speeches/") ~ "speech",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(category)) %>%
  distinct(url, .keep_all = TRUE) %>%
  mutate(
    url_date = extract_date_from_url(url)
  ) %>%
  filter(!is.na(url_date))

selected_urls <-
  candidate_urls %>%
  group_by(category) %>%
  arrange(desc(url_date), .by_group = TRUE) %>%
  slice_head(n = n_per_category) %>%
  ungroup()

scrape_page_paragraphs <-
  function(url, category, delay_seconds = 5) {

    Sys.sleep(delay_seconds)

    pg <-
      safe_read_html(url)

    if (is.null(pg)) {
      return(
        tibble(
          category = category,
          url = url,
          title = NA_character_,
          published_line = NA_character_,
          published_date = as.Date(NA),
          published_time = NA_character_,
          paragraph_id = integer(),
          paragraph_type = character(),
          paragraph_text = character()
        )
      )
    }

    title <-
      pg %>%
      html_element("h1") %>%
      html_text2() %>%
      str_squish()

    main_node <-
      pg %>%
      html_element("main")

    page_text <-
      main_node %>%
      html_text2()

    published_line <-
      extract_published_line(page_text)

    published_date <-
      extract_published_date(published_line)

    published_time <-
      extract_published_time(published_line)

    nodes <-
      main_node %>%
      html_elements("p, li, blockquote")

    if (length(nodes) == 0) {
      return(
        tibble(
          category = category,
          url = url,
          title = title,
          published_line = published_line,
          published_date = published_date,
          published_time = published_time,
          paragraph_id = integer(),
          paragraph_type = character(),
          paragraph_text = character()
        )
      )
    }

    tibble(
      paragraph_type = html_name(nodes),
      paragraph_text = html_text2(nodes) %>% str_squish()
    ) %>%
      filter(is_valid_text_line(paragraph_text)) %>%
      mutate(
        paragraph_id = row_number(),
        category = category,
        url = url,
        title = title,
        published_line = published_line,
        published_date = published_date,
        published_time = published_time,
        .before = 1
      )
  }

paragraph_data <-
  pmap_dfr(
    list(selected_urls$url, selected_urls$category),
    function(url, category) {
      scrape_page_paragraphs(
        url = url,
        category = category,
        delay_seconds = crawl_delay_seconds
      )
    }
  )

document_data <-
  paragraph_data %>%
  group_by(
    category,
    url,
    title,
    published_line,
    published_date,
    published_time
  ) %>%
  summarise(
    full_text = paste(paragraph_text, collapse = "\n\n"),
    n_paragraphs = n(),
    .groups = "drop"
  ) %>%
  arrange(category, desc(published_date), desc(published_time))

write_csv(
  paragraph_data,
  "data/raw/collected_paragraphs.csv"
)

write_csv(
  document_data,
  "data/raw/collected_documents.csv"
)
