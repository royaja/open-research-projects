
# Dette prosjektet ser p?? hva som har blitt publisert p?? Doffin.
# Her samles s??ketreff og tekst fra utlysninger om evaluering, analyse og utredning.
# Materialet som brukes i prosjektet dekker perioden april 2024 til mai 2025.
# Kode av R.A. Jacobsen.

library(tidyverse)
library(chromote)
library(jsonlite)
library(httr2)

search_terms <- c(
  "evaluering",
  "folgeevaluering",
  "analyse",
  "utredning"
)

from_date <- "2024-04-11"
to_date <- "2026-04-11"
hits_per_page <- 100
max_pages <- 40
max_empty_pages <- 3

clean_text <- function(x) {
  x |>
    replace_na("") |>
    str_replace_all("\\u00a0", " ") |>
    str_replace_all("[[:space:]]+", " ") |>
    str_trim()
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

pause_briefly <- function() {
  Sys.sleep(runif(1, 0.8, 1.8))
}

build_search_url <- function(search_term, page) {
  req <- request("https://www.doffin.no/search") |>
    req_url_query(
      searchString = search_term,
      fromDate = from_date,
      toDate = to_date,
      page = page,
      sortBy = "PUBLICATION_DATE_ASC",
      hitsPerPage = hits_per_page
    )

  req$url
}

run_js_json <- function(browser, js_code) {
  out <- browser$Runtime$evaluate(expression = js_code)
  fromJSON(out$result$value, simplifyDataFrame = TRUE)
}

get_search_page <- function(browser, search_term, page) {
  url <- build_search_url(search_term, page)

  browser$Page$navigate(url)
  Sys.sleep(6)

  js <- "
  (() => {
    const abs = (href) => {
      try {
        return new URL(href, window.location.origin).href;
      } catch(e) {
        return href;
      }
    };

    const nodes = [...document.querySelectorAll('a[href*=\"/notices/\"]')];

    const rows = nodes.map(a => {
      let container = a;

      for (let i = 0; i < 6; i++) {
        if (!container || !container.parentElement) break;
        container = container.parentElement;

        const txt = (container.innerText || '').trim();
        const linkCount = container.querySelectorAll('a[href*=\"/notices/\"]').length;

        if (txt.length > 50 && txt.length < 3000 && linkCount <= 3) {
          break;
        }
      }

      return {
        title: (a.innerText || '').trim(),
        link: abs(a.getAttribute('href')),
        card_text: ((container && container.innerText) ? container.innerText : '').trim()
      };
    });

    const seen = new Set();
    const filtered = rows.filter(x => {
      if (!x.link || !x.title) return false;
      if (seen.has(x.link)) return false;
      seen.add(x.link);
      return true;
    });

    return JSON.stringify(filtered);
  })();
  "

  res <- run_js_json(browser, js)

  if (length(res) == 0) {
    return(tibble())
  }

  tibble(
    search_term = search_term,
    page = page,
    title = clean_text(res$title),
    link = clean_text(res$link),
    card_text = clean_text(res$card_text)
  ) |>
    filter(
      str_detect(link, "/notices/"),
      title != ""
    ) |>
    distinct(link, .keep_all = TRUE)
}

scrape_search_term <- function(browser, search_term) {
  out <- vector("list", max_pages)
  empty_pages <- 0

  for (page in seq_len(max_pages)) {
    message("Search term: ", search_term, " | page: ", page)

    page_data <- tryCatch(
      get_search_page(browser, search_term, page),
      error = function(e) tibble()
    )

    out[[page]] <- page_data

    if (nrow(page_data) == 0) {
      empty_pages <- empty_pages + 1
    } else {
      empty_pages <- 0
    }

    if (empty_pages >= max_empty_pages) {
      break
    }

    pause_briefly()
  }

  bind_rows(out)
}

browser_search <- ChromoteSession$new()

all_search_results <-
  map_dfr(search_terms, function(term) {
    scrape_search_term(browser_search, term)
  }) |>
  distinct(search_term, link, .keep_all = TRUE)

try(browser_search$close(), silent = TRUE)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

write_csv(all_search_results, "data/raw/doffin_search_results_raw.csv")

search_results_unique <-
  all_search_results |>
  group_by(link) |>
  summarise(
    search_terms = paste(sort(unique(search_term)), collapse = " | "),
    pages_found = paste(sort(unique(page)), collapse = " | "),
    title = first(title),
    card_text = first(card_text),
    .groups = "drop"
  ) |>
  mutate(notice_id = str_extract(link, "\\d{4}-\\d+"))

write_csv(search_results_unique, "data/raw/doffin_search_results_unique.csv")

get_notice_page <- function(browser, url) {
  browser$Page$navigate(url)
  Sys.sleep(5)

  js <- "
  (() => {
    const bodyText = (document.body && document.body.innerText) ? document.body.innerText.trim() : '';
    const h1 = document.querySelector('h1') ? document.querySelector('h1').innerText.trim() : '';

    return JSON.stringify({
      page_title: document.title || '',
      h1: h1,
      body_text: bodyText
    });
  })();
  "

  res <- run_js_json(browser, js)

  tibble(
    link = url,
    page_title = clean_text(res$page_title %||% ""),
    notice_title = clean_text(res$h1 %||% ""),
    body_text = clean_text(res$body_text %||% "")
  )
}

browser_notice <- ChromoteSession$new()

notice_pages <-
  map_dfr(seq_along(search_results_unique$link), function(i) {
    message("Notice ", i, " of ", length(search_results_unique$link))

    out <- tryCatch(
      get_notice_page(browser_notice, search_results_unique$link[[i]]),
      error = function(e) {
        tibble(
          link = search_results_unique$link[[i]],
          page_title = NA_character_,
          notice_title = NA_character_,
          body_text = NA_character_
        )
      }
    )

    pause_briefly()
    out
  })

try(browser_notice$close(), silent = TRUE)

write_csv(notice_pages, "data/raw/doffin_notice_pages_raw.csv")

doffin_raw <-
  search_results_unique |>
  left_join(notice_pages, by = "link") |>
  mutate(title = coalesce(notice_title, title)) |>
  select(
    notice_id,
    title,
    search_terms,
    pages_found,
    link,
    card_text,
    body_text,
    page_title
  )

write_csv(doffin_raw, "data/raw/doffin_eval_dataset_raw.csv")
