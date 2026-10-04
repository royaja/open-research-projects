# Pasientkrav, standardisering og lokal variasjon i digital sykehusinformasjon
#
# Data er hentet fra offentlig tilgjengelig pasientinformasjon p?? nettsidene til
# Vestre Viken HF, Oslo universitetssykehus HF, Akershus universitetssykehus HF,
# Sykehuset i Vestfold HF og Sykehuset Telemark HF.
#
# Datamaterialet best??r av 80 nettsider fordelt p?? 16 behandlinger og unders??kelser
# ved fem helseforetak. Nettsidene ble hentet og kodet 1. oktober 2026.
#
# For en n??rmere beskrivelse av datainnsamling, utvalg, operasjonalisering og koding,
# se metodenotatet som f??lger prosjektet.
#
#
# Skriptet er lagret i UTF-8.
# 
# https://orcid.org/0000-0002-0906-2780
# https://doi.org/10.5281/zenodo.23133196 


# Libraries

library(tidyverse)
library(readxl)


# Data

datafil <-
  file.path(
    "data",
    "2026-10-02 Pasientinformasjon helseforetak.xlsx"
  )

helse_df <-
  read_xlsx(
    datafil,
    sheet = "Data"
  )


# Wrangle data

helse_df <-
  helse_df %>%
  mutate(
    foretak_kort = recode(
      foretak,
      "Akershus universitetssykehus HF" = "Ahus",
      "Oslo universitetssykehus HF" = "OUS",
      "Sykehuset Telemark HF" = "Telemark",
      "Sykehuset i Vestfold HF" = "Vestfold",
      "Vestre Viken HF" = "Vestre Viken"
    )
  )


# Kontroll av data

kontroll <-
  helse_df %>%
  summarise(
    nettsider = n(),
    behandlinger = n_distinct(behandling),
    helseforetak = n_distinct(foretak),
    unike_id = n_distinct(id),
    unike_url = n_distinct(url)
  )

kontroll

stopifnot(
  kontroll$nettsider == 80,
  kontroll$behandlinger == 16,
  kontroll$helseforetak == 5,
  kontroll$unike_id == 80,
  kontroll$unike_url == 80
)


# Hovedtall

hovedtall <-
  helse_df %>%
  summarise(
    handlinger_gjennomsnitt = mean(handlinger),
    handlinger_median = median(handlinger),
    tidskrav_gjennomsnitt = mean(tidskrav),
    betingelser_gjennomsnitt = mean(betingelser),
    minst_fem_handlinger = sum(handlinger >= 5),
    minst_ti_handlinger = sum(handlinger >= 10),
    alle_tre_kravtyper = sum(
      handlinger > 0 &
        tidskrav > 0 &
        betingelser > 0
    )
  )

hovedtall


# Helseforetak

helseforetak <-
  helse_df %>%
  group_by(foretak_kort) %>%
  summarise(
    handlinger = mean(handlinger),
    tidskrav = mean(tidskrav),
    betingelser = mean(betingelser),
    .groups = "drop"
  ) %>%
  arrange(desc(handlinger))

helseforetak


# Behandlinger og undersC8kelser

behandlinger <-
  helse_df %>%
  group_by(behandling) %>%
  summarise(
    handlinger_gjennomsnitt = mean(handlinger),
    tidskrav_gjennomsnitt = mean(tidskrav),
    betingelser_gjennomsnitt = mean(betingelser),
    .groups = "drop"
  ) %>%
  arrange(desc(handlinger_gjennomsnitt))

behandlinger


# Kravtyper

kravtyper <-
  helse_df %>%
  summarise(
    medisin = mean(medisin) * 100,
    mat_faste = mean(mat_faste) * 100,
    hygiene = mean(hygiene) * 100,
    transport = mean(transport) * 100,
    ta_med = mean(ta_med) * 100,
    kontakt = mean(kontakt) * 100,
    ekstern_info = mean(ekstern_info) * 100,
    sikkerhet = mean(sikkerhet) * 100,
    konsekvens = mean(konsekvens) * 100
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "krav",
    values_to = "andel"
  ) %>%
  mutate(
    krav = recode(
      krav,
      medisin = "Medisin",
      mat_faste = "Mat/faste",
      hygiene = "Hygiene",
      transport = "Transport",
      ta_med = "Ta med",
      kontakt = "Kontakt/varsling",
      ekstern_info = "Ekstern informasjon",
      sikkerhet = "Sikkerhet",
      konsekvens = "Konsekvens ved avvik"
    ),
    andel_etikett = floor(andel + 0.5)
  ) %>%
  arrange(desc(andel))

kravtyper


# Lokale varianter

lokale_varianter <-
  helse_df %>%
  summarise(
    antall = sum(lokal_variant),
    andel = mean(lokal_variant) * 100
  )

lokale_varianter

lokale_varianter_behandling <-
  helse_df %>%
  group_by(behandling) %>%
  summarise(
    lokale_varianter = sum(lokal_variant),
    .groups = "drop"
  ) %>%
  arrange(desc(lokale_varianter))

lokale_varianter_behandling


# Koloskopi

koloskopi <-
  helse_df %>%
  filter(behandling == "Koloskopi") %>%
  select(
    foretak_kort,
    handlinger,
    tidskrav,
    betingelser,
    lokal_variant
  )

koloskopi


# Variansdekomponering

variansdekomponering <- function(data, variabel) {

  variabel <- rlang::ensym(variabel)

  x <-
    data %>%
    transmute(
      behandling,
      foretak,
      verdi = !!variabel
    )

  grand_mean <-
    mean(x$verdi)

  n_foretak <-
    n_distinct(x$foretak)

  n_behandlinger <-
    n_distinct(x$behandling)

  ss_total <-
    sum(
      (x$verdi - grand_mean)^2
    )

  ss_behandling <-
    x %>%
    group_by(behandling) %>%
    summarise(
      gjennomsnitt = mean(verdi),
      .groups = "drop"
    ) %>%
    summarise(
      ss =
        n_foretak *
        sum(
          (gjennomsnitt - grand_mean)^2
        )
    ) %>%
    pull(ss)

  ss_foretak <-
    x %>%
    group_by(foretak) %>%
    summarise(
      gjennomsnitt = mean(verdi),
      .groups = "drop"
    ) %>%
    summarise(
      ss =
        n_behandlinger *
        sum(
          (gjennomsnitt - grand_mean)^2
        )
    ) %>%
    pull(ss)

  ss_rest <-
    ss_total -
    ss_behandling -
    ss_foretak

  tibble(
    komponent = c(
      "Behandling",
      "Helseforetak",
      "Rest"
    ),
    andel =
      100 *
      c(
        ss_behandling,
        ss_foretak,
        ss_rest
      ) /
      ss_total
  )
}


dekomponering <-
  bind_rows(
    variansdekomponering(
      helse_df,
      handlinger
    ) %>%
      mutate(
        krav = "Handlinger"
      ),
    variansdekomponering(
      helse_df,
      tidskrav
    ) %>%
      mutate(
        krav = "Tidskrav"
      ),
    variansdekomponering(
      helse_df,
      betingelser
    ) %>%
      mutate(
        krav = "Betingelser"
      )
  )

dekomponering


# Mappe for figurer

figurmappe <- "figures"

dir.create(
  figurmappe,
  showWarnings = FALSE
)


# Felles figurstil

tema_fig <-
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )


# Figur 1: Gjennomsnittlig antall handlinger per behandling

figur_1 <-
  behandlinger %>%
  mutate(
    behandling = fct_reorder(
      behandling,
      handlinger_gjennomsnitt
    )
  ) %>%
  ggplot(
    aes(
      x = handlinger_gjennomsnitt,
      y = behandling
    )
  ) +
  geom_point(
    size = 3.6,
    color = "#4C78A8"
  ) +
  geom_text(
    aes(
      label = sprintf(
        "%.1f",
        handlinger_gjennomsnitt
      )
    ),
    nudge_x = 0.35,
    hjust = 0,
    size = 3.4
  ) +
  scale_x_continuous(
    breaks = seq(0, 14, 2),
    limits = c(0, 15.5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Antall handlinger",
    y = NULL
  ) +
  tema_fig +
  theme(
    panel.grid.major.y = element_blank()
  )

figur_1

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 1 handlinger per behandling.png"
  ),
  plot = figur_1,
  width = 170,
  height = 125,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 1 handlinger per behandling.pdf"
  ),
  plot = figur_1,
  width = 170,
  height = 125,
  units = "mm"
)


# Figur 2: Typer krav i pasientinformasjonen

figur_2 <-
  kravtyper %>%
  mutate(
    krav = fct_reorder(
      krav,
      andel
    )
  ) %>%
  ggplot(
    aes(
      x = andel,
      y = krav
    )
  ) +
  geom_col(
    width = 0.7,
    fill = "#4C78A8"
  ) +
  geom_text(
    aes(
      label = paste0(
        andel_etikett,
        " %"
      )
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, 20),
    limits = c(0, 80),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Andel av nettsidene (%)",
    y = NULL
  ) +
  tema_fig +
  theme(
    panel.grid.major.y = element_blank()
  )

figur_2

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 2 kravtyper.png"
  ),
  plot = figur_2,
  width = 170,
  height = 105,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 2 kravtyper.pdf"
  ),
  plot = figur_2,
  width = 170,
  height = 105,
  units = "mm"
)


# Figur 3: Oppdeling av observert variasjon

dekomponering_figur <-
  dekomponering %>%
  mutate(
    krav = factor(
      krav,
      levels = c(
        "Handlinger",
        "Tidskrav",
        "Betingelser"
      )
    ),
    komponent = factor(
      komponent,
      levels = c(
        "Behandling",
        "Helseforetak",
        "Rest"
      )
    )
  )

figur_3 <-
  ggplot(
    dekomponering_figur,
    aes(
      x = krav,
      y = andel,
      fill = komponent
    )
  ) +
  geom_col(
    width = 0.7
  ) +
  geom_text(
    aes(
      label = if_else(
        andel >= 4,
        paste0(
          round(andel, 1),
          " %"
        ),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3.2
  ) +
  scale_fill_manual(
    values = c(
      "Behandling" = "#4C78A8",
      "Helseforetak" = "#72B7B2",
      "Rest" = "grey75"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    labels = function(x) paste0(x, " %"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Andel av observert variasjon",
    fill = NULL
  ) +
  tema_fig

figur_3

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 3 variansdekomponering.png"
  ),
  plot = figur_3,
  width = 170,
  height = 95,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 3 variansdekomponering.pdf"
  ),
  plot = figur_3,
  width = 170,
  height = 95,
  units = "mm"
)


# Figur 4: Koloskopi ved fem helseforetak

koloskopi_long <-
  helse_df %>%
  filter(
    behandling == "Koloskopi"
  ) %>%
  mutate(
    foretak_kort = factor(
      foretak_kort,
      levels = c(
        "OUS",
        "Vestre Viken",
        "Vestfold",
        "Ahus",
        "Telemark"
      )
    )
  ) %>%
  select(
    foretak_kort,
    handlinger,
    tidskrav
  ) %>%
  pivot_longer(
    cols = c(
      handlinger,
      tidskrav
    ),
    names_to = "type",
    values_to = "antall"
  ) %>%
  mutate(
    type = recode(
      type,
      handlinger = "Handlinger",
      tidskrav = "Tidskrav"
    )
  )

figur_4 <-
  ggplot(
    koloskopi_long,
    aes(
      x = foretak_kort,
      y = antall,
      fill = type
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  geom_text(
    aes(
      label = antall
    ),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 4.1,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Handlinger" = "#4C78A8",
      "Tidskrav" = "#72B7B2"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 16, 2),
    limits = c(0, 17),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Antall krav",
    fill = NULL
  ) +
  tema_fig

figur_4

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 4 koloskopi.png"
  ),
  plot = figur_4,
  width = 170,
  height = 95,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    figurmappe,
    "2026-10-02 Figur 4 koloskopi.pdf"
  ),
  plot = figur_4,
  width = 170,
  height = 95,
  units = "mm"
)


# Tilleggsfigur: Lokale varianter per behandling

tilleggsfigur_lokale_varianter <-
  lokale_varianter_behandling %>%
  mutate(
    behandling = fct_reorder(
      behandling,
      lokale_varianter
    )
  ) %>%
  ggplot(
    aes(
      x = lokale_varianter,
      y = behandling
    )
  ) +
  geom_col(
    width = 0.7,
    fill = "#72B7B2"
  ) +
  geom_text(
    aes(
      label = lokale_varianter
    ),
    hjust = -0.2,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = 0:5,
    limits = c(0, 5.4),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Antall lokale varianter",
    y = NULL
  ) +
  tema_fig +
  theme(
    panel.grid.major.y = element_blank()
  )

tilleggsfigur_lokale_varianter


# Session information

sessionInfo()
