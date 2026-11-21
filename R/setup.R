# Packages, palette and table/plot defaults shared by every page.

library(tidyverse)
library(kableExtra)
library(knitr)
library(scales)
library(cli)

theme_set(theme_minimal(base_size = 13))

BRAND_PRIMARY   <- "#1A1A2E"
BRAND_ACCENT    <- "#0F3460"   # FRR / LOOSEN
BRAND_HIGHLIGHT <- "#E94560"   # FAR / TIGHTEN
BRAND_GREY      <- "#6E6E73"   # EER marker
HOLD_GREEN      <- "#2E7D32"
SIGNAL_AMBER    <- "#B26A00"   # IMPROVE SIGNAL

COLOURS <- list(
  TIGHTEN          = BRAND_HIGHLIGHT,
  LOOSEN           = BRAND_ACCENT,
  HOLD             = HOLD_GREEN,
  `IMPROVE SIGNAL` = SIGNAL_AMBER
)

# Text and background colours for action cells in tables
ACTION_CELL <- tribble(
  ~action,          ~text,     ~background,
  "TIGHTEN",        "#9C1F36", "#fbe6ea",
  "LOOSEN",         "#0F3460", "#e6eaf1",
  "HOLD",           "#2E7D32", "#e8f5ea",
  "IMPROVE SIGNAL", "#8A5200", "#fdf0dc"
)

pct <- function(x, accuracy = 0.1) percent(x, accuracy = accuracy)

kable_house <- function(df, col.names = names(df), escape = TRUE, align = NULL) {
  kable(df, format = "html", col.names = col.names, escape = escape, align = align) |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width        = TRUE,
      position          = "left",
      font_size         = 14
    )
}

action_cell <- function(action, label = action) {
  style <- tibble(action = action) |> left_join(ACTION_CELL, by = join_by(action))
  cell_spec(label, format = "html", bold = TRUE, color = style$text, background = style$background)
}
