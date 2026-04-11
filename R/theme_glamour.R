# theme_glamour.R
# Core ggplot2 theme implementing Glamour of Graphics principles.

#' Glamour ggplot2 Theme
#'
#' A complete ggplot2 theme built around *The Glamour of Graphics* principles:
#' - No borders or heavy gridlines
#' - Intentional typography with size/weight hierarchy
#' - Left-aligned titles
#' - Markdown-enabled title and subtitle (via `ggtext`)
#' - Generous white space
#'
#' @param palette Character. Name of a glamour palette (see [glamour_palette()]).
#'   Defaults to `"warm"`. Alternatively, supply a custom named vector with
#'   at least `"background"` and `"ink"` entries via the `colors` argument.
#' @param colors Named character vector. Override individual palette colors.
#'   Names must be a subset of `c("background", "ink", "accent1", "accent2",
#'   "accent3", "muted")`. Merged on top of `palette`.
#' @param font_serif Character. Font family name for body text (titles, labels).
#'   Defaults to `glamour_fonts()["serif"]` (i.e., `"glamour_serif"` after
#'   [load_glamour_fonts()] has been called).
#' @param font_sans Character. Font family for axis tick labels.
#'   Defaults to `glamour_fonts()["sans"]`.
#' @param base_size Numeric. Base font size in pt. Defaults to `12`.
#' @param legend Character or `NULL`. Legend position passed to
#'   `theme(legend.position = ...)`. Common values: `"none"`, `"bottom"`,
#'   `"right"`, `"top"`. Defaults to `"none"` (prefer direct labels per
#'   Glamour of Graphics).
#' @param ticks Logical. Whether to show axis ticks. Defaults to `FALSE`.
#' @param grid Character. Which grid lines to show. One of:
#'   `"none"` (default), `"x"` (vertical), `"y"` (horizontal), `"both"`.
#' @param markdown_title Logical. Render the plot title as markdown via
#'   `ggtext::element_markdown()`. Defaults to `TRUE`.
#' @param markdown_subtitle Logical. Render the subtitle as markdown.
#'   Defaults to `TRUE`.
#' @param yscale_percent Logical or numeric. If `TRUE`, formats y-axis labels
#'   as percentages (multiplying by 100). If a number, passes that as the
#'   `scale` argument to `scales::label_percent()` (e.g., `1` when values are
#'   already 0–1). If `FALSE` (default), no y rescaling is applied.
#'
#' @return A ggplot2 `theme` object (and optionally a `scale_y_continuous`
#'   object concatenated via `+` into a list returned as a `glamour_theme`
#'   S3 object that can be added to a ggplot with `+`).
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' load_glamour_fonts()
#'
#' ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = glamour_palette("warm")["accent1"]) +
#'   labs(
#'     title = color_label("Heavier cars", glamour_palette("warm")["accent1"]) |>
#'               paste("get fewer miles per gallon"),
#'     subtitle = "Each point is one car from the mtcars dataset"
#'   ) +
#'   theme_glamour()
#' }
theme_glamour <- function(
    palette           = "warm",
    colors            = NULL,
    font_serif        = glamour_fonts()[["serif"]],
    font_sans         = glamour_fonts()[["sans"]],
    base_size         = 12,
    legend            = "none",
    ticks             = FALSE,
    grid              = "none",
    markdown_title    = TRUE,
    markdown_subtitle = TRUE,
    yscale_percent    = FALSE
) {
  # Resolve palette colors, allowing caller overrides
  pal <- glamour_palette(palette)
  if (!is.null(colors)) {
    pal[names(colors)] <- colors
  }

  bg  <- pal[["background"]]
  ink <- pal[["ink"]]

  # ------------------------------------------------------------------
  # Grid line elements
  # ------------------------------------------------------------------
  grid_major_x <- if (grid %in% c("x", "both")) {
    ggplot2::element_line(color = scales::alpha(ink, 0.12), linewidth = 0.3)
  } else {
    ggplot2::element_blank()
  }
  grid_major_y <- if (grid %in% c("y", "both")) {
    ggplot2::element_line(color = scales::alpha(ink, 0.12), linewidth = 0.3)
  } else {
    ggplot2::element_blank()
  }

  # ------------------------------------------------------------------
  # Tick marks
  # ------------------------------------------------------------------
  tick_length <- if (ticks) ggplot2::unit(4, "pt") else ggplot2::unit(0, "pt")

  # ------------------------------------------------------------------
  # Title / subtitle elements (plain or markdown)
  # ------------------------------------------------------------------
  title_el <- if (markdown_title) {
    ggtext::element_markdown(
      family = font_serif,
      size   = base_size * 1.4,
      face   = "bold",
      color  = ink,
      margin = ggplot2::margin(b = 4)
    )
  } else {
    ggplot2::element_text(
      family = font_serif,
      size   = base_size * 1.4,
      face   = "bold",
      color  = ink,
      margin = ggplot2::margin(b = 4)
    )
  }

  subtitle_el <- if (markdown_subtitle) {
    ggtext::element_markdown(
      family = font_sans,
      size   = base_size * 1.05,
      color  = scales::alpha(ink, 0.7),
      margin = ggplot2::margin(b = 10)
    )
  } else {
    ggplot2::element_text(
      family = font_sans,
      size   = base_size * 1.05,
      color  = scales::alpha(ink, 0.7),
      margin = ggplot2::margin(b = 10)
    )
  }

  # ------------------------------------------------------------------
  # Build the theme
  # ------------------------------------------------------------------
  t <- ggplot2::theme_minimal(base_size = base_size, base_family = font_sans) +
    ggplot2::theme(
      # Backgrounds — all unified to palette background
      plot.background   = ggplot2::element_rect(fill = bg,  color = NA),
      panel.background  = ggplot2::element_rect(fill = bg,  color = NA),
      legend.background = ggplot2::element_rect(fill = bg,  color = NA),
      legend.key        = ggplot2::element_rect(fill = bg,  color = NA),

      # Remove panel border
      panel.border      = ggplot2::element_blank(),

      # Grid lines
      panel.grid.major.x = grid_major_x,
      panel.grid.major.y = grid_major_y,
      panel.grid.minor   = ggplot2::element_blank(),

      # Axis ticks
      axis.ticks        = ggplot2::element_line(color = scales::alpha(ink, 0.3)),
      axis.ticks.length = tick_length,

      # Axis text — contrasting sans for legibility
      axis.text         = ggplot2::element_text(
        family = font_sans,
        size   = base_size * 0.85,
        color  = scales::alpha(ink, 0.7)
      ),
      axis.title        = ggplot2::element_text(
        family = font_sans,
        size   = base_size * 0.9,
        color  = scales::alpha(ink, 0.85),
        margin = ggplot2::margin(t = 4, r = 4)
      ),

      # Titles
      plot.title          = title_el,
      plot.subtitle       = subtitle_el,
      plot.title.position = "plot",           # left-align to panel edge
      plot.caption        = ggplot2::element_text(
        family = font_sans,
        size   = base_size * 0.75,
        color  = scales::alpha(ink, 0.5),
        hjust  = 1,
        margin = ggplot2::margin(t = 8)
      ),
      plot.caption.position = "plot",

      # Generous white space
      plot.margin = ggplot2::margin(20, 20, 16, 20, unit = "pt"),

      # Legend
      legend.position   = legend,
      legend.title      = ggplot2::element_text(
        family = font_sans,
        size   = base_size * 0.85,
        color  = ink,
        face   = "bold"
      ),
      legend.text       = ggplot2::element_text(
        family = font_sans,
        size   = base_size * 0.85,
        color  = ink
      ),

      # Strip labels (facets)
      strip.text = ggplot2::element_text(
        family = font_serif,
        size   = base_size * 0.9,
        color  = ink,
        face   = "bold",
        margin = ggplot2::margin(b = 6, t = 6)
      ),
      strip.background = ggplot2::element_blank()
    )

  # ------------------------------------------------------------------
  # Optional y-axis percent formatting
  # ------------------------------------------------------------------
  if (!isFALSE(yscale_percent)) {
    scale_val <- if (isTRUE(yscale_percent)) 100 else yscale_percent
    list(t, ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = scale_val)
    ))
  } else {
    t
  }
}
