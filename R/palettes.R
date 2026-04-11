# palettes.R
# Named color palettes and ggplot2 scale wrappers for glamour-style graphics.
#
# Each palette is a named character vector so individual colors can be
# referenced by role (e.g., glamour_palettes$warm["background"]).

# ---------------------------------------------------------------------------
# Internal palette registry
# ---------------------------------------------------------------------------

.glamour_palettes <- list(

  #' Warm Ivory — light background, dark ink, earthy accents
  #' Good for: white/ivory backgrounds, editorial feel
  warm = c(
    background = "#FFEEDD",
    ink        = "#141204",
    accent1    = "#1B512D",   # forest green
    accent2    = "#967AA1",   # muted purple
    accent3    = "#C1666B",   # terracotta
    muted      = "#B8A99A"    # warm grey
  ),

  #' Slate — light cool-grey background
  #' Good for: clean, minimal presentation
  slate = c(
    background = "#F2F4F5",
    ink        = "#1A1E23",
    accent1    = "#2E6DA4",   # steel blue
    accent2    = "#E07B39",   # burnt orange
    accent3    = "#4B8C6F",   # sage
    muted      = "#9DAAB3"    # cool grey
  ),

  #' Midnight — dark background, light ink
  #' Good for: dark-mode / presentation slides
  midnight = c(
    background = "#1A1B2E",
    ink        = "#F8F7FF",
    accent1    = "#E94560",   # vivid red-pink
    accent2    = "#0F3460",   # deep navy (use for secondary bg)
    accent3    = "#A8DADC",   # powder blue
    muted      = "#6B7280"    # muted grey
  )
)


# ---------------------------------------------------------------------------
# Public accessors
# ---------------------------------------------------------------------------

#' Retrieve a Named Glamour Palette
#'
#' Returns a named character vector of hex colors for the requested palette.
#'
#' @param name Character. One of `"warm"` (default), `"slate"`, `"midnight"`.
#'
#' @return A named character vector of hex color codes.
#' @export
#'
#' @examples
#' glamour_palette("warm")
#' glamour_palette("warm")["accent1"]
glamour_palette <- function(name = "warm") {
  choices <- names(.glamour_palettes)
  if (!name %in% choices) {
    stop(sprintf("Palette '%s' not found. Choose from: %s",
                 name, paste(choices, collapse = ", ")))
  }
  .glamour_palettes[[name]]
}


#' List Available Glamour Palettes
#'
#' @return A character vector of palette names.
#' @export
glamour_palette_names <- function() names(.glamour_palettes)


# ---------------------------------------------------------------------------
# ggplot2 scale wrappers
# ---------------------------------------------------------------------------

#' Discrete Color Scale Using a Glamour Palette
#'
#' Maps accent colors from a glamour palette to a discrete aesthetic.
#'
#' @param palette Character. Palette name passed to [glamour_palette()].
#'   Defaults to `"warm"`.
#' @param accents Integer vector. Indices of palette accent slots to use,
#'   e.g., `c(1, 2, 3)`. Defaults to `1:3`.
#' @param ... Additional arguments passed to [ggplot2::scale_color_manual()].
#'
#' @return A ggplot2 scale object.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(hp, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_glamour("warm")
#' }
scale_color_glamour <- function(palette = "warm", accents = 1:3, ...) {
  pal <- glamour_palette(palette)
  accent_names <- paste0("accent", accents)
  cols <- pal[accent_names]
  ggplot2::scale_color_manual(values = unname(cols), ...)
}

#' @rdname scale_color_glamour
#' @export
scale_colour_glamour <- scale_color_glamour


#' Discrete Fill Scale Using a Glamour Palette
#'
#' @inheritParams scale_color_glamour
#' @return A ggplot2 scale object.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_glamour("slate")
#' }
scale_fill_glamour <- function(palette = "warm", accents = 1:3, ...) {
  pal <- glamour_palette(palette)
  accent_names <- paste0("accent", accents)
  cols <- pal[accent_names]
  ggplot2::scale_fill_manual(values = unname(cols), ...)
}
