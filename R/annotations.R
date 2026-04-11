# annotations.R
# Helpers for direct-labeling and colored text in titles/subtitles.
#
# The core Glamour of Graphics pattern:
#   Instead of a legend, encode grouping information directly in the title
#   or subtitle using colored spans — rendered by ggtext::element_markdown().
#
# Example:
#   title = glue::glue(
#     "{color_label('SUVs', accent1)} cost more than {color_label('sedans', accent2)}"
#   )

#' Wrap Text in a Colored HTML Span for ggtext Markdown
#'
#' Produces an HTML `<span>` tag suitable for use inside a ggplot title or
#' subtitle rendered by `ggtext::element_markdown()`. Use this to replace
#' a legend with colored text that matches the data encoding.
#'
#' @param text Character. The text to color.
#' @param color Character. A hex color or any CSS color string.
#' @param bold Logical. Whether to bold the text. Defaults to `TRUE`.
#'
#' @return A character string containing an HTML span element.
#' @export
#'
#' @examples
#' color_label("SUVs", "#1B512D")
#' # <span style='color:#1B512D;'><b>SUVs</b></span>
#'
#' # In a ggplot title:
#' \dontrun{
#' pal <- glamour_palette("warm")
#' ggplot(mpg, aes(displ, hwy, color = drv)) +
#'   geom_point() +
#'   labs(title = paste(
#'     color_label("Front-wheel", pal["accent1"]),
#'     "cars are more efficient than",
#'     color_label("rear-wheel", pal["accent2"]),
#'     "cars"
#'   )) +
#'   theme_glamour(legend = "none")
#' }
color_label <- function(text, color, bold = TRUE) {
  if (bold) {
    sprintf("<span style='color:%s;'><b>%s</b></span>", color, text)
  } else {
    sprintf("<span style='color:%s;'>%s</span>", color, text)
  }
}


#' Build a Legend-Replacing Title Snippet
#'
#' Convenience wrapper that takes a named vector of `label -> color` pairs
#' and collapses them into a single markdown string with a separator.
#' Useful for subtitles that list all groups.
#'
#' @param labels Named character vector where names are display labels and
#'   values are hex colors. E.g., `c("Group A" = "#1B512D", "Group B" = "#967AA1")`.
#' @param sep Character. Separator between labels. Defaults to `"  "` (two
#'   spaces — gives visual breathing room in a title).
#' @param bold Logical. Whether to bold each label. Defaults to `TRUE`.
#'
#' @return A single character string of HTML spans separated by `sep`.
#' @export
#'
#' @examples
#' pal <- glamour_palette("warm")
#' color_legend(c("SUVs" = pal["accent1"], "Sedans" = pal["accent2"]))
color_legend <- function(labels, sep = "  \u2022  ", bold = TRUE) {
  spans <- mapply(color_label, text = names(labels), color = unname(labels),
                  MoreArgs = list(bold = bold), SIMPLIFY = TRUE)
  paste(spans, collapse = sep)
}


#' Add a Direct Data Label Layer
#'
#' A thin wrapper around `ggplot2::geom_text()` with glamour-friendly defaults:
#' no background, nudge, and inheriting the color aesthetic so labels match
#' their data series. For non-overlapping labels consider `ggrepel::geom_text_repel()`.
#'
#' @param mapping Additional aesthetic mappings passed to `geom_text()`.
#' @param nudge_x,nudge_y Numeric nudge values. Defaults to `0` and `0.02`.
#' @param size Numeric. Text size in pt / ggplot size units. Defaults to `3`.
#' @param family Character. Font family. Defaults to `glamour_fonts()["sans"]`.
#' @param fontface Character. Font face. Defaults to `"bold"`.
#' @param ... Additional arguments passed to `ggplot2::geom_text()`.
#'
#' @return A ggplot2 layer.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(hp, mpg, label = rownames(mtcars))) +
#'   geom_point() +
#'   glamour_label()
#' }
glamour_label <- function(
    mapping  = NULL,
    nudge_x  = 0,
    nudge_y  = 0.02,
    size     = 3,
    family   = glamour_fonts()[["sans"]],
    fontface = "bold",
    ...
) {
  ggplot2::geom_text(
    mapping  = mapping,
    nudge_x  = nudge_x,
    nudge_y  = nudge_y,
    size     = size,
    family   = family,
    fontface = fontface,
    ...
  )
}
