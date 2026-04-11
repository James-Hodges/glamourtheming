# utils.R
# Miscellaneous helper utilities for the glamour theming workflow.

#' Preview a Glamour Palette as a Plot
#'
#' Renders a horizontal swatch plot of all colors in a palette, labelled
#' with their role names and hex codes.
#'
#' @param palette Character. Palette name passed to [glamour_palette()].
#'   Defaults to `"warm"`.
#'
#' @return A `ggplot` object (invisibly). Side effect: prints the plot.
#' @export
#'
#' @examples
#' show_palette("warm")
#' show_palette("midnight")
show_palette <- function(palette = "warm") {
  pal    <- glamour_palette(palette)
  n      <- length(pal)
  roles  <- names(pal)
  labels <- paste0(roles, "\n", pal)

  # Determine a readable text color per swatch using relative luminance
  text_cols <- vapply(pal, function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    lum <- 0.2126 * rgb[1] + 0.7152 * rgb[2] + 0.0722 * rgb[3]
    if (lum > 0.45) "#1A1A1A" else "#F8F7FF"
  }, character(1))

  df <- data.frame(
    x      = seq_len(n),
    label  = labels,
    fill   = pal,
    text_c = text_cols,
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = 1, fill = I(fill))) +
    ggplot2::geom_tile(width = 0.95, height = 0.95) +
    ggplot2::geom_text(ggplot2::aes(label = label, color = I(text_c)),
                       size = 3, lineheight = 1.3) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::labs(title = paste0("Glamour palette: ", palette),
                  x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 8)),
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )

  print(p)
  invisible(p)
}


#' Preview All Glamour Palettes
#'
#' Calls [show_palette()] for every registered palette.
#'
#' @return Invisibly returns a named list of ggplot objects.
#' @export
show_all_palettes <- function() {
  plots <- lapply(glamour_palette_names(), function(nm) show_palette(nm))
  names(plots) <- glamour_palette_names()
  invisible(plots)
}


#' Save a ggplot with Glamour Defaults
#'
#' A thin wrapper around [ggplot2::ggsave()] with sensible defaults for
#' high-quality output: 300 dpi, 8×5 inches, PNG.
#'
#' @param plot A ggplot object. Defaults to the last plot (`ggplot2::last_plot()`).
#' @param filename Character. Output file path. Defaults to `"plot.png"`.
#' @param width Numeric. Width in inches. Defaults to `8`.
#' @param height Numeric. Height in inches. Defaults to `5`.
#' @param dpi Numeric. Resolution. Defaults to `300`.
#' @param device Character. Output device. Defaults to `"png"`.
#' @param bg Character. Background color. Defaults to `"transparent"` so the
#'   plot background set in the theme shows through.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return Invisibly returns the `filename`.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- ggplot(mtcars, aes(hp, mpg)) + geom_point() + theme_glamour()
#' glamour_save(p, "output/hp_vs_mpg.png")
#' }
glamour_save <- function(
    plot     = ggplot2::last_plot(),
    filename = "plot.png",
    width    = 8,
    height   = 5,
    dpi      = 300,
    device   = "png",
    bg       = "transparent",
    ...
) {
  ggplot2::ggsave(
    filename = filename,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    device   = device,
    bg       = bg,
    ...
  )
  message("Saved: ", filename)
  invisible(filename)
}
