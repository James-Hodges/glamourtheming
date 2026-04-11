# typography.R
# Helpers for loading and managing fonts for glamour-style graphics.
#
# Workflow:
#   Call load_glamour_fonts() once per session (or in .Rprofile).
#   Then call showtext::showtext_auto() to have showtext intercept all
#   graphics devices automatically.

#' Load Glamour Fonts from Google Fonts
#'
#' Registers a serif body font and a contrasting sans-serif font via
#' `sysfonts::font_add_google()` and activates `showtext` rendering.
#' Call this once at the top of a script or in `.Rprofile`.
#'
#' @param serif_family Character. Family name to register for the serif font.
#'   Defaults to `"glamour_serif"`.
#' @param sans_family Character. Family name to register for the sans-serif font.
#'   Defaults to `"glamour_sans"`.
#' @param serif_google Character. Google Fonts name for the serif font.
#'   Defaults to `"Source Serif 4"`.
#' @param sans_google Character. Google Fonts name for the sans-serif font.
#'   Defaults to `"DM Sans"`.
#' @param auto Logical. Whether to call `showtext::showtext_auto()` after
#'   loading fonts. Defaults to `TRUE`.
#'
#' @return Invisibly returns a named list with the registered family names.
#' @export
#'
#' @examples
#' \dontrun{
#' load_glamour_fonts()
#' # Then use family = "glamour_serif" in theme_glamour()
#' }
load_glamour_fonts <- function(
    serif_family  = "glamour_serif",
    sans_family   = "glamour_sans",
    serif_google  = "Source Serif 4",
    sans_google   = "DM Sans",
    auto          = TRUE
) {
  if (!requireNamespace("sysfonts",  quietly = TRUE)) stop("Install 'sysfonts'.")
  if (!requireNamespace("showtext",  quietly = TRUE)) stop("Install 'showtext'.")

  sysfonts::font_add_google(serif_google, serif_family)
  sysfonts::font_add_google(sans_google,  sans_family)

  if (auto) showtext::showtext_auto()

  message(sprintf(
    "Glamour fonts loaded:\n  serif  -> '%s' (%s)\n  sans   -> '%s' (%s)",
    serif_family, serif_google,
    sans_family,  sans_google
  ))

  invisible(list(serif = serif_family, sans = sans_family))
}


#' Glamour Font Family Names
#'
#' Returns the registered family names set by [load_glamour_fonts()].
#' Useful when building theme calls programmatically.
#'
#' @return A named character vector with elements `"serif"` and `"sans"`.
#' @export
#'
#' @examples
#' glamour_fonts()["serif"]
glamour_fonts <- function() {
  c(serif = "glamour_serif", sans = "glamour_sans")
}
