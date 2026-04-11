# .Rprofile — runs automatically when R starts in this project directory.
# Load all package functions in development mode and initialize fonts.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
}

# Load and register Google Fonts for this session.
# Comment out if working offline or fonts are already registered.
if (exists("load_glamour_fonts")) {
  load_glamour_fonts()
}
