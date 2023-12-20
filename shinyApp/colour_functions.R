
bnssgcols <- c(`white` = "#FFFFFF",`light grey` = "#999999",`light pink` = "#D093B6",`mid pink` = "#8d488d",`light blue` = "#8AC0E5",`dark pink` = "#853358",
               `mid blue` = "#2472AA",`dark blue` = "#003087",`dark grey` = "#333333")

##Tells R Where to look for color codes, from the name
bnssg_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (bnssgcols)
  
  bnssgcols[cols]
}

##Set colour 'names'scheme' names:
bnssg_palettes <- list(
  "main"  = bnssg_cols("light grey", "light pink", "mid pink", "light blue", "mid blue", "dark blue", "dark grey","dark pink"),
  "pgb" = bnssg_cols("dark pink", "mid pink", "light pink", "light grey", "light blue", "mid blue", "dark blue"),
  "pgblite" = bnssg_cols("dark pink", "light grey", "dark blue"),
  "pink" = bnssg_cols("dark pink", "mid pink", "light pink"),
  "blue" = bnssg_cols("dark blue", "mid blue", "light blue"),
  "mapcol" = bnssg_cols("dark blue", "mid blue", "light blue","light grey")
)

##Function so that the colours can be found from the scheme name
bnssg_pal <- function(palette, reverse = FALSE, ...) {
  pal <- bnssg_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_colour_bnssg <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bnssg_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("color", paste0("bnssg_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colors = pal(256), ...)
  }
}

scale_fill_bnssg <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bnssg_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("bnssg_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
