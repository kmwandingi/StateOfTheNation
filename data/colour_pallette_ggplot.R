# ergo corporate colors--------------
ergo_colors <- c(
  `dark`  = "#4a4e4d ",
  `green`      = "#019c86",
  `blue green` = "#0297a0",
  `blue`       = "#04354b",
  `red` = "#96384e",
  `orange`        = "#eda48e",
  `yellow`  = "#eed284"
)

#' Function to extract ergo colors as hex codes
#'
#' @param ... Character names of ergo_colors 
#'
ergo_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ergo_colors)
  
  ergo_colors[cols]
}

#combine colors into palletes

ergo_palettes <- list(
  `main`  = ergo_cols("blue", "green", "blue green"),
  
  `cool`  = ergo_cols("blue", "green", "blue green"),
  
  `dark`   = ergo_cols("dark","blue"),
  
  `mixed` = ergo_cols("green", "blue","red", "orange", "yellow"),
  
  `light`  = ergo_cols("orange", "yellow")
)

#' Return function to interpolate a ergo color palette
#'
#' @param palette Character name of palette in ergo_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
ergo_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ergo_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#Scales for ggplot2

#' Color scale constructor for ergo colors
#'
#' @param palette Character name of palette in ergo_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_ergo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ergo_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ergo_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for ergo colors
#'
#' @param palette Character name of palette in ergo_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_ergo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ergo_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ergo_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}