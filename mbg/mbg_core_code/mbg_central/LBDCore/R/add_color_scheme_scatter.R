#' Add color scheme to scatterplot
#'
#' @description Defines the color scheme used in the data scatterplot
#'
#' @param g_datamap a ggplot data scatter built in `build_g_data_scatter()`
#' @param color_scheme_scatter a character string specifying different color schemes
#' options: `brewer`, `binary`, `carto1`, `carto2`, `carto3`
#'
#' @return g_datamap with a color scale applied
#'
#' @export
add_color_scheme_scatter <- function(g_datamap, color_scheme_scatter) {
  if (color_scheme_scatter == "brewer") {
    g_datamap <- g_datamap + scale_color_brewer(palette = "Paired")
  } else if (color_scheme_scatter == "binary") {
    g_datamap <- g_datamap + scale_colour_manual(values = c("No" = "gray", "Yes" = "firebrick"))
  } else if (color_scheme_scatter == "carto1") {
    g_datamap <- g_datamap + scale_colour_manual(values = c(
      "#5F4690", "#1D6996", "#38A6A5", "#0F8554",
      "#73AF48", "#EDAD08", "#E17C05", "#CC503E",
      "#94346E", "#6F4070", "#994E95", "#666666"
    ))
  } else if (color_scheme_scatter == "carto2") {
    g_datamap <- g_datamap + scale_colour_manual(values = c(
      "#7F3C8D", "#11A579", "#3969AC", "#F2B701",
      "#E73F74", "#80BA5A", "#E68310", "#008695",
      "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99"
    ))
  } else if (color_scheme_scatter == "carto3") {
    g_datamap <- g_datamap + scale_colour_manual(values = c(
      "#E58606", "#5D69B1", "#52BCA3", "#99C945",
      "#CC61B0", "#24796C", "#DAA51B", "#2F8AC4",
      "#764E9F", "#ED645A", "#CC3A8E", "#A5AA99"
    ))
  }
}
