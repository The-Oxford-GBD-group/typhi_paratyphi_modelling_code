#' get color list
#'
#' @description Gets hex values for colors to be used in maps based on `color_scheme`
#'
#' @param color_scheme character string of desired map coloring.
#' options: `classic`, `darker_middle`, `red_blue`, `carto_red_blue`
#'
#' @return returns a list of color hex values
#'
#' @export
get_color_list <- function(color_scheme) {
  # Set up the color scale
  if (color_scheme == "classic") {
    color_list <- c(
      "#a50026", "#d73027", "#f46d43", "#fdae61",
      "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9",
      "#74add1", "#4575b4", "#313695"
    )
  } else if (color_scheme == "darker_middle") {
    color_list <- c(
      "#A50026", "#960633", "#880D41", "#79144F",
      "#6B1B5D", "#5C216B", "#4E2879", "#3F2F87",
      "#313695"
    )
  } else if (color_scheme == "red_blue") {
    color_list <- c(
      "#A50026", "#B22E3C", "#C05D52", "#CD8B69",
      "#DBBA7F", "#E9E996", "#C4C595", "#9FA195",
      "#7A7D95", "#555995", "#313695"
    )
  } else if (color_scheme == "carto_red_blue") {
    color_list <- c(
      "#008080", "#70a494", "#b4c8a8", "#f6edbd",
      "#edbb8a", "#de8a5a", "#ca562c"
    )
  } else if (color_scheme %in% rownames(brewer.pal.info[brewer.pal.info$category == "seq", ])) {
    color_list <- rev(brewer.pal(9, color_scheme)[-1])
  } else {
    stop(paste(color_scheme, "is not a recognized color scheme"))
  }

  return(color_list)
}
