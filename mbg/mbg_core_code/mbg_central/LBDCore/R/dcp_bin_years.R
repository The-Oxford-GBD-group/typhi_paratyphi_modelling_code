#' Bin Years
#'
#' @description Take a data table and bin the year variable
#'
#' @param df_to_bin The subsetted and formatted data.table object output by the
#' dcp_make_df_graph() function split into points or polygons
#'
#' @return A data.table with binned plot_year variable
#'
#' @export
dcp_bin_years <- function(df_to_bin) {
  df_to_bin <- df_to_bin[, survey := paste0(source, "_", country, "_", year)]
  df_to_bin <- subset(df_to_bin, year >= 1998)
  df_to_bin <- df_to_bin[year > 1997 & year <= 2002, plot_year := 2000]
  df_to_bin <- df_to_bin[year > 2002 & year <= 2007, plot_year := 2005]
  df_to_bin <- df_to_bin[year > 2007 & year <= 2012, plot_year := 2010]
  df_to_bin <- df_to_bin[year > 2012, plot_year := 2015]

  return(df_to_bin)
}
