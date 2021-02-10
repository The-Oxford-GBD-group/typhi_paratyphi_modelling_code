#' Build data scatterplot wrapper
#'
#' @description Wrapper for build_g_data_scatter and fix_g_data_scatter to create
#' complete data scatterplot grob objects for all and new data
#'
#' @param df_graph data.table outputted by `dcp_make_df_graph()`
#' @param df_summary data.table outputted by `dcp_make_df_graph()`
#' @param title title of the data coverage plot
#' @param reg_title name of the region being mapped
#' @param year_min minimum year, controls range of years shown in scatterplot
#' @param year_max maximum year, controls range of years shown in scatterplot
#' @param table_data data.table of point and polygon counts by country
#' from `dcp_make_table_new()`
#' @param base_font_size font size. All font sizes in the plots are scaled off this
#' @param region_name column in df_graph that has the region names for the plot
#' @param color_scheme_scatter a character string specifying different color schemes
#' options: `brewer`, `binary`, `carto1`, `carto2`, `carto3`
#' @param stage3 data.table of stage 3 countries to be removed
#' @param stage_3_gray boolean. If true, removes stage 3 countries from scatterplot
#' @param new_data_plots boolean. If true, makes data scatterplots with data marked new from
#' `dcp_find_new_data()`
#'
#' @return returns a list of 4 objects -
#' * 'g_data_legend': grob object, legend of g_data
#' * 'g_data_new_legend': grob object, legend of g_data_new
#' * 'g_data': scatterplot grob object with all data
#' * 'g_data_new': scatterplot grob object with new data
#'
#' @export
make_data_scatterplots <- function(df_graph,
                                   df_summary,
                                   title,
                                   reg_title,
                                   year_min,
                                   year_max,
                                   table_data,
                                   base_font_size,
                                   region_name,
                                   color_scheme_scatter,
                                   stage3,
                                   stage_3_gray,
                                   new_data_plots = FALSE) {

  # Fix title if present
  if (title != "") {
    title <- paste0(title, " ")
  }

  # Set up heading for size (proportional to 'N')
  size_lab <- "Sample Size"

  # Ensure that year_min, year_max are numeric
  year_min <- as.numeric(year_min)
  year_max <- as.numeric(year_max)

  # Cap shape size at percentile
  n_cap_pctile <- 0.9
  df_graph <- df_graph[, capped_n := n]
  df_graph <- df_graph[
    capped_n >= quantile(df_summary$n[!is.na(df_summary$n)], probs = n_cap_pctile),
    capped_n := quantile(df_summary$n[!is.na(df_summary$n)], probs = n_cap_pctile)
  ]

  g_data_list <- build_g_data_scatter(
    df_graph = df_graph,
    alpha_val = 0.9,
    by_color = "source",
    color_label = "Data Source",
    color_scheme_scatter = color_scheme_scatter,
    size_lab = size_lab,
    title = title,
    reg_title = reg_title,
    region_name = region_name,
    table_data = table_data,
    base_font_size = base_font_size,
    year_min = year_min,
    year_max = year_max,
    stage3 = stage3,
    stage_3_gray = stage_3_gray
  )
  # Pull legends and then remove
  g_data_legend <- gLegend(g_data_list[[1]])
  g_data_list[[1]] <- g_data_list[[1]] + theme(legend.position = "none")
  g_data <- fix_g_data_scatter(g_data_list, df_graph, stage3, stage_3_gray)
  message("  Successfully created scatter for main plot.")

  # Optionally make new data scatterplots
  if (new_data_plots) {
    message("  You have chosen to make New Data Plots. Making New Data scatters...")
    g_data_new_list <- build_g_data_scatter(
      df_graph = df_graph,
      alpha_val = 1,
      by_color = "new_data_lab",
      color_label = "New Data",
      color_scheme_scatter = "binary",
      size_lab = size_lab,
      title = title,
      reg_title = reg_title,
      region_name = region_name,
      table_data = table_data,
      base_font_size = base_font_size,
      year_min = year_min,
      year_max = year_max,
      stage3 = stage3,
      stage_3_gray = stage_3_gray
    )
    g_data_new_legend <- gLegend(g_data_new_list[[1]])
    g_data_new_list[[1]] <- g_data_new_list[[1]] + theme(legend.position = "none")
    g_data_new <- fix_g_data_scatter(g_data_new_list, df_graph, stage3, stage_3_gray)
  } else {
    message("  You have chosen not to make New Data Plots. Skipping new data scatters.")
    g_data_new_list <- NA
    g_data_new_legend <- NA
    g_data_new <- NA
  }


  message("Done making scatterplots.")
  return(list(
    "g_data_legend" = g_data_legend,
    "g_data_new_legend" = g_data_new_legend,
    "g_data" = g_data,
    "g_data_new" = g_data_new
  ))
}
