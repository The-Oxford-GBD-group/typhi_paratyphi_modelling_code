#' Data Scatter Height Fix
#'
#' @description Fixes the height of the rows in the data scatterplot and combines
#' underlying table and data scatter into a grob object
#'
#' @param g_data_scatter_list list outputted by `build_g_data_scatter()`
#' @param df_graph data.table outputted by `dcp_make_df_graph()`
#' @param stage3 data.table of stage 3 countries to be removed
#' @param stage_3_gray boolean. If true, removes stage 3 countries from scatterplot
#'
#' @return returns a grob object with combined table and scatterplot
#'
#' @export
fix_g_data_scatter <- function(g_data_scatter_list, df_graph, stage3, stage_3_gray) {

  # This function takes the data map objects and makes it
  # so that all of the heights are evenly distributed
  # returns a grob object instead of a ggplot object
  if (stage_3_gray) {
    df_graph <- df_graph[!(country %in% stage3$iso3), ]
  }

  height_fix <- function(g_data_scatter) {
    g_data_scatter <- ggplotGrob(g_data_scatter)

    # Figure out # rows per region
    regions_table <- unique(df_graph[, c("country", "region_name")])
    regions_table <- as.data.frame(table(regions_table$region_name))

    names(regions_table) <- c("region_name", "n")

    # This is a bit weird
    # identify the indices of the relevant heights from the grob layout
    idx <- g_data_scatter$layout$b[grepl("panel", g_data_scatter$layout$name)]

    # reset heights (relative heights, scaled to # rows)
    g_data_scatter$heights[idx] <- unit(regions_table$n, "null")

    return(g_data_scatter)
  }

  # Fix heights
  g_data_fixed_list <- lapply(g_data_scatter_list, height_fix)

  g_data_combined <- cbind(g_data_fixed_list[[1]], g_data_fixed_list[[2]], size = "last")
  # Get index of the 2nd set of panels
  idx <- g_data_combined$layout$l[grepl("panel", g_data_combined$layout$name)] %>% unique()
  g_data_combined$widths[idx[1]] <- unit(4, "null")
  g_data_combined$widths[idx[2]] <- unit(1.2, "null")

  return(g_data_combined)
}
