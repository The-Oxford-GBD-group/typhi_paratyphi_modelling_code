#' Build data scatterplot
#'
#' @description Builds the data scatterplot and underlying table on the left hand side of coverage plots
#'
#' @param df_graph data.table outputted by `dcp_make_df_graph()`
#' @param alpha_val number between 0 and 1 controlling transparency of points
#' @param by_color name of column in df_graph defining color groupings in plot
#' @param color_label title of color portion of the scatterplot legend
#' @param color_scheme_scatter a character string specifying different color schemes
#' options: `brewer`, `binary`, `carto1`, `carto2`, `carto3`
#' @param size_lab title of the size portion of the scatterplot legend
#' @param title title of the data coverage plot
#' @param reg_title name of the region being mapped
#' @param region_name column in df_graph that has the region names for the plot
#' @param table_data data.table of point and polygon counts by country
#' from `dcp_make_table_new()`
#' @param base_font_size font size. All font sizes in the plots are scaled off this
#' @param year_min minimum year, controls range of years shown in scatterplot
#' @param year_max maximum year, controls range of years shown in scatterplot
#' @param stage3 data.table of stage 3 countries to be removed
#' @param stage_3_gray boolean. If true, removes stage 3 countries from scatterplot
#'
#' @return returns a list of 2 objects -
#' * 'g_datamap': scatterplot ggplot object
#' * 'g_table': underlying table ggplot object
#'
#' @export
build_g_data_scatter <- function(df_graph,
                                 alpha_val,
                                 by_color,
                                 color_label,
                                 color_scheme_scatter,
                                 size_lab,
                                 title,
                                 reg_title,
                                 region_name,
                                 table_data,
                                 base_font_size,
                                 year_min,
                                 year_max,
                                 stage3,
                                 stage_3_gray) {

  # Set up table data
  td <- copy(table_data)
  td[, N := NULL] # Don't display N column
  setnames(td, "Country", "country")
  td <- gather(td, type, value, -country) %>% as.data.table()

  # Add on regions
  reg_table <- subset(df_graph, select = c("country", "location_name", "region_name")) %>%
    unique()
  td <- merge(td, reg_table, by = "country")

  if (stage_3_gray) {
    td <- td[!(country %in% stage3$iso3), ]
    df_graph <- df_graph[!(country %in% stage3$iso3), ]
  }

  base_font_size <- round(base_font_size * 0.8)

  panel_spacing <- 2
  if (region_name %in% c("africa", "africa_no_yem")) panel_spacing <- 1
  if (region_name == "stage2") panel_spacing <- .5


  # Make the table part of the plot
  # Note: weird 5/14 scalar from here: https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
  g_table <- ggplot(
    data = td,
    aes(
      x = type,
      y = location_name,
      label = value
    )
  ) +
    geom_text(size = base_font_size * (5 / 14) * (0.8)) +
    theme_minimal(base_size = base_font_size) + #
    scale_x_discrete(position = "top") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = rel(1)),
      panel.spacing.y = unit(panel_spacing, "lines")
    ) +
    facet_wrap(~region_name, scales = "free_y", ncol = 1)

  # Builds a data scatterplot
  g_datamap <- ggplot(
    data = df_graph,
    aes(
      x = year,
      y = location_name,
      group = get(by_color)
    )
  ) +
    geom_vline(xintercept = c(2000, 2005, 2010, 2015)) +
    geom_point(aes(
      size = capped_n,
      shape = pointpoly,
      color = get(by_color)
    ),
    alpha = alpha_val
    ) +
    scale_size_area(limits = c(0, max(df_graph$capped_n[!is.na(df_graph$capped_n)])), max_size = 4) +
    theme_minimal(base_size = base_font_size) +
    xlim(year_min, year_max) +
    labs(
      shape = "Data Type",
      color = color_label,
      size = size_lab
    ) +
    guides(
      color = guide_legend(order = 1, override.aes = list(shape = 15, size = 5)),
      shape = guide_legend(order = 2, override.aes = list(size = 4)),
      size = guide_legend(order = 3)
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text.x = element_text(
        size = rel(1.3),
        vjust = 0.5,
        margin = margin(b = 20)
      ),
      panel.spacing.y = unit(panel_spacing, "lines"),
      plot.margin = margin(l = 12)
    ) +
    facet_wrap(~region_name, scales = "free_y", ncol = 1)

  g_datamap <- add_color_scheme_scatter(g_datamap, color_scheme_scatter)

  return(list(g_datamap, g_table))
}
