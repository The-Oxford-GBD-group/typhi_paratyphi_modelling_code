#' Combine map objects
#'
#' @description Combines all elements into the final data coverage plot
#'
#' @param g_datamap data scatterplot, grob output of `make_data_scatterplots()`
#' @param g_data_legend data scatterplot legend, grob output of `make_data_scatterplots()`
#' @param map_list list of 4 period maps, outputted by `make_a_period_map()`
#' @param n_countries numeric, number of countries being mapped
#' @param reg_title title of the region being mapped
#' @param title title of data coverage plot
#' @param base_font_size font size. All font sizes in the plots are scaled off this
#' @param n_total numeric, total number of rows in dataset
#' @param polys_total numeric, total number of polygons
#' @param points_total numeric, total number of points
#'
#' @return returns a complete data coverage plot
#'
#' @export
dcp_make_4up_map <- function(g_datamap,
                             g_data_legend,
                             map_list,
                             n_countries,
                             reg_title,
                             title,
                             base_font_size,
                             n_total,
                             polys_total,
                             points_total) {

  # g_datamap = object for left side of map (lets you add new data map

  # grab your legends using the predefined functions, then state their grid location
  p.legend <- gLegend(map_list[[1]])
  p.legend$vp <- grid::viewport(layout.pos.row = 1:12, layout.pos.col = 9)

  # Note g_data_legend grabbed above while processing g_data
  g_data_legend$vp <- grid::viewport(layout.pos.row = 2:11, layout.pos.col = 4)

  # Add a title
  title_grob <- textGrob(paste0(title, ": ", reg_title),
    gp = gpar(fontsize = base_font_size * 1.5)
  )
  title_grob$vp <- grid::viewport(layout.pos.row = 1, layout.pos.col = 1:3)

  time_stamp <- Sys.Date()
  # Add notes at bottom
  note_grob <- textGrob(paste0(
    "N: ", formatC(n_total, format = "d", big.mark = ","), "\n",
    "Points: ", formatC(points_total, format = "d", big.mark = ","), "\n",
    "Polygons: ", formatC(polys_total, format = "d", big.mark = ","),
    "\n",
    format(time_stamp, format = "%m/%d/%Y")
  ),
  gp = gpar(fontsize = base_font_size)
  )
  note_grob$vp <- grid::viewport(layout.pos.row = 11:12, layout.pos.col = 9)

  # Set up based on number of countries
  if (n_countries > 24) g_datamap$vp <- grid::viewport(layout.pos.row = 2:11, layout.pos.col = 1:3)
  if (n_countries > 12 & n_countries <= 24) g_datamap$vp <- grid::viewport(layout.pos.row = 3:10, layout.pos.col = 1:3)
  if (n_countries <= 12) g_datamap$vp <- grid::viewport(layout.pos.row = 4:9, layout.pos.col = 1:3)

  # Initialize plot with master title
  grid.newpage()
  pushViewport(grid::viewport(layout = grid.layout(nrow = 12, ncol = 9)))
  vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, clip = "off")
  # Plot all data coverage maps
  # print(tbl, as.table=TRUE)
  grid.draw(g_datamap) # Note now a grob, so grid.draw
  print(map_list[[1]] + theme(legend.position = "none"), vp = vplayout(1:6, 5:6))
  print(map_list[[2]] + theme(legend.position = "none"), vp = vplayout(1:6, 7:8))
  print(map_list[[3]] + theme(legend.position = "none"), vp = vplayout(7:12, 5:6))
  print(map_list[[4]] + theme(legend.position = "none"), vp = vplayout(7:12, 7:8))
  grid.draw(p.legend)
  grid.draw(title_grob)
  grid.draw(note_grob)
  # If there are no data points, then the g_data_legend can fail
  #  Try plotting it without recording first to make sure it works
  plot_g_data_legend <- tryCatch({
    grid.draw(g_data_legend, recording = FALSE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
  if (plot_g_data_legend) grid.draw(g_data_legend, recording = TRUE)
}
