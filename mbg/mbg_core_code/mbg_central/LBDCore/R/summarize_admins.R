#' @title summarize_admins
#' @description Function to summarize admin_pred objects
#'
#' This is a wrapper for `make_admin_pred_summary()`
#'
#' @param ind indicator
#' @param ig indicator_group
#' @param summstats Summary statistics (functions) to compute.
#' Order will be the order they are written in csv
#' This is passed to `make_admin_pred_summary()`
#' @param raked Raked (T), unraked (F), or both (`c(T,F)`)?
#' @param ad_levels Admin levels to summarize (0, 1, and 2 by default)
#' @return Writes csv files to `sharedir/pred_derivatives/admin_summaries/`
#' @examples
#' \dontrun{
#' summarize_admins(
#'   summstats = c("mean", "lower", "upper", "cirange"),
#'   ad_levels = c(0, 1, 2),
#'   raked = c(T, F)
#' )
#' }
#' @rdname summarize_admins
#' @export
summarize_admins <- function(ind = indicator,
                             ig = indicator_group,
                             summstats = c("mean", "lower", "upper", "cirange"),
                             raked = c(T, F),
                             ad_levels = c(0, 1, 2),
                             file_addin = NULL,
                             ...) {
  sharedir <- sprintf("/share/geospatial/mbg/%s/%s", ig, ind)
  input_dir <- paste0(sharedir, "/output/", run_date, "/")
  output_dir <- paste0(input_dir, "/pred_derivatives/admin_summaries/")
  dir.create(output_dir, recursive = T, showWarnings = F)

  # Convert raked to character
  rr <- character()
  if (T %in% raked) rr <- c(rr, "raked")
  if (F %in% raked) rr <- c(rr, "unraked")

  # If file_addin present, use it
  if (!is.null(file_addin)) file_addin <- paste0("_", file_addin)
  if (is.null(file_addin)) file_addin <- ""

  # Summarize and save admin preds
  for (rake in rr) {
    load(paste0(input_dir, ind, "_", rake, "_admin_draws_eb_bin0_0.RData"))
    sp_hierarchy_list <- mutate_if(sp_hierarchy_list, is.factor, as.character)
    sp_hierarchy_list <- mutate_at(sp_hierarchy_list, grep("_CODE", names(sp_hierarchy_list), value = T), as.numeric)

    for (ad in ad_levels) {
      message(paste0("Summarizing ", ind, ": admin ", ad, " (", rake, ")"))
      ad_summary_table <- make_admin_pred_summary(
        admin_pred = get(paste0("admin_", ad)),
        sp_hierarchy_list,
        summary_stats = summstats,
        ...
      )
      fwrite(ad_summary_table,
        file = paste0(output_dir, ind, "_admin_", ad, "_", rake, file_addin, "_summary.csv")
      )
    }
  }
}
