#' Find new data for new data coverage plots
#'
#' @description Take a df_summary object and check to see if there's new data; mark if so
#'
#' @param df_summary The summary prepped data.table object output by the
#' dcp_merge_with_gbd_locations() function
#' @param indicator The indicator associated with this data, used to associate
#' with a filename in the folder '/home/j/WORK/11_geospatial/10_mbg/
#' data_coverage_plots/00_data_summary_tables'
#' @param since_date The date used to compare against old plots, in the format
#' produced by as.character(Sys.Date()) - ie. YYYY-MM-DD. Defaults to the
#' last time data was added to the summary table
#'
#' @return The df_summary with a new column, 'new_data', indicating whether
#' each row was added in the time since 'since_date'
#'
#' @export
dcp_find_new_data <- function(df_summary, indicator, since_date) {

  # Look for an existing data summary table
  summary_dir <- paste0(j_root, "WORK/11_geospatial/10_mbg/data_coverage_plots/00_data_summary_tables/")
  summary_file <- paste0(summary_dir, indicator, "_data_summary.csv")

  df_summary$date <- as.character(Sys.Date())

  if (file.exists(summary_file)) {
    # read in old file
    df_summary_old <- fread(summary_file, stringsAsFactors = F)
    # grab svy ids & dates from old file
    # Rename svy_id to nid, if needed
    needs_rename <- ("svy_id" %in% names(df_summary_old)) & !("nid" %in% names(df_summary_old))
    if (needs_rename) df_summary_old[, nid := svy_id]
    # Only merge on the old df_summary if both merge columns exist
    if (("nid" %in% names(df_summary_old)) & ("date" %in% names(df_summary_old))) {
      # Ensure that the nid_dates_old is unique by NID
      nid_dates_old <- unique(df_summary_old[, c("nid", "date")])
      nid_dates_old <- nid_dates_old[!is.na(nid)]
      nid_dates_old <- nid_dates_old[!duplicated(nid), ]

      # Ensure that the nid and date fields are the correct data types
      nid_dates_old[, date_old := as.character(date)]
      suppressWarnings(nid_dates_old[, temp := as.integer(nid)])
      nid_dates_old[, nid := NULL]
      setnames(nid_dates_old, "temp", "nid")
      nid_dates_old <- nid_dates_old[, c("nid", "date_old"), with = F]

      # merge
      df_summary <- merge(df_summary,
        nid_dates_old,
        by = c("nid"),
        all.x = T
      )

      # replace if an older date exists
      df_summary[(date != date_old) & !is.na(date_old), date := date_old]
      df_summary[, date_old := NULL]
    }
  }

  # replace dates for country-rows with no data with "na"
  df_summary[is.na(nid), date := NA]
  df_summary <- df_summary[order(location_name)]

  unlink(summary_file)
  write.csv(df_summary, file = summary_file, row.names = FALSE)

  # Mark which data is new with a variable "new_data" (for graphing)
  if (is.null(since_date)) {
    df_summary[date == max(df_summary$date, na.rm = T), new_data := 1]
    df_summary[!is.na(date) & is.na(new_data), new_data := 0]
  } else {
    df_summary[(as.Date(date) > as.Date(since_date)) & !is.na(date), new_data := 1]
    df_summary[!is.na(date) & is.na(new_data), new_data := 0]
  }

  return(df_summary)
}
