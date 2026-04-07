#' Plot all numeric columns in a dataframe
#'
#' @param dataset timeseries data
#' @param time_var Date
#' @param group_var animal or subject ID or treatment group
#' @param y_cols leave null to autodetect numeric columns or specify from a list
#' @param exclude columns to skip in a dataframe
#' @param save_plots select TRUE to save plots
#' @param output_dir creates a new directory to save plots in
#' @param ... Additional arguments passed to \code{plot_ts}
#'
#' @returns graphs for every single column or every defined column
#' @export
#'
#' @examples
#' \dontrun{
#' plots<- plot_ts_all(dataset = df1,
#'   time_var = exp.hour,
#'   group_var = group,
#'   save_plots = TRUE)
#' }


plot_ts_all <- function(dataset,
                             time_var,
                             group_var,
                             y_cols    = NULL,    # NULL = auto-detect numeric cols
                             exclude   = NULL,    # cols to skip by name
                             save_plots = FALSE,
                             output_dir = "plots/",
                             ...) {

  # Auto-detect numeric columns if y_cols not specified
  if (is.null(y_cols)) {
    time_col  <- as_label(enquo(time_var))
    group_col <- as_label(enquo(group_var))

    y_cols <- dataset %>%
      select(where(is.numeric)) %>%
      select(-any_of(c(time_col, group_col, exclude))) %>%
      colnames()
  }

  message("Plotting ", length(y_cols), " columns: ", paste(y_cols, collapse = ", "))

  plots <- lapply(y_cols, function(col) {
    p <- plot_ts_24h(
      dataset   = dataset,
      time_var  = {{ time_var }},
      y_var     = !!sym(col),
      group_var = {{ group_var }},
      ...                          # forwards shaded_days, etc.
    )

    if (save_plots) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      ggsave(
        filename = file.path(output_dir, paste0(col, ".png")),
        plot     = p,
        width    = 10, height = 5
      )
    }

    p
  })

  names(plots) <- y_cols
  return(plots)
}
