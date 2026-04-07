align_to_start <- function(data,
                           subject_col,
                           time_col,
                           start_time = NULL,
                           value_cols = NULL) {

  # Validate inputs
  if (!subject_col %in% names(data)) stop("subject_col not found in data")
  if (!time_col   %in% names(data)) stop("time_col not found in data")

  # Determine each subject's reference start time
  if (is.null(start_time)) {
    # Default: each subject's own earliest observation
    ref_starts <- tapply(data[[time_col]], data[[subject_col]], min)
    data$ref_start <- ref_starts[as.character(data[[subject_col]])]
  } else {
    # User-specified: same cutoff applied to all subjects
    ref_start_val <- start_time
    if (!inherits(ref_start_val, class(data[[time_col]]))) {
      ref_start_val <- as.POSIXct(start_time)  # coerce if needed
    }
    data$ref_start <- ref_start_val
  }

  # Exclude all rows before the reference start
  data <- data[data[[time_col]] >= data$ref_start, ]

  # Compute elapsed time from the reference start
  data$elapsed_time <- data[[time_col]] - data$ref_start

  # Build output column selection
  keep_cols <- c(subject_col, time_col, "ref_start", "elapsed_time")
  if (!is.null(value_cols)) {
    missing <- setdiff(value_cols, names(data))
    if (length(missing)) warning("Some value_cols not found: ", paste(missing, collapse = ", "))
    keep_cols <- c(keep_cols, intersect(value_cols, names(data)))
  } else {
    keep_cols <- c(keep_cols, setdiff(names(data), keep_cols))
  }

  data[, keep_cols]
}
