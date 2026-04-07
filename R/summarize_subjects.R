#' Summarize each column in your dataframe
#'
#' @param dataset dataframe
#' @param subject_var group or subject.id
#' @param columns_to_use all or select a specific list
#' @param group_var group
#' @param bodymass_var body.mass
#' @param photoperiod_var Null by default
#'
#' @returns summary data for each column
#' @export
#'
#' @examples
#' \dontrun{
#' summary_df2_photo <- summarize_subjects(dataset = df2,
#'      subject_var =subject.id,
#'       columns_to_use = columns_updated,
#'        group_var = group,
#'         bodymass_var = body.mass,
#'         photoperiod_var = "photoperiod")
#'}
#'
#'

summarize_subjects<- function(dataset, subject_var, columns_to_use, group_var, bodymass_var,
                              photoperiod_var = NULL) {  # NULL = off by default

  # Build grouping variables dynamically
  grp_vars <- if (!is.null(photoperiod_var)) {
    c(as_label(enquo(subject_var)), photoperiod_var)
  } else {
    as_label(enquo(subject_var))
  }

  dataset %>%
    group_by(across(all_of(grp_vars))) %>%
    reframe(
      across(
        all_of(columns_to_use),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          sem  = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
          sd   = ~sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      group     = {{group_var}},
      body.mass = {{bodymass_var}}
    ) %>%
    distinct()
}
