#' Filter excessively large running events out
#'
#' @param df dataframe
#' @param id_col subject.id
#' @param time_col exp.hour
#' @param ped_col PedMeters_R
#' @param threshold 50 meters (or if you have wheel running, you can adapt this function)
#'
#' @returns cleaned dataframe with NAs where excessively large events were
#' @export
#'
#' @examples
#' \dontrun{
#' df1_filtered <- clean_PedMeters(df1_filtered)
#'}
#'

clean_PedMeters <- function(df,
                             id_col = "subject.id",
                             time_col = "exp.hour",
                             ped_col = "PedMeters_R",
                             threshold = 50) {
  df <- df %>%
    mutate(
      # Replace events above threshold with NA
      across(all_of(ped_col), ~ ifelse(. > threshold, NA, .))
    ) %>%
    group_by(across(all_of(id_col))) %>%
    arrange(across(all_of(time_col)), .by_group = TRUE) %>%
    mutate(
      PedMeters_sum_cleaned = cumsum(replace_na(.data[[ped_col]], 0))
    ) %>%
    ungroup()

  n_filtered <- sum(is.na(df[[ped_col]]))
  message(paste(n_filtered, "pedometer events exceeded threshold of", threshold, "and were replaced with NA."))

  return(df)
}
