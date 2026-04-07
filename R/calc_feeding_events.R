#' Calculate feeding events from the cumulative food in column
#'
#' This function rezeroes the data since we trimmed timepoints earlier.
#' Then it calculates each feeding event, with the amount eaten during the hour displayed.
#'
#' @param df dataframe
#' @param id_col subject.id
#' @param time_col exp.hour
#' @param cumsum_col FoodInA_M
#'
#' @returns dataframe with new columns for rezeroed cumsum and each feeding event
#' @export
#'
#' @examples
#' \dontrun{
#' df1 <- calc_feeding_events(df1)
#'}

calc_feeding_events <- function(df,
                                id_col = "subject.id",
                                time_col = "exp.hour",
                                cumsum_col = "FoodInA_M") {

  df <- df %>%
    group_by(across(all_of(id_col))) %>%
    arrange(across(all_of(time_col)), .by_group = TRUE) %>%
    mutate(
      cumsum_rezeroed = .data[[cumsum_col]] - first(.data[[cumsum_col]]),
      FeedingEvent = lead(cumsum_rezeroed, default = last(cumsum_rezeroed)) - cumsum_rezeroed
    ) %>%
    ungroup()

  return(df)
}
