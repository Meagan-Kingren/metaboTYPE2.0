#' Title
#'
#' @param df dataframe
#' @param id_col subject.id
#' @param time_col exp.hour
#' @param cumsum_col FoodInA_M
#' @param threshold 0.3 (can be lower if you know how much they're really eating, but this is 300mg per event or more filtered out)
#'
#' @returns cleaned dataframe with excessive feeding events filtered out and a new cumsum column
#' @export
#'
#' @examples
#' \dontrun{
#' df1_filtered <- filter_feeding_events(df1_filtered)
#'}
#'
#'

filter_feeding_events <- function(df,
                                id_col = "subject.id",
                                time_col = "exp.hour",
                                cumsum_col = "FoodInA_M",
                                threshold = 0.3) {

  df <- df %>%
    group_by(across(all_of(id_col))) %>%
    arrange(across(all_of(time_col)), .by_group = TRUE) %>%
    mutate(
      cumsum_rezeroed = .data[[cumsum_col]] - first(.data[[cumsum_col]]),
      FeedingEvent = lead(cumsum_rezeroed, default = last(cumsum_rezeroed)) - cumsum_rezeroed,

      # Replace events above threshold with NA
      FeedingEvent = ifelse(FeedingEvent > threshold, NA, FeedingEvent),

      # Final cumulative sum from cleaned feeding events
      cumsum_clean = cumsum(replace_na(FeedingEvent, 0))
    ) %>%
    ungroup()

  n_filtered <- sum(is.na(df$FeedingEvent))
  message(paste(n_filtered, "feeding events exceeded threshold of", threshold, "and were replaced with NA."))

  return(df)
}
