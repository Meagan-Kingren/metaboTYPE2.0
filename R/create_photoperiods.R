#' Create photoperiod columns for better graphing purposes
#'
#' @param df dataframe
#' @param time_col exp.hour
#' @param period_length 12
#' @param first_period Light
#'
#' @returns dataframe with additional column for Light or Dark period
#' @export
#'
#' @examples
#' df2 <- create_photoperiods(df1_filtered)
#'
#'

create_photoperiods <- function(df,
                                time_col = "exp.hour",
                                period_length = 12,
                                first_period = "Light") {

  second_period <- ifelse(first_period == "Light", "Dark", "Light")

  df <- df %>%
    mutate(
      photoperiod = ifelse(
        (floor(.data[[time_col]] / period_length) %% 2) == 0,
        first_period,
        second_period
      )
    )

  return(df)
}
