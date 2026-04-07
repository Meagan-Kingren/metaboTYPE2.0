#' Filter high and low RER values
#'
#' This is helpful to filter out torpor events. Relax thresholds if you know you're sending a mouse into ketogenesis
#'
#' @param df dataframe
#' @param rer_col RER_M
#' @param upper 1.2
#' @param lower 0.6
#'
#' @returns dataframe with cleaned RER data (NAs where values were outside params)
#' @export
#'
#' @examples
#' \dontrun{
#' df1_filtered <- clean_RER(df1_filtered)
#'}
#'
clean_RER <- function(df,
                      rer_col = "RER_M",
                      upper = 1.2,
                      lower = 0.6) {
  df <- df %>%
    mutate(
      across(all_of(rer_col), ~ ifelse(. > upper | . < lower, NA, .))
    )

  n_filtered <- sum(is.na(df[[rer_col]]))
  message(paste(n_filtered, "RER values outside of range [", lower, ",", upper, "] were replaced with NA."))

  return(df)
}
