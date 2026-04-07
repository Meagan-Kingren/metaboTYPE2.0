#' Get all column names in a dataframe
#'
#' @param dataset combined dataframe
#' @param exclude optional columns to exclude can be entered here as a list =c("colum_name", ...)
#'
#' @returns column names as a value
#' @export
#'
#' @examples
#' \dontrun{
#' all_cols <- get_columns(df1)
#'}

get_columns <- function(dataset,
                        exclude = NULL) {    # optional columns to exclude

  cols <- colnames(dataset)

  if (!is.null(exclude)) {
    cols <- cols[!cols %in% exclude]
  }

  return(cols)
}
