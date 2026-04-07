#' Align a dataframe to a common start date and time
#'
#' @param dataset oftentimes this will be your TimeSeries or Circadian datatab
#' @param subject_var Animal or Subject ID
#' @param datetime_var Date created when importing your dataset
#' @param cutoff_datetime the first time you want included for this dataframe in "%Y/%m/%d %H:%M:%S" format
#' @param timezone Options other than America/Chicago are America/New_York, America_Los_Angeles, America/Chicago, Asia/Tokyo, Asia/Hong_Kong, Europe/London, Europe/Paris, and Europe/Berlin are popular options
#'
#' @returns A new dataframe with all subjects starting at the same date/time
#' @export
#'
#' @examples
#' # 1. Import the data, forcing DateTime to read as plain text
#' data1 <- read_csv("TimeSeries1.csv", col_types = cols(DateTime = col_character()))
#' # 2. Create a proper POSIXct Date column from the DateTime string
#' data1 <- data1 %>% mutate(Date = as.POSIXct(DateTime, format = "%Y/%m/%d %H:%M:%S", tz = "America/Chicago"))
#' # 3. Run the alignment function
#' data1_filt <- align_to_common_time(dataset = data1, subject_var = Animal, datetime_var = Date, cutoff_datetime = "2023/06/27 07:30:02", timezone = "America/Chicago")
#'

align_to_common_time <- function(dataset,
                                 subject_var,
                                 datetime_var,
                                 cutoff_datetime,
                                 timezone = "America/Chicago") {
  cutoff <- as.POSIXct(cutoff_datetime, format = "%Y/%m/%d %H:%M:%S", tz = timezone)

  subject_var  <- as.character(substitute(subject_var))
  datetime_var <- as.character(substitute(datetime_var))

  # Filter rows before cutoff
  dataset <- dataset[dataset[[datetime_var]] >= cutoff, ]

  # Sort within each subject
  dataset <- dataset[order(dataset[[subject_var]], dataset[[datetime_var]]), ]

  # Compute elapsed hours per subject
  dataset$exp.hour <- ave(
    as.numeric(dataset[[datetime_var]]),
    dataset[[subject_var]],
    FUN = function(x) (x - min(x, na.rm = TRUE)) / 3600
  )

  dataset
}
