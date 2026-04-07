#' Select photoperiod (light/dark cycle) for your time aligned dataframe
#'
#' @param dataset your (combined) dataframe of timeseries or circadian data
#' @param subject_var Animal or Subject ID
#' @param datetime_var Date
#' @param lights_on time the lights turn on at
#' @param lights_off time the lights turn off at
#' @param cycle_length 24 hours
#' @param timezone America/Chicago or others
#'
#' @returns photoperiod aligned data
#' @export
#'
#' @examples
#'
#' Using defaults
#' photoperiod_data1 <- align_to_photoperiod(dataset = data1, subject_var  = Animal, datetime_var = Date)

# Overriding defaults
#' photoperiod_data1 <- align_to_photoperiod(dataset = data1, subject_var  = Animal,  datetime_var = Date,  lights_on    = "06:00:00",  lights_off   = "18:00:00")
#'

align_to_photoperiod <- function(dataset,
                                 subject_var,
                                 datetime_var,
                                 lights_on    = "07:00:00",
                                 lights_off   = "19:00:00",
                                 cycle_length = 24,
                                 timezone     = "America/Chicago") {

  subject_var  <- as.character(substitute(subject_var))
  datetime_var <- as.character(substitute(datetime_var))

  # Parse lights on/off times using the date of each observation
  dates <- as.Date(dataset[[datetime_var]], tz = timezone)

  lights_on_ct  <- as.POSIXct(paste(dates, lights_on),  format = "%Y-%m-%d %H:%M:%S", tz = timezone)
  lights_off_ct <- as.POSIXct(paste(dates, lights_off), format = "%Y-%m-%d %H:%M:%S", tz = timezone)

  # Print photoperiod summary to console
  light_hours <- as.numeric(difftime(lights_off_ct[1], lights_on_ct[1], units = "hours"))
  dark_hours  <- cycle_length - light_hours

  message(
    "\n--- Photoperiod Summary ---",
    "\n  Lights on:       ", lights_on,
    "\n  Lights off:      ", lights_off,
    "\n  Light phase:     ", light_hours, " hours",
    "\n  Dark phase:      ", dark_hours,  " hours",
    "\n  Cycle length:    ", cycle_length, " hours",
    "\n  Timezone:        ", timezone,
    "\n---------------------------\n"
  )

  # Determine which phase each observation falls in
  dataset$phase <- ifelse(
    dataset[[datetime_var]] >= lights_on_ct & dataset[[datetime_var]] < lights_off_ct,
    "light", "dark"
  )

  # For dark phase observations after midnight, anchor to lights_on previous day
  after_midnight <- format(dataset[[datetime_var]], "%H:%M:%S", tz = timezone) < lights_on

  anchor <- ifelse(
    dataset$phase == "light" | !after_midnight,
    as.numeric(lights_on_ct),
    as.numeric(lights_on_ct) - (cycle_length * 3600)
  )
  class(anchor) <- c("POSIXct", "POSIXt")
  attr(anchor, "tzone") <- timezone

  dataset$hours_since_lights_on <- as.numeric(
    difftime(dataset[[datetime_var]], anchor, units = "hours")
  ) %% cycle_length

  # Compute elapsed hours per subject from their first observation
  dataset <- dataset[order(dataset[[subject_var]], dataset[[datetime_var]]), ]

  dataset$exp.hour <- ave(
    as.numeric(dataset[[datetime_var]]),
    dataset[[subject_var]],
    FUN = function(x) (x - min(x, na.rm = TRUE)) / 3600
  )

  dataset
}
