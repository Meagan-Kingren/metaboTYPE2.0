sem <- function(x) sd(x, na.rm = TRUE) / sqrt(length(!is.na(x)))
