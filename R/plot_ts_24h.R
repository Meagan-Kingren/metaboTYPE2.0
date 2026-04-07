#' Plot timeseries data showing the light/dark cycle
#'
#' @param dataset your combined timeseries dataframe
#' @param time_var date
#' @param y_var column or parameter of choice
#' @param group_var animal or subject ID or treatment group
#' @param title "Time Series of (your parameter)
#' @param xlab hour
#' @param ylab column unit or null
#' @param shaded_days number of days to shade; select null for full experiment
#' @param shaded_width length of cycle
#' @param shaded_fill color of fill
#' @param shaded_alpha opacity of bars
#'
#' @returns time series graph with dark cycle shaded
#' @export
#'
#' @examples
#' \dontrun{
#' plots<- plot_ts_24h(dataset = df1,
#' time_var = exp.hour,
#'  y_var = kcal_hr_M,
#'  group_var = group,
#'  title = "Energy Expenditure
#'  with Light/Dark Cycle",
#'  ylab = "EE (kcal/hr)",
#'  shaded_days = 21,
#'  shaded_width = 12,
#'  shaded_fill = "grey80",
#'  shaded_alpha = 0.5)
#'}

plot_ts_24h <- function(dataset, time_var, y_var, group_var,
                        title = NULL,
                        xlab = "Hour",
                        ylab = NULL,
                        shaded_days = NULL,
                        shaded_width = 12,
                        shaded_fill = "grey80",
                        shaded_alpha = 0.5) {

  # Determine max time value for axis scaling
  time_vals <- dataset[[as_label(enquo(time_var))]]
  time_max <- max(time_vals, na.rm = TRUE)

  # Automatically create a readable y-axis label if not provided
  y_label <- if (is.null(ylab)) {
    gsub("_", " ", as_label(enquo(y_var)))
  } else ylab

  plot_title <- if (is.null(title)) paste("Time Series of", y_label) else title

  # Base ggplot
  p <- ggplot(dataset,
              aes(x = {{time_var}},
                  y = {{y_var}},
                  color = {{group_var}},
                  group = {{group_var}}))

  # 1 Shaded rectangles (drawn underneath lines)
  if (!is.null(shaded_days)) {
    p <- p +
      annotate(
        "rect",
        xmin = seq(from = shaded_width,
                   by = shaded_width * 2,
                   length.out = shaded_days),
        xmax = seq(from = shaded_width * 2,
                   by = shaded_width * 2,
                   length.out = shaded_days),
        ymin = -Inf,
        ymax = Inf,
        fill = shaded_fill,
        alpha = shaded_alpha
      )
  }

  # 2 Data plotted above shading
  p <- p +
    geom_line(linewidth = 1) +
    labs(
      title = plot_title,
      x = xlab,
      y = y_label,
      color = "Group"
    ) +
    # X-axis every 24 hours
    scale_x_continuous(breaks = seq(0, time_max, by = 24)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.line = element_line(color = "black"),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )

  return(p)
}
