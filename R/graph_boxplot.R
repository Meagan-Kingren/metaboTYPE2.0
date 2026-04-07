#' Create boxplots of summary data
#'
#' Once you create summary data, you can graph it all using boxplots.
#' You can choose to do this from photoperiod data or from general summary data.
#' This will allow you to compare light and dark cycles as well as parameters during each.
#'
#' @param dataset summary dataframe
#' @param group_var group or treatment
#' @param columns_to_plot column list or value of columns to use
#' @param subject_var subject.id
#' @param photoperiod_var NULL or photoperiod
#' @param ylab = NULL to just plot column title
#' @param title column title
#' @param point_size point size for each mean in the boxplot
#' @param point_alpha opacity of each point
#'
#' @returns
#' @export
#'
#' @examples
#' plots <- graph_boxplot(dataset = summary_df2,group_var= group, columns_to_plot = cols, subject_var = subject.id)
#'
#'

graph_boxplot<- function(dataset,
                   group_var,
                   columns_to_plot,
                   subject_var,
                   photoperiod_var = NULL,    # NULL = off by default
                   ylab = NULL,
                   title = NULL,
                   point_size = 3,
                   point_alpha = 0.7) {

  plots <- lapply(columns_to_plot, function(col) {

    y_label    <- if (is.null(ylab)) gsub("_", " ", col) else ylab
    plot_title <- if (is.null(title)) paste(y_label, "by Group") else title

    p <- ggplot(dataset, aes(x = {{ group_var }},
                             y = .data[[col]],
                             fill = {{ group_var }})) +
      geom_boxplot(outlier.shape = NA, alpha = 0.6) +
      geom_point(aes(color = {{ group_var }}),
                 size = point_size,
                 alpha = point_alpha,
                 position = position_jitter(width = 0.1, seed = 42)) +
      labs(
        title = plot_title,
        x     = NULL,
        y     = y_label
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.line        = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        legend.position  = "none",
        axis.text.x      = element_text(angle = 45, hjust = 1)  # angle labels 45 degrees

      )

    # Add facet split if photoperiod_var is specified
    if (!is.null(photoperiod_var)) {
      p <- p + facet_wrap(~ .data[[photoperiod_var]],
                          labeller = label_value)   # labels panels as "photoperiod: Light" etc.
    }

    p
  })

  names(plots) <- columns_to_plot
  return(plots)
}

