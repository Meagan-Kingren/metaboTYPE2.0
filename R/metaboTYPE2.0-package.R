#' @importFrom dplyr %>% mutate group_by across all_of arrange ungroup
#'   first last lead select reframe distinct any_of where
#' @importFrom tidyr replace_na
#' @importFrom ggplot2 ggplot aes geom_line geom_boxplot geom_point labs
#'   theme theme_minimal element_line element_blank element_text annotate
#'   facet_wrap scale_x_continuous ggsave position_jitter label_value
#' @importFrom rlang enquo as_label sym .data
#' @importFrom stats ave sd
"_PACKAGE"

utils::globalVariables(c("cumsum_rezeroed", "FeedingEvent", "label_value"))
