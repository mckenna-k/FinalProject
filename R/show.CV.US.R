#' show.CV.US
#'
#' @param obj
#' unknown
#' @param option
#' unknown
#'
#' @description
#' affiche le CV facon US
#'
#' @export
#'
#'

show.CV.US <- function(obj, option = 1) {
  if (FALSE %in% is.null(obj[, 3])) {
    knitr::kable(
      head(obj, n = dim(obj)[1]),
      caption = "",
      format = "html",
      booktabs = T,
      linesep = " "
    ) %>%
      kable_styling(latex_options = c("striped", "HOLD_position"))
  } else{
    knitr::kable(
      head(obj[, 1:2], n = dim(obj)[1]),
      caption = "",
      format = "html",
      booktabs = T,
      linesep = " "
    ) %>%
      kable_styling(latex_options = c("striped", "HOLD_position"))
  }
}
