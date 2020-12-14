#' show.CV.US
#'
#' @param obj
#' Un dataframe d'objets a afficher (doit etre quelque chose du classe CV.US)
#' @param option
#' l'option du graphique a sortir
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
      kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"))
  } else{
    knitr::kable(
      head(obj[, 1:2], n = dim(obj)[1]),
      caption = "",
      format = "html",
      booktabs = T,
      linesep = " "
    ) %>%
      kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"))
  }
}
