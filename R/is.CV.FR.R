#' is.CV.FR
#'
#' @param obj
#' Un CV qui est cense etre un CV facon francais
#'
#' @export
#' @description
#' affiche "CV.FR" si obj est un CV suivant la facon Francais
#'
#'
is.CV.FR <- function(obj) {
  return("CV.FR" %in% class(obj))
}
