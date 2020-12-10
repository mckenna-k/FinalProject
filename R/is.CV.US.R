#' is.CV.US
#'
#' @param obj
#' Un CV qui est cense etre un CV facon americain
#'
#' @description
#' affiche "CV.US" si obj est un CV suivant la facon Americain
#'
#' @export
#'
is.CV.US <- function(obj) {
  return("CV.US" %in% class(obj))
}
