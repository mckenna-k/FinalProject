#' catMailComplet
#'
#' @param mail
#' mail doit etre un string de caracters avec une arobase
#' @param Langue
#' Langue doit etre un string de caracters
#'
#' @export
#' @description
#' catMailComplet prend les valeurs et test pour voir si il y a une arobase.
#' Si il n'y a pas de arobase, il renvoit une message d'erreur
#'
#' @examples
#' catMailComplet("McKenna.kevin.gmail.com","France")
#' [1] Erreur: pas une format propre pour un mail
#'
catMailComplet <- function(mail, langue){
  if (is.null(mail) || !stringr::str_detect(mail, "@")) {
    as.character(paysdf[[langue]][17])
  } else {
    mail
  }
}
