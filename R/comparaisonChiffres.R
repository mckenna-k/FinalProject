#' comparaisonChiffres
#'
#' @param numero
#' un string que est cense etre que les chiffres
#' @export
#'
#' @examples
comparaisonChiffres <- function(numero){
  return(stringr::str_count(numero)==stringr::str_count(numero,"[0123456789]"))
}
