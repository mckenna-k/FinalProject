#' numeroFrancais
#'
#' @param numero
#' numero doit etre 10 chiffres de longeurs
#' @export
#'
#' @examples
#' numeroFrancais(1234567898)
#' numeroFrancais(0123456789)
numeroFrancais <- function(numero){
  if (stringr::str_detect(numero, "^0")) {
    return(paste0(substring(numero,2,2),"-",substring(numero,3,4),"-",substring(numero,5,6),"-",substring(numero,7,8),"-",substring(numero,9,10)))
  }
  else{
    return(paste0(substring(numero,1,2),"-",substring(numero,3,4),"-",substring(numero,5,6),"-",substring(numero,7,8),"-",substring(numero,9,10)))
  }
}
