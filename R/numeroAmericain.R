#' numeroAmericain
#'
#' @param numero
#' numero doit etre 10 chiffres de longeurs
#' @export
#'
#' @examples
#' numeroAmericain(1234567898)
numeroAmericain <- function(numero){
  return(paste0(substring(numero,1,3),"-",substring(numero,4,6),"-",substring(numero,7,10)))
}
