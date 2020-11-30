#' catNumeroComplet
#'
#' @param countryCode
#' nom doit etre un chaine de caracters de chiffres
#' @param tonNumero
#' tonNumero doit etre un chaine de caracters de 10 chiffres
#' @param Langue
#' Langue doit etre un string de caracters
#'
#' @export
#' @description
#' catNomComplet prend les valeurs du countryCode et tonNumero et les concatent ensembles.
#' Il le met dans la format du pays choisi (soit France, soit USA)
#'
#' @examples
#' catNumeroComplet("33","0123456789","France")
#' [1] "+33 1-23-45-67-89"
#' catNumeroComplet("33","0123456789","USA")
#' [1] "+33 012-345-6789"
#'
catNumeroComplet <- function(countryCode, tonNumero, Langue){
  paste(if (is.null(countryCode)) {
    paysdf[[Langue]][16]
  } else if (stringr::str_detect(countryCode, "^\\+") == T) {
    if (comparaisonChiffres(substring(countryCode, 2))) {
      countryCode
    } else {
      paysdf[[Langue]][16]
    }
  } else if (comparaisonChiffres(countryCode)) {
    paste0("+", countryCode)
  } else {
    paysdf[[Langue]][16]
  },
  if (is.null(tonNumero) ||
      !comparaisonChiffres(tonNumero)) {
    paysdf[[Langue]][16]
  } else {
    if (stringr::str_count(tonNumero, "[0123456789]") == 10) {
      if (Langue == "France") {
        numeroFrancais(tonNumero)
      } else {
        numeroAmericain(tonNumero)
      }
    } else {
      paysdf[[Langue]][15]
    }
  })
}
