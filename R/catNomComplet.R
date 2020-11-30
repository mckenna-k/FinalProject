#' catNomComplet
#'
#' @param Langue
#' Langue doit etre un string de caracters
#' @param nom
#' nom doit etre un string de caracters
#' @param prenom
#' prenom doit etre un string de caracters
#'
#' @export
#' @description
#' catNomComplet prend les valeurs du nom et prenom et les concatent ensembles.
#' Si la langue donne est France, il met le nom en majuscule
#'
#' @examples
#' catNomComplet("France","McKenna","Kevin")
#' [1] "MCKENNA Kevin"
#' catNomComplet("USA","McKenna","Kevin")
#' [1] "McKenna Kevin"
#'
catNomComplet <- function(Langue, nom, prenom){
  paste(
    if (Langue == "France") {
      stringr::str_to_upper(nom)
    } else {
      nom
    },
    prenom
  )
}
