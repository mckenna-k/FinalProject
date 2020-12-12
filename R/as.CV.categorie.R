#' as.CV.categorie
#'
#' @param dataTxt
#' un tableau de donnee facon type: level,chiffre
#' @param pays
#' un chaine de caracters, pour decider la langue.  "France","USA"
#'
#'@export
#'@description
#'as.CV.categorie prend un tableau de valeurs venant de Shiny et les separent pour mieux les afficher sous RMarkdown
#'
as.CV.categorie <- function(dataTxt, pays) {
  if (is.function(dataTxt)) {
    dataTxt <- dataTxt()
  }
  #if(is.null(dataTxt)){return(null)}
  results <- dataTxt %>%
    tidyr::separate(1, c("type", names(dataTxt)[1]), sep = ":") %>%
    tidyr::separate(2, c(names(dataTxt)[1], KBFpackage::message["level", pays]), sep = paste0(", ", KBFpackage::message["level", pays]))
  results[, 3] <- as.numeric(results[, 3])
  class(results) <-
   append(class(results), ifelse(pays == "France", "CV.FR", "CV.US"))
  return(results)

}
