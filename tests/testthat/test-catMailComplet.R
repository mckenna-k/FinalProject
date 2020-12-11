test_that("Test pour voir si c'est un mail valid",{
  x <- "Peregrin.Touque.shire.com"
  y <- "Peregrin.Touque@shire.com"
  pays <- "France"
  expect_match(catMailComplet(y,pays),"Peregrin.Touque@shire.com")
  expect_match(catMailComplet(x,pays),"Erreur: pas une format propre pour un mail")
})
