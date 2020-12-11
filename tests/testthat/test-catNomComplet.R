test_that("Concatener nom et prenom dependant du pays",{
  x <- "Touque"
  y <- "Peregrin"
  pays <- "France"
  pays2 <- "USA"
  expect_match(catNomComplet(pays,x,y),"TOUQUE Peregrin")
  expect_match(catNomComplet(pays2,x,y),"Touque Peregrin")
})
