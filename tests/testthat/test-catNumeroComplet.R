test_that("Concatener les country code et numero telephonic dependant du pays",{
  x <- "33"
  y <- "0123456789"
  pays <- "France"
  expect_match(catNumeroComplet(x,y,pays),"+33 1-23-45-67-89")
})
