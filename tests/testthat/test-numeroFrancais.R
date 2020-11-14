test_that("numeroFrancais renvoit un numero telephonic facon francais",{
  x <- "12-23-34-45-56"
  y <- "1223344556"
  z <- "0123456789"
  w <- "1-23-45-67-89"
  expect_identical(x,numeroFrancais(y))
  expect_identical(w,numeroFrancais(z))
})
