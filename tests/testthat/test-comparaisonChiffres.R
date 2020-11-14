test_that("Comparer le nombre de chiffres avec le nombre de characters total",{
  x <- "122-334-4556"
  y <- "1223344556"
  expect_true(comparaisonChiffres(y))
  expect_false(comparaisonChiffres(x))
})
