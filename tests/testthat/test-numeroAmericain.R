test_that("numeroAmericain renvoit un numero telephonic facon americain",{
  x <- "122-334-4556"
  y <- "1223344556"
  expect_identical(x,numeroAmericain(y))
})
