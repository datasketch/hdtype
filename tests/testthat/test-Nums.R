

test_that("Num Lists work", {

  x <- list(1:3, 2, 5:7, 5)
  xs <- Nums(x)
  d <- tibble::tibble(a = 1:4, nums = xs)

  expect_true(is_Nums(xs))

  x <- list(1:3, NA, 5:7, double(0))
  xs <- Nums(x)

  x <- rep(list(1), 5000)
  xs <- Nums(x)

  x <- rep(NA, 10)
  xs <- Nums(x)

  x <- as.list(rep(NA, 10))
  Nums(x)



})



