

test_that("Num Lists work", {

  x <- list(1:3, 2, 5:7, 5)
  xs <- Nums(x)
  d <- tibble::tibble(a = 1:4, nums = xs)

  expect_true(is_Nums(xs))

  x <- list(1:3, NA, 5:7, double(0))
  xs <- Nums(x)



})



