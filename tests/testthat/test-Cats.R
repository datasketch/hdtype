
test_that("Num Lists work", {

  x <- list(letters[1:3], "b", letters[5:7], "e")
  xs <- Cats(x)
  d <- tibble::tibble(a = 1:4, nums = xs)

  d

})
