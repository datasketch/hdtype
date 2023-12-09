
test_that("Num Lists work", {

  x <- list(letters[1:3], "b", letters[5:7], "e")
  xs <- Cats(x)
  d <- tibble::tibble(a = 1:4, nums = xs)

  x <- rep(list("Unique"), 500)
  xs <- Cats(x)

  as_basetype(xs)

  x <- rep(NA, 10)
  xs <- Cats(x)

  x <- as.list(rep(NA, 10))
  Cats(x)



})
