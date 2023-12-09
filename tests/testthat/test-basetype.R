test_that("multiplication works", {

  x <- Cat(c("Uno", "Dos", "Uno"))
  expect_equal(as_basetype(x), vctrs::vec_data(x))

  x <- list(letters[1:3], NA, letters[2:4], character(0))
  x <- Cats(x)
  as_basetype(x)
  as_basetype(x, flatten = TRUE)
  expect_equal(as_basetype(x, flatten = TRUE), c("a|b|c", NA, "b|c|d",NA))

  x <- Num(1:10)
  expect_equal(as_basetype(x), vctrs::vec_data(x))

  x <- list(1:3, NA, 5:7, double(0))
  x <- Nums(x)
  as_basetype(x)
  as_basetype(x, flatten = TRUE)
  expect_equal(as_basetype(x, flatten = TRUE), c("1|2|3", NA, "5|6|7",NA))

})
