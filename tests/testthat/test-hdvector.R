test_that("multiplication works", {

  cat <- Cat("cat")
  expect_true(is_hdvector(cat))

  x <- 0
  expect_false(is_hdvector(0))
  expect_true(is_hdvector(0, strict = FALSE))

})
