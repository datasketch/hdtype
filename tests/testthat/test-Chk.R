test_that("multiplication works", {

  x <- c(TRUE, FALSE, TRUE, NA)

  expect_equal(set_chk_labels(x, spec = "bool"), x)
  expect_equal(set_chk_labels(x, spec = "yesno"), c("Yes", "No", "Yes", NA))

  # Cat
  check <- new_Chk(x)
  check
  attr(check, "stats")
  attr(check, "stats")
  attr(check, "values")


  expect_equal(attr(check,"stats")$summary$value, c(FALSE, TRUE, NA))
  expect_equal(attr(check,"stats")$summary$n[2], 2)
  expect_equal(attr(check,"stats")$n_unique, 2)
  expect_equal(attr(check,"format")$n_categories, 2)

  labels <- attr(check,"format")$values$label
  expect_equal(attr(check,"format")$values$label, x)


  x <- Chk(TRUE)
  attr(x,"format")$values

  x <- Chk(TRUE, spec = "yesno")
  attr(x,"format")$values
  expect_equal(Chk_labels(x), "Yes")

  x <- Chk(TRUE, spec = "emoji1")
  attr(x,"format")$values
  expect_equal(Chk_labels(x), "✔️")

})
