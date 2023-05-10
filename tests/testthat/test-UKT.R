test_that("UKT", {

  # UKT
  x <- c(NA, NA)
  ukts1 <- new_UKT(x)

  expect_true(is_UKT(ukts1))

  ukts2 <- c(NA, UKT(NA))

  c(NA, UKT(NA))
  c(UKT(NA), NA)
  c(as.character(NA), UKT(NA))

  # c(UKT(NA), as.character(NA)) ### ERROR
  c(UKT(NA), UKT(NA))
  c(UKT(NA), NA)



})
