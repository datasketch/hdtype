test_that("base_type", {

  dat1 <- Dat("2020-05-20")
  expect_equal(as_base_type(dat1), as.Date("2020-05-20"))

})
