test_that("baseType", {

  dat1 <- Dat("2020-05-20")
  expect_equal(as_baseType(dat1), as.Date("2020-05-20"))

})
