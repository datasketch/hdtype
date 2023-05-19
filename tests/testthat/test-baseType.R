test_that("base_type", {

  dat1 <- Dat("2020-05-20")
  expect_equal(as_basetype(dat1), as.Date("2020-05-20"))

  date <- as_basetype(as.Date("2022-04-05"))
  expect_true(isDate(date))
  expect_true(lubridate::is.Date(as_basetype(dat1)))

  as_basetype(Cat("2032"))

})
