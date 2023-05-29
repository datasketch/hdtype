test_that("Test guess", {

  # Guess Cat

  v <- c("MSPS-CD-166-2020", "003-2020", "0811 - 2020")
  expect_false(isDate(v)) # Test if there are letters
  expect_equal(guess_hdtype(v), hdtype("Cat"))

  v <- c("10306908", "900935265", "9010385043", "9010385043", "9010385043",
         NA, "901046823", "830035101", "900417425-2")
  expect_false(isDate(v)) # Test if many are parsed as NA. 60% failed to parse
  expect_equal(guess_hdtype(v), hdtype("Cat"))

  # Guess Num
  expect_equal(guess_hdtype(c("1",NA,"2")), hdtype("Num"))
  expect_equal(guess_hdtype(c(0.3, 2, NA)), hdtype("Num"))

  v <- c("4,59", "5,38", "10,78", "123",NA)
  expect_equal(guess_hdtype(v), hdtype("Num"))

  v <- c("343.755,08", "5.380,00", NA, "21.555,11", "1.550.000")
  maybeNum(v)
  expect_equal(guess_hdtype(v), hdtype("Num"))

  # Guess Pct

  expect_equal(guess_hdtype(1), hdtype("Num"))
  expect_equal(guess_hdtype(c(0, 1, 0)), hdtype("Num"))
  expect_equal(guess_hdtype(c(0.3, 2, 0)), hdtype("Num"))

  expect_equal(guess_hdtype(c(0.3, 1, 0)), hdtype("Pct"))
  expect_equal(guess_hdtype(c(0.3, 0.4, 1)), hdtype("Pct"))
  expect_equal(guess_hdtype(c("30%", "200%", NA)), hdtype("Pct"))

  # Guess Dat
  v <- c("2020-04-04", NA)
  isDate(v)
  expect_equal(guess_hdtype(v), hdtype("Dat"))

  v <- "24/06/2020"
  expect_true(isDate(v))
  expect_equal(guess_hdtype(v), hdtype("Dat"))

  v <- "24/6/2020"
  expect_true(isDate(v))
  expect_equal(guess_hdtype(v), hdtype("Dat"))

  h <- Dat(v)
  attributes(h)$format

  # # TODO
  # v <- c("25/03/2020","31/03/2020","06/04/2020","17/04/2020")
  # isDate(v)
  # expect_true(isDate(v))
  #
  # v <- c("03/30/2020","1/05/2020","06/04/2020","7/04/2020")
  # Dat(v)
  # isDate(v)
  # expect_true(isDate(v))


  v <- "24/06/2020"
  expect_equal(guess_hdtype(v), hdtype("Dat"))
  expect_equal(attributes(Dat(v))$format$date_format, "%d/%m/%Y")

  v <- "2020/12/31"
  expect_equal(guess_hdtype(v), hdtype("Dat"))
  expect_equal(attributes(Dat(v))$format$date_format, "%Y/%m/%d")
  v <- "2020:05:20"
  expect_equal(guess_hdtype(v), hdtype("Dat"))
  expect_equal(attributes(Dat(v))$format$date_format, "%Y:%m:%d")


  # v <- c("2014-03-01","2043-04-04","2014-04-04")
  # isDate(v)
  # isDatetime(v)
  # isTime(v)
  # whichDTH(v)
  # v <- c("2014-03-01","2043-04-04","20140404")
  # parseDatetime(v, "D")
  # v <- c("2014-03-01 5:04:00","2043-04-04 5:04:00","2014-04-04 5:04:00")
  # parseDatetime(v, "T")
  # v <- c("04:00","13:05:00","5:04:00")
  # parseDatetime(v, "H")

})
