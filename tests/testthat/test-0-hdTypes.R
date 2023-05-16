
test_that("create hdtypes",{

  library(readr)

  # TODO still need void ctype?
  x <- character(0)
  expect_equal(guess_hdtype(x),hdtype("NUT"))

  x <- NA
  expect_equal(guess_hdtype(x),hdtype("UKT"))


  # expect_true(inherits(c(hdtype("Num"), "Cat"),"hdtype"))
  expect_true(inherits(c(hdtype("Num"), "Cat"),"character"))
  ## TODO check coercion rules
  expect_true(inherits(c("Num", hdtype("Cat")),"character"))
  expect_true(inherits(c(hdtype("Num"), hdtype("Cat")),"hdtype"))


  # Data Frames

  # data <-tibble::tibble(a = as.Date(c("2016-04-03", "2016-05-04")),
  #                    b = as.character(c("2016-04-03", "2016-05-04")),
  #                    c = as.factor(c("2016-04-03", "2016-05-04")))
  # expect_true(all(purrr::map_lgl(data,isDate)))

  # expect_equal(unname(unique(vctrs::vec_c(!!!purrr::map(data,guess_hdtype)))),hdtype("Dat"))


  data <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )

  expect_true(inherits(guess_hdtype(data$a),"hdtype"))

  # data <- sample_data("Cat-Dat-Yea-Num-Pct")
  hdtypes <- c(a = "Cat",b = "Dat", c = "Yea", d = "Num", e = "Pct")
  expect_equal(purrr::map_chr(data, guess_hdtype),hdtypes)

  expect_equal(as.character(hdtypes), c("Cat", "Dat", "Yea", "Num", "Pct"))


  # expect_equal(guessFtype(data),"Cat-Dat-Yea-Num-Pct")

  # TODO check formats
  # guessCformats(data)

  #expect_false("___" %in% availableCtypeIds(allowEmpty = FALSE))

})

test_that("All hdtypes have format and stats",{

  avhdts <- available_hdtypes()

  #class(get("Pct_format"))

  get_format_funs <- paste0(avhdts, "_format")
  all_funs <- purrr::map_chr(get_format_funs, ~ class(get(.)))
  expect_true(unique(all_funs) == "function")

  get_stats_funs <- paste0(avhdts, "_stats")
  all_funs <- purrr::map_chr(get_stats_funs, ~ class(get(.)))
  expect_true(unique(all_funs) == "function")


})





# test_that("Cast hdtype",{
#
#   c(hdtype("Num"),"Num")
#   c("Num", hdtype("Num"))
#
#   vctrs::vec_ptype2("Cat", hdtype())
#   vctrs::vec_ptype2(hdtype(),"Num")
#
#   vctrs::vec_ptype_show(hdtype(), character(), hdtype())
#
#   vctrs::vec_cast("Num", hdtype())
#   h <- hdtype("Cat")
#   vctrs::vec_data(h)
#   vctrs::vec_cast(hdtype("Cat"), character())
#
#   hdtype("Cat") == "Cat"
#
#   d <- data.frame(x = hdtype(c("Num", "Cat")), y = 1:2)
#   #readr::write_csv(d,"test.csv")
#
# })

# test_that("write hdtypes",{
#
#   data <- data.frame(
#     a = Cat(c("black", "white")),
#     b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
#     c = Yea(2001:2002),
#     d = Num(runif(2)*10),
#     e = Pct(runif(2))
#   )
#   data_str <- readr::write_csv(data,"test.csv") %>% tibble::as_tibble()
#   str(data_str)
#   test <- readr::read_csv("test.csv",
#                           col_types = readr::cols(.default = "c"))
#   unlink("test.csv")
#   #expect_equivalent(data_str, test)
#
# })



