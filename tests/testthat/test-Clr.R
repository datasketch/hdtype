test_that("Clr", {





  # Clr
  x <- c(c("yellow","red", "blue"), NA)
  colors <- new_Clr(x)
  attr(colors, "stats")
  attr(colors, "format")
  str(colors)

  ## TODO review color spec, like HEX or Color Names
  #expect_equal(set_clr_spec(x, spec = "hex"), tolower(x))

  expect_equal(attr(colors,"stats")$summary$category, unique(x))
  expect_equal(attr(colors,"stats")$summary$n[2], 2)
  expect_equal(attr(colors,"stats")$n_unique, 3)
  expect_equal(attr(colors,"format")$n_categories, 3)



  #Clr(NULL)
  Clr(NA)
  Clr(c(NA, NA))

  expect_error(Clr(c("1","0.2")))

  # Accepts anything coercible from double()
  class(x)
  expect_true(inherits(x, "hd_Clr"))

  c("x", Clr("y"))
  c(Clr("x"), "y")

  as.character(Clr(x))

  vctrs::vec_cast(Clr(c("c","d")), character())
  vctrs::vec_cast(c("c","d"), new_Bin())

  x <- Clr(c(1,1,2,2,3,3))
  expect_equal(Clr_get_categories(x)$category,c("1","2","3"))
  expect_equal(Clr_get_n_categories(x), 3)
  stats <- Clr_stats(x)
  expect_equal(stats$summary$n[1:3], as.vector(table(x)))

  x <- Clr(c("a","b"), spec = "UPPERCASE")
  expect_equal(Clr_get_categories(x)$label,c("A","B"))


  a <- data.frame(mycats = Clr(c("black", "white")), value = 1:2)
  tibble::tibble(a)

})
