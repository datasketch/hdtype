test_that("Cat", {

  # Cat
  x <- c(c("Apple","Banana", "Banana", "Lemons"), NA)
  cats <- new_Cat(x)
  attr(cats, "stats")
  attr(cats, "format")
  str(cats)

  expect_equal(set_cat_spec(x, spec = "lowercase"), tolower(x))

  expect_equal(attr(cats,"stats")$summary$category, unique(x))
  expect_equal(attr(cats,"stats")$summary$n[2], 2)
  expect_equal(attr(cats,"stats")$n_unique, 3)
  expect_equal(attr(cats,"format")$n_categories, 3)

  # TODO add option to TRIM (spaces, etc) Cats, and to regroup/refactor



  # Removed option provide a named vector, as it may lead to errors
  # in different names for the same category
  # x <- letters
  # names(x) <- LETTERS
  # cats <- new_Cat(x)
  # expect_equal(attr(cats,"stats")$summary$label, c(LETTERS, NA))


  #Cat(NULL)
  Cat(NA)
  Cat(c(NA, NA))

  # Accepts anything coercible from double()
  x <- Cat(c("1","0.2"))
  class(x)
  expect_true(inherits(x, "hd_Cat"))

  c("x", Cat("y"))
  c(Cat("x"), "y")

  as.character(Cat(x))

  vctrs::vec_cast(Cat(c("c","d")), character())
  vctrs::vec_cast(c("c","d"), new_Bin())

  x <- Cat(c(1,1,2,2,3,3))
  expect_equal(Cat_get_categories(x)$category,c("1","2","3"))
  expect_equal(Cat_get_n_categories(x), 3)
  stats <- Cat_stats(x)
  expect_equal(stats$summary$n[1:3], as.vector(table(x)))

  x <- Cat(c("a","b"), spec = "UPPERCASE")
  expect_equal(Cat_get_categories(x)$label,c("A","B"))


  a <- data.frame(mycats = Cat(c("black", "white")), value = 1:2)
  tibble::tibble(a)

})
