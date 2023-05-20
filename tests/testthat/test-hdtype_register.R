test_that("hdtype register", {

  id <- "Kat"
  from <- "Cat"

  create_hdtype(id = "Kat", from = "Cat")

  Kat("X")
  class(Kat('a'))

  c(Kat("2"), "b")
  tibble::tibble(a = Kat(2))

  hdtype::Cat_get_categories(Kat)

  c(new_Kat2("d"), "a")



})
