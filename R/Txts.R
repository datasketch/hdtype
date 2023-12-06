
#' @export
Txts <- function(x){
  xs <- vctrs::as_list_of(x, .ptype = character())
  xs <- purrr::map(xs, Txt)
  xs
}

#' @export
is_Txts <- function(x){
  all(purrr::map_lgl(x, is_Txt))
}

#' @export
Txts_format <-  function(x){
  if(!is_Cats(x)) stop("x must be a Txts")
  NULL
}

#' @export
Txts_stats <-  function(x){
  if(!is_Cats(x)) stop("x must be a Txts")
  NULL
}

