
#' @export
Cats <- function(x){
  xs <- vctrs::as_list_of(x, .ptype = character())
  xs <- purrr::map(xs, Cat)
  xs
}

#' @export
is_Cats <- function(x){
  all(purrr::map_lgl(x, is_Cat))
}

#' @export
Cats_format <-  function(x){
  if(!is_Cats(x)) stop("x must be a Cats")
  NULL
}

#' @export
Cats_stats <-  function(x){
  if(!is_Cats(x)) stop("x must be a Cats")
  NULL
}

