
#' @export
Cats <- function(x){
  if(all(is.na(x)) & !is.list(x)) x <- as.list(x)
  xs <- vctrs::as_list_of(x, .ptype = character())
  xs <- purrr::map(xs, Cat, skip_stats = TRUE)
  xs
}

#' @export
is_Cats <- function(x){
  if(!is.list(x)) return(FALSE)
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

