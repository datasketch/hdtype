
#' @export
Nums <- function(x, skip_stats = TRUE){
  if(all(is.na(x)) & !is.list(x)) x <- as.list(x)
  xs <- vctrs::as_list_of(x, .ptype = double())
  xs <- purrr::map(xs, Num, skip_stats = TRUE)
  xs
}

#' @export
is_Nums <- function(x){
  all(purrr::map_lgl(x, is_Num))
}

#' @export
Nums_format <-  function(x){
  if(!is_Nums(x)) stop("x must be a Num")
  NULL
}

#' @export
Nums_stats <-  function(x){
  if(!is_Nums(x)) stop("x must be a Num")
  NULL
}
