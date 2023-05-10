
#' @export
as_baseType <- function(x){
  ## TODO Cat as factors???
  UseMethod("as_baseType")
}

#' @export
as_baseType.default <- function(x){
  vctrs::vec_data(x)
}

