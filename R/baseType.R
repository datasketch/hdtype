
#' @export
as_baseType <- function(x){
  ## TODO Cat as factors???
  UseMethod("as_baseType")
}

#' @export
as_baseType.default <- function(x){

  if(is_Dat(x)) return(as.Date(x))
  vctrs::vec_data(x)
}

