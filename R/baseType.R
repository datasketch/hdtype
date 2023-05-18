
#' @export
as_basetype <- function(x){
  ## TODO Cat as factors???
  UseMethod("as_basetype")
}

#' @export
as_basetype.default <- function(x){

  if(is_Dat(x)) return(as.Date(x))
  vctrs::vec_data(x)
}

