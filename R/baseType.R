
#' @export
as_base_type <- function(x){
  ## TODO Cat as factors???
  UseMethod("as_base_type")
}

#' @export
as_base_type.default <- function(x){

  if(is_Dat(x)) return(as.Date(x))
  vctrs::vec_data(x)
}

