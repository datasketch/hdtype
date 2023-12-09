
#' @export
as_basetype <- function(x, flatten = FALSE, collapse = "|"){
  ## TODO Cat as factors???
  #UseMethod("as_basetype")
  if(is_Dat(x)) return(as.Date(x))
  if(lubridate::is.Date(x)) return(x)
  if(is_Nums(x) || is_Cats(x)){
    x <- purrr::map(x, vctrs::vec_data)
    if(flatten) x <- collapse_each(x, collapse = collapse)
    return(x)
  }
  #if(is.list(x)) stop("Cannot be a list")
  vctrs::vec_data(x)

}

collapse_each <- function(x, collapse = ","){
  sapply(x, function(y){
    if(all(is.na(y))) return(NA)
    paste(y, collapse = collapse)
  })
}


##' @export
#as_basetype.default <- function(x, flatten = FALSE){
#}

