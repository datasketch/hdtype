

#' @title Check if x is an hdvector
#'
#' @description test for objects of type "hdvector"
#'
#' @param x hdtype object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is one of the [available_hdtypes()] or not.
#'
#' @examples
#'
#' x <- Cat("value1")
#' is_hdvector(x)
#'
#' @export
is_hdvector <- function(x, strict = TRUE){
  av_types <- paste0("hd_",available_hdtypes())
  hd_class <- class(x)[grepl("hd_", class(x))]
  if(length(hd_class) == 0){
    if(strict){
      return(FALSE)
    } else{
      is.vector(x)
    }
  }else{
    return(hd_class %in% av_types)
  }
}


