
#' @export
available_hdTypes <- function(as_character = FALSE){
  hds <- hdTypes:::hdTypes
  if(as_character) return(hds$id)
  hdType(hds$id)

}
