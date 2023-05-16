
#' @export
available_hdtypes <- function(as_character = FALSE){
  hds <- hdtype:::hdtypes
  if(as_character) return(hds$id)
  hdtype(hds$id)

}
