

create_hdtype <- function(id, from = NULL, ...){

  # id <- "Kat"
  # from <- "Cat"

  # https://stackoverflow.com/questions/3094232/add-objects-to-package-namespace

  # Assign constructor
  fun <- get(paste0("new_", from))
  body(fun) <- patch_hd_class(fun, from, id, prefix = "hd_")
  assign(paste0("new_", id), fun, envir = .GlobalEnv)

  # Assign creator
  fun <- get(from)
  body(fun) <- patch_Type(fun, from, id, prefix = "")
  assign(id, fun, envir = .GlobalEnv)

  #assign(id, fun, envir =  asNamespace('hdtype'))

  fun <- get(paste0("vec_ptype_abbr.",paste0("hd_",from)))
  body(fun) <- patch_Type(fun, from, id)
  assign(paste0("vec_ptype_abbr.",paste0("hd_",id)), fun, envir = .GlobalEnv)




}


patch_hd_class <- function(fun, from, id, prefix = "hd_") {
  #https://stackoverflow.com/questions/38732663/how-to-insert-expression-into-the-body-of-a-function-in-r
  fun.body <- deparse(body(fun))
  patched.fun.body <- gsub(paste0('"',prefix,from, '"'),
                           #c("hd_Cat", "hd_Kat")
                           paste0("c(",paste0('"',prefix,from, '",'),
                                  paste0('"',prefix,id, '")')),
                           fun.body)
  expr <- as.expression(parse(text = patched.fun.body))
  return(expr)
}

patch_Type <- function(fun, from, to, prefix = "") {
  #https://stackoverflow.com/questions/38732663/how-to-insert-expression-into-the-body-of-a-function-in-r
  fun.body <- deparse(body(fun))
  patched.fun.body <- gsub(paste0(prefix,from), paste0(prefix,to), fun.body)
  expr <- as.expression(parse(text = patched.fun.body))
  return(expr)
}






#' @export
hdtype_register <- function(id, from){

  # args
  # args <- list(...)

  f <- get(from)
  environment(f) <- asNamespace('hdtype')
  #environment(new_func) <- environment(old_func)
  #assignInNamespace("myfunction", foo, "mypackage")

  f

}
