
new_hdtype <- function(x = character()){
  vctrs::vec_assert(x, character())
  av <- available_hdtypes(as_character = TRUE)
  if(!all(vctrs::vec_data(x) %in% av)){
    stop(x, "hdtype must be one of: ", paste(av, collapse = ", "))
  }
  vctrs::new_vctr(x, class = "hdtype")
}

#' @export
hdtype <- function(x = character()) {
  x <- vctrs::vec_cast(x, character())
  new_hdtype(x)
}



# hdtype ----------------------------------------------------------------------

#' @title hdtype Vectors
#'
#' @description test for objects of type "hdtype"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdtype or not.
#'
#' @examples
#'
#' some_value <- hdtype("Cat")
#' is_hdtype(some_value)
#'
#' @export
is_hdtype <- function(x) {
  inherits(x, "hdtype")
}


#' @title hdtype Vectors
#'
#' @description test for objects of type "hdtype"
#'
#' @param x hdtype object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is one of the [available_hdtypes()] or not.
#'
#' @examples
#'
#' some_cat_value <- Cat("value1")
#' is_any_hdtype(some_cat_value)
#'
#' @export
is_any_hdtype <- function(x){
  sum(grepl("hd_", class(x))) > 0
}



#' @title hdtype Vectors
#'
#' @description Detect which hdtype is the value.
#'
#' @param x hdtype object to be coerced or tested
#'
#' @return returns the name of the hdtype value. You can see the valid hdtypes with [available_hdtypes()]
#'
#' @examples
#'
#' some_cat_value <- Cat("value1")
#' which_hdtype(some_cat_value)
#'
#' @export
which_hdtype <- function(x){
  if(is_any_hdtype(x)){
    gsub("hd_","",class(x)[grep("hd_", class(x))])
  } else {
    warning("The value is not a valid hdtype")
  }
}


# hdtype ------------------------------------------------------------------

#' @title hdtype Vectors
#'
#' @description coerces its argument to a hdtype It is an abbreviated form of hdtype
#'
#' @param x object to be coerced
#'
#' @return attempts to coerce its argument to hdtype type
#'
#' @examples
#'
#' some_chr_value <- "Cat"
#' class(some_chr_value)
#'
#' some_hdt_value <- as_hdtype(some_chr_value)
#' class(some_hdt_value)
#'
#' @export
as_hdtype <- function(x) {
  if(any(class(x) == "factor")){
    x <- as.character(x)
  }
  vctrs::vec_cast(x, new_hdtype())
}




# Methods

## Format method

#' @export
format.hdtype <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hdtype <- function(x, ...) {
  "hdtype"
}

# Coercion

#' @method vec_ptype2 hdtype
#' @export
vec_ptype2.hdtype <- function(x, y, ...) UseMethod("vec_ptype2.hdtype", y)

#' @method vec_ptype2.hdtype default
#' @export
vec_ptype2.hdtype.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# A hdtype combined with a hdtype returns a hdtype

#' @method vec_ptype2.hdtype hdtype
#' @export
vec_ptype2.hdtype.hdtype <- function(x, y, ...) new_hdtype()

# # hdtype and character return hdtype

#' @method vec_ptype2.hdtype character
#' @export
vec_ptype2.hdtype.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hdtype
#' @export
vec_ptype2.character.hdtype <- function(x, y, ...) character()

# Casting

#' @method vec_cast hdtype
#' @export
vec_cast.hdtype <- function(x, to, ...) UseMethod("vec_cast.hdtype")

#' @method vec_cast.hdtype default
#' @export
vec_cast.hdtype.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce hdtype to hdtype

#' @method vec_cast.hdtype hdtype
#' @export
vec_cast.hdtype.hdtype <- function(x, to, ...) x

#' @method vec_cast.hdtype character
#' @export
vec_cast.hdtype.character <- function(x, to, ...) hdtype(x)

#' @method vec_cast.character hdtype
#' @export
vec_cast.character.hdtype <- function(x, to, ...) vctrs::vec_data(x)


#' @method as.character hdtype
#' @export
as.character.hdtype <- function(x) as.character(vctrs::vec_data(x))



