
new_UKT <- function(x = character(), na_str = NA){
  if(is.logical(x)) x <- as.character(x)
  vctrs::vec_assert(x, character())
  vctrs::new_vctr(x, format = list(na_str = na_str), class = "hd_UKT")
}


#' @title UnknownType Vectors
#' @description Creates objects of type "hd_UKT". Usually for NA vectors
#'
#' @param x object to be created as UKT type
#'
#' @examples
#' x <- c(NA, NA)
#' ukts <- new_UKT(x)
#' class(ukts)
#'
#' @export
UKT <- function(x = character(), format = NULL) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  new_UKT(x)
}

#' @title Category Vectors
#' @description test for objects of type "hd_UKT"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hd_UKT or not.
#'
#' @examples
#' some_value <- Cat(c("Cat1", "Cat2"))
#' is_Cat(some_value)
#'
#' @export
is_UKT <- function(x) {
  inherits(x, "hd_UKT")
}

# Methods

## Format method

#' @export
format.hd_UKT <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hd_UKT <- function(x, ...) {
  "UKT"
}

# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_UKT
#' @export
#' @export vec_ptype2.hd_UKT
vec_ptype2.hd_UKT <- function(x, y, ...) UseMethod("vec_ptype2.hd_UKT", y)

#' @method vec_ptype2.hd_UKT default
#' @export
vec_ptype2.hd_UKT.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


# A UKT combined with a UKT returns a UKT

#' @method vec_ptype2.hd_UKT hd_UKT
#' @export
vec_ptype2.hd_UKT.hd_UKT <- function(x, y, ...) new_UKT()

# UKT and character return double

#' @method vec_ptype2.hd_UKT character
#' @export
vec_ptype2.hd_UKT.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_UKT
#' @export
vec_ptype2.character.hd_UKT <- function(x, y, ...) character()





# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_UKT
#' @export
#' @export vec_cast.hd_UKT
vec_cast.hd_UKT <- function(x, to, ...) UseMethod("vec_cast.hd_UKT")

#' @method vec_cast.hd_UKT default
#' @export
vec_cast.hd_UKT.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Cat to Cat

#' @method vec_cast.hd_UKT hd_UKT
#' @export
vec_cast.hd_UKT.hd_UKT <- function(x, to, ...) x

#' @method vec_cast.hd_UKT character
#' @export
vec_cast.hd_UKT.character <- function(x, to, ...) UKT(x)

#' @method vec_cast.character hd_UKT
#' @export
vec_cast.character.hd_UKT <- function(x, to, ...) vctrs::vec_data(x)


#' #' @method vec_cast.hd_UKT logical
#' #' @export
#' vec_cast.hd_UKT.logical <- function(x, to, ...) UKT(x)
#'
#' #' @method vec_cast.logical hd_UKT
#' #' @export
#' vec_cast.logical.hd_UKT <- function(x, to, ...) UKT(x)
#'



#' @export
as_UKT <- function(x) {
  vctrs::vec_cast(as.character(x), new_UKT())
}


#' @export
UKT_format <-  function(x){
  if(!is_UKT(x)) stop("x must be a Dat")
  NULL
}

#' @export
UKT_stats <-  function(x){
  if(!is_UKT(x)) stop("x must be a Dat")
  NULL
}
