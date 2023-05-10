

new_NUT <- function(x = character(), na_str = NA){
  if(is.logical(x)) x <- as.character(x)
  vctrs::vec_assert(x, character())
  vctrs::new_vctr(x, format = list(na_str = na_str), class = "hd_NUT")
}


#' @title UnknownType Vectors
#' @description Creates objects of type "hd_NUT". Usually for NA vectors
#'
#' @param x object to be created as NUT type
#'
#' @examples
#' x <- c(NA, NA)
#' ukts <- new_NUT(x)
#' class(ukts)
#'
#' @export
NUT <- function(x = character(), format = NULL) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  new_NUT(x)
}

#' @title Category Vectors
#' @description test for objects of type "hd_NUT"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hd_NUT or not.
#'
#' @examples
#' some_value <- Cat(c("Cat1", "Cat2"))
#' is_Cat(some_value)
#'
#' @export
is_NUT <- function(x) {
  inherits(x, "hd_NUT")
}

# Methods

## Format method

#' @export
format.hd_NUT <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hd_NUT <- function(x, ...) {
  "NUT"
}

# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_NUT
#' @export
#' @export vec_ptype2.hd_NUT
vec_ptype2.hd_NUT <- function(x, y, ...) UseMethod("vec_ptype2.hd_NUT", y)

#' @method vec_ptype2.hd_NUT default
#' @export
vec_ptype2.hd_NUT.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


# A NUT combined with a NUT returns a NUT

#' @method vec_ptype2.hd_NUT hd_NUT
#' @export
vec_ptype2.hd_NUT.hd_NUT <- function(x, y, ...) new_NUT()

# NUT and character return double

#' @method vec_ptype2.hd_NUT character
#' @export
vec_ptype2.hd_NUT.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_NUT
#' @export
vec_ptype2.character.hd_NUT <- function(x, y, ...) character()





# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_NUT
#' @export
#' @export vec_cast.hd_NUT
vec_cast.hd_NUT <- function(x, to, ...) UseMethod("vec_cast.hd_NUT")

#' @method vec_cast.hd_NUT default
#' @export
vec_cast.hd_NUT.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Cat to Cat

#' @method vec_cast.hd_NUT hd_NUT
#' @export
vec_cast.hd_NUT.hd_NUT <- function(x, to, ...) x

#' @method vec_cast.hd_NUT character
#' @export
vec_cast.hd_NUT.character <- function(x, to, ...) NUT(x)

#' @method vec_cast.character hd_NUT
#' @export
vec_cast.character.hd_NUT <- function(x, to, ...) vctrs::vec_data(x)


#' #' @method vec_cast.hd_NUT logical
#' #' @export
#' vec_cast.hd_NUT.logical <- function(x, to, ...) NUT(x)
#'
#' #' @method vec_cast.logical hd_NUT
#' #' @export
#' vec_cast.logical.hd_NUT <- function(x, to, ...) NUT(x)
#'



#' @export
as_NUT <- function(x) {
  vctrs::vec_cast(as.character(x), new_NUT())
}


#' @export
NUT_get_stats <-  function(x){
  if(!is_NUT(x)) stop("x must be a Dat")
  NULL
}

