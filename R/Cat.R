
new_Cat <- function(x = character(),
                    categories = NULL,
                    spec = NULL,
                    skip_stats = FALSE){
  vctrs::vec_assert(x, character())

  if(is.null(categories)){
    categories <- tibble::tibble(category = unique(x[!is.na(x)]),
                         label = unique(x[!is.na(x)]))
  }

  stats <- NULL
  if(!skip_stats){
    summary <- table(x,useNA = "always") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(dist = n/sum(n)) %>%
      dplyr::rename(category = x)
    if(!all(is.na(x))){
      summary_no_na <- table(x,useNA = "no") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(dist_no_na = n/sum(n)) %>%
        dplyr::rename(category = x) |>
        dplyr::select(-n)
    } else{
      summary_no_na <- tibble::tibble(category = character(0),
                                      dist_no_na = numeric(0))
    }
    summary <- dplyr::left_join(summary, summary_no_na, by = "category")
    stats <- list(
      n_unique = length(unique(x[!is.na(x)])),
      n_na = sum(is.na(x)),
      pct_na = sum(is.na(x))/length(x),
      summary = summary
    )
  }

  # Set label spec
  categories <- categories |>
    dplyr::mutate(label = set_cat_spec(label, spec = spec))


  vctrs::new_vctr(x,
                  format = list(categories = categories,
                            n_categories = length(categories$category),
                            spec = spec),
           stats = stats, class = "hd_Cat")
}

set_cat_spec <- function(x, spec = NULL){
  spec <- spec %||% "Title Case"
  available_Cat_specs <- c("Title Case", "lowercase", "UPPERCASE")
  if(!spec %in% available_Cat_specs){
    stop("Spec not defined. Available Chk spec: ", available_Cat_specs)
  }
  makeup::makeup(x, sample = spec)
}



#' @title Category Vectors
#' @description Creates objects of type "hd_Cat". hd_Cat objects contain three main attributes: categories, n_categories and stats. With categories you can check wich values are valid for the variable. With n_categories you can check how many of valid values are valid for the variable. With the last attribute, stats, you can check different basic operations to describe the varaible (n_unique, n_na, pct_na and summary).
#'
#' @param x object to be created as Cat type
#'
#' @param categories an optional character vector of labels for the categories
#' @param skip_stats a logical evaluating to TRUE or FALSE indicating whether variable stats should be calculated and added to the hd_Cat object. The stats are n of unique categories, n of NA values, percentage of NA values and a frequency table.
#'
#' @examples
#' x <- c("Apple","Banana", "Banana", "Lemons", NA)
#' cats <- new_Cat(x)
#' class(cats)
#' attr(cats, "stats")
#' attr(cats, "categories")
#'
#' @export
Cat <- function(x = character(), categories = NULL,
                spec = NULL, skip_stats = FALSE) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  new_Cat(x, categories = categories, spec = spec, skip_stats = skip_stats)
}

#' @title Category Vectors
#' @description test for objects of type "hd_Cat"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hd_Cat or not.
#'
#' @examples
#' some_value <- Cat(c("Cat1", "Cat2"))
#' is_Cat(some_value)
#'
#' @export
is_Cat <- function(x) {
  inherits(x, "hd_Cat")
}

# Methods

## Format method

#' @export
format.hd_Cat <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hd_Cat <- function(x, ...) {
  "Cat"
}

# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Cat
#' @export
#' @export vec_ptype2.hd_Cat
vec_ptype2.hd_Cat <- function(x, y, ...) UseMethod("vec_ptype2.hd_Cat", y)

#' @method vec_ptype2.hd_Cat default
#' @export
vec_ptype2.hd_Cat.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


# A Cat combined with a Cat returns a Cat

#' @method vec_ptype2.hd_Cat hd_Cat
#' @export
vec_ptype2.hd_Cat.hd_Cat <- function(x, y, ...) new_Cat()

# Cat and character return double

#' @method vec_ptype2.hd_Cat character
#' @export
vec_ptype2.hd_Cat.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_Cat
#' @export
vec_ptype2.character.hd_Cat <- function(x, y, ...) character()

# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Cat
#' @export
#' @export vec_cast.hd_Cat
vec_cast.hd_Cat <- function(x, to, ...) UseMethod("vec_cast.hd_Cat")

#' @method vec_cast.hd_Cat default
#' @export
vec_cast.hd_Cat.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Cat to Cat

#' @method vec_cast.hd_Cat hd_Cat
#' @export
vec_cast.hd_Cat.hd_Cat <- function(x, to, ...) x

#' @method vec_cast.hd_Cat character
#' @export
vec_cast.hd_Cat.character <- function(x, to, ...) Cat(x)

#' @method vec_cast.character hd_Cat
#' @export
vec_cast.character.hd_Cat <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as_Cat <- function(x) {
  vctrs::vec_cast(as.character(x), new_Cat())
}


#' @export
Cat_get_categories <- function(x){
  if(!is_Cat(x)) stop("x must be a Cat")
  attr(x, "format")$categories
}

#' @export
Cat_get_n_categories <- function(x){
  if(!is_Cat(x)) stop("x must be a Cat")
  attr(x, "format")$n_categories
}


#' @export
Cat_format <-  function(x){
  if(!is_Cat(x)) stop("x must be a Cat")
  attr(x, "format")
}

#' @export
Cat_stats <-  function(x){
  if(!is_Cat(x)) stop("x must be a Cat")
  attr(x, "stats")
}

