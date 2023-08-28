
new_Clr <- function(x = character(),
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
    summary <- table(x,useNA = "always") |>
      tibble::as_tibble() |>
      dplyr::mutate(dist = n/sum(n)) |>
      dplyr::rename(category = x)
    if(!all(is.na(x))){
      summary_no_na <- table(x,useNA = "no") |>
        tibble::as_tibble() %>%
        dplyr::mutate(dist_no_na = n/sum(n))  |>
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
           stats = stats, class = "hd_Clr")
}

set_clr_spec <- function(x, spec = NULL){
  spec <- spec %||% "hex"
  available_Clr_specs <- c("hex")
  if(!spec %in% available_Clr_specs){
    stop("Spec not defined. Available Chk spec: ", available_Clr_specs)
  }
  x
}



#' @title Clregory Vectors
#' @description Creates objects of type "hd_Clr". hd_Clr objects contain three main attributes: categories, n_categories and stats. With categories you can check wich values are valid for the variable. With n_categories you can check how many of valid values are valid for the variable. With the last attribute, stats, you can check different basic operations to describe the varaible (n_unique, n_na, pct_na and summary).
#'
#' @param x object to be created as Clr type
#'
#' @param categories an optional character vector of labels for the categories
#' @param skip_stats a logical evaluating to TRUE or FALSE indicating whether variable stats should be calculated and added to the hd_Clr object. The stats are n of unique categories, n of NA values, percentage of NA values and a frequency table.
#'
#' @examples
#' x <- c("Apple","Banana", "Banana", "Lemons", NA)
#' cats <- new_Clr(x)
#' class(cats)
#' attr(cats, "stats")
#' attr(cats, "categories")
#'
#' @export
Clr <- function(x = character(), categories = NULL,
                spec = NULL, skip_stats = FALSE) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.character(x)
  if(!paletero::are_colors(x))
    stop("x are not colors")
  new_Clr(x, categories = categories, spec = spec, skip_stats = skip_stats)
}

#' @title Clregory Vectors
#' @description test for objects of type "hd_Clr"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hd_Clr or not.
#'
#' @examples
#' some_value <- Clr(c("Clr1", "Clr2"))
#' is_Clr(some_value)
#'
#' @export
is_Clr <- function(x) {
  inherits(x, "hd_Clr")
}

# Methods

## Format method

#' @export
format.hd_Clr <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hd_Clr <- function(x, ...) {
  "Clr"
}

# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Clr
#' @export
#' @export vec_ptype2.hd_Clr
vec_ptype2.hd_Clr <- function(x, y, ...) UseMethod("vec_ptype2.hd_Clr", y)

#' @method vec_ptype2.hd_Clr default
#' @export
vec_ptype2.hd_Clr.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


# A Clr combined with a Clr returns a Clr

#' @method vec_ptype2.hd_Clr hd_Clr
#' @export
vec_ptype2.hd_Clr.hd_Clr <- function(x, y, ...) new_Clr()

# Clr and character return double

#' @method vec_ptype2.hd_Clr character
#' @export
vec_ptype2.hd_Clr.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_Clr
#' @export
vec_ptype2.character.hd_Clr <- function(x, y, ...) character()

# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Clr
#' @export
#' @export vec_cast.hd_Clr
vec_cast.hd_Clr <- function(x, to, ...) UseMethod("vec_cast.hd_Clr")

#' @method vec_cast.hd_Clr default
#' @export
vec_cast.hd_Clr.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Clr to Clr

#' @method vec_cast.hd_Clr hd_Clr
#' @export
vec_cast.hd_Clr.hd_Clr <- function(x, to, ...) x

#' @method vec_cast.hd_Clr character
#' @export
vec_cast.hd_Clr.character <- function(x, to, ...) Clr(x)

#' @method vec_cast.character hd_Clr
#' @export
vec_cast.character.hd_Clr <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as_Clr <- function(x) {
  vctrs::vec_cast(as.character(x), new_Clr())
}


#' @export
Clr_get_categories <- function(x){
  if(!is_Clr(x)) stop("x must be a Clr")
  attr(x, "format")$categories
}

#' @export
Clr_get_n_categories <- function(x){
  if(!is_Clr(x)) stop("x must be a Clr")
  attr(x, "format")$n_categories
}


#' @export
Clr_format <-  function(x){
  if(!is_Clr(x)) stop("x must be a Clr")
  attr(x, "format")
}

#' @export
Clr_stats <-  function(x){
  if(!is_Clr(x)) stop("x must be a Clr")
  attr(x, "stats")
}

