

new_Chk <- function(x = character(),
                    spec = NULL,
                    labels = NULL,
                    skip_stats = FALSE){
  vctrs::vec_assert(x, logical())
  nms <- names(x)
  stats <- NULL
  if(!skip_stats){
    summary <- table(x,useNA = "always") |>
      tibble::as_tibble() |>
      dplyr::mutate(dist = n/sum(n)) |>
      dplyr::rename(category = x)
    if(!all(is.na(x))){
      summary_no_na <- table(x,useNA = "no") |>
        tibble::as_tibble() |>
        dplyr::mutate(dist_no_na = n/sum(n)) |>
        dplyr::rename(category = x) |>
        dplyr::select(-n)
    } else{
      summary_no_na <- tibble::tibble(category = character(0),
                                      dist_no_na = numeric(0))
    }
    summary <- dplyr::left_join(summary, summary_no_na, by = "category") |>
      dplyr::mutate(category = as.logical(category))
    stats <- list(
      n_unique = length(unique(x[!is.na(x)])),
      n_na = sum(is.na(x)),
      pct_na = sum(is.na(x))/length(x),
      summary = summary
    )
  }
  default_spec <- "bool"
  if(!is.null(spec)){
    default_spec <- spec
  }
  value_labels <- set_chk_labels(x, spec = spec, labels = labels)
  values <- tibble::tibble(value = x,
                           label = value_labels)
  vctrs::new_vctr(x,
                  format = list(values = values,
                                n_categories = 2,
                                spec = default_spec),
                  stats = stats,
                  class = "hd_Chk")
}

set_chk_labels <- function(x, spec = NULL, labels = NULL){

  spec <- spec %||% "bool"
  available_Chk_specs <- c("bool", "yesno", "emoji1", "emoji2", "unicode1", "unicode2")
  if(!is.null(spec)){
    if(!spec %in% available_Chk_specs){
      stop("Spec not defined. Available Chk spec: ", available_Chk_specs)
    }
  }
  chk_specs <- tibble::tribble(
    ~value, ~bool, ~yesno, ~emoji1, ~emoji2, ~unicode1, ~unicode2,
    TRUE, TRUE, "Yes", "✔️", "✅",  "✓","✔",
    FALSE,FALSE, "No", "❌",  "❌", "⨯","✖",
  )

  if(!is.null(labels)){
    matches <- labels
  } else{
    matches <- chk_specs |> dplyr::select(all_of(c("value", spec)))
  }
  labels <- dstools::match_replace(x,matches)
  labels

}



#' @export
Chk <- function(x = character(), spec = NULL, labels = NULL, skip_stats = FALSE) {
  # x <- vctrs::vec_cast(x, character())
  x <- as.logical(x)
  new_Chk(x, spec = spec, labels = labels, skip_stats = skip_stats)
}

#' @export
is_Chk <- function(x) {
  inherits(x, "hd_Chk")
}

# Methods

## Format method

#' @export
format.hd_Chk <- function(x, ...) {
  attr(x,"format")$values$label
}

#' @export
vec_ptype_abbr.hd_Chk <- function(x, ...) {
  "Chk"
}

# Coercion


#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Chk
#' @export
#' @export vec_ptype2.hd_Chk
vec_ptype2.hd_Chk <- function(x, y, ...) UseMethod("vec_ptype2.hd_Chk", y)

#' @method vec_ptype2.hd_Chk default
#' @export
vec_ptype2.hd_Chk.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# A Chk combined with a Chk returns a Chk

#' @method vec_ptype2.hd_Chk hd_Chk
#' @export
vec_ptype2.hd_Chk.hd_Chk <- function(x, y, ...) new_Chk()

# Chk and character return character

#' @method vec_ptype2.hd_Chk character
#' @export
vec_ptype2.hd_Chk.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_Chk
#' @export
vec_ptype2.character.hd_Chk <- function(x, y, ...) character()

# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Chk
#' @export
#' @export vec_cast.hd_Chk
vec_cast.hd_Chk <- function(x, to, ...) UseMethod("vec_cast.hd_Chk")

#' @method vec_cast.hd_Chk default
#' @export
vec_cast.hd_Chk.default <- function(x, to, ...) vec_default_cast(x, to)


# Coerce Chk to Chk: TODO need to make sure Cats equivalence
# Ex. Yes/No  yes/no -> Yes/No

#' @method vec_cast.hd_Chk hd_Chk
#' @export
vec_cast.hd_Chk.hd_Chk <- function(x, to, ...) x

#' @method vec_cast.hd_Chk character
#' @export
vec_cast.hd_Chk.character <- function(x, to, ...) Chk(x)

#' @method vec_cast.character hd_Chk
#' @export
vec_cast.character.hd_Chk <- function(x, to, ...) vctrs::vec_data(x)

#' @export
as_Chk <- function(x) {
  vctrs::vec_cast(x, new_Chk())
}


#' @export
Chk_labels <- function(x){
  if(!is_Chk(x)) stop("x must be a Chk")
  attr(x,"format")$values$label
}



#' @export
Chk_get_categories <- function(x){
  if(!is_Chk(x)) stop("x must be a Chk")
  attr(x, "categories")
}

#' @export
Chk_get_n_categories <- function(x){
  if(!is_Chk(x)) stop("x must be a Chk")
  attr(x, "n_categories")
}


#' @export
Chk_format <-  function(x){
  if(!is_Chk(x)) stop("x must be a Chk")
  attr(x, "format")
}

#' @export
Chk_stats <-  function(x){
  if(!is_Chk(x)) stop("x must be a Chk")
  attr(x, "stats")
}



