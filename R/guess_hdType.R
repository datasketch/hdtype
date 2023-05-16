
#' @title hdtype
#' @description evaluates which of the available_hdtypes is the object
#'
#' @param v a value for which you want to evaluate
#'
#' @return An available_hdtype value
#'
#' @examples
#'
#' # Num hdtype
#' value <- c("1",NA,"2")
#' guess_hdtype(value)
#'
#' # Cat hdtype
#' value <- c("MSPS-CD-166-2020", "003-2020", "0811 - 2020")
#' guess_hdtype(value)
#'
#' # Pct hdtype
#' value <- c(0.3, 0.4, 1)
#' guess_hdtype(value)
#'
#'
#' @export
guess_hdtype <- function(v){
  # if("data.frame" %in% class(v))
  #   v <- v %>% flatten

  if(is_any_hdtype(v)){
    return(hdtype(which_hdtype(v)))
  }

  if(length(v) == 0){
    return(hdtype("NUT"))
  }
  if(all(is.na(v))){
    return(hdtype("UKT"))
  }

  v <- unique(v[!is.na(v)])


  if(any(class(v) %in% c("integer","numeric"))){
    hdtype <- hdtype("Num")
    if(all(v %in% 1500L:2200L)) hdtype <- hdtype("Yea")
    if(maybePct(v)) hdtype <- hdtype("Pct")
    return(hdtype)
  }
  if(any(class(v) == "Date") | any(c("POSIXt", "POSIXct") %in% class(v)))
    return(hdtype("Dat"))

  if(class(v)!= "factor" & !has_warning(as.numeric(v))){
    return(hdtype("Num"))
  }

  #dth <- whichDTH(v)
  # if(!is.null(dth))
  #   hdtype <- hdtype(dth)
  if(isDate(v)){
    return(hdtype("Dat"))
  }
  else{
    v <- as.character(v)
    if(maybeNum(v)){
      return(hdtype("Num"))
    }
    if(maybePct(v)){
      return(hdtype("Pct"))
    }
    hdtype <- hdtype("Cat")
    if(hdtype == hdtype("Cat") && maybeImgUrl(v)){
      hdtype <- hdtype("Img")
    }
    if(hdtype == hdtype("Cat") && maybeTxt(v))
      hdtype <- hdtype("Txt")
  }
  hdtype
}

maybeNum <- function(v){
  v0 <- gsub(",",".",v)
  v0 <- gsub("\\.","",v0)

  nums <- tryCatch(as.numeric(v0),
                   error=function(e) e, warning=function(w) w
                   )
  if(inherits(nums, "warning")){
    return(FALSE)
  }
  if(dstools::na_proportion(nums) > 0.8){
    return(FALSE)
  }
  TRUE
}

has_decimal_comma <- function(v){
  v0 <- gsub("[0-9\\.]","", v)
  v1 <- gsub("-", "", v0)
  #has_commas <- grepl(",",v0)
  has_other_punct <- grepl("([-])|[[:punct:]]", gsub(",","", v1))
  if(any(has_other_punct)) return(FALSE)
  TRUE
}


maybePct <- function(v){
  if(is.numeric(v)){
    between_0_1_inc <- all(purrr::map_lgl(v, ~ all(. >= 0 && . <= 1)))
    between_0_1_exc <- any(purrr::map_lgl(v, ~ all(. > 0 && . < 1)))
    return(between_0_1_inc && between_0_1_exc)
  }
  if(is.character(v)){
    return(all(grepl("([^%]*%[^%]*[0-9]+)|([0-9]+[^%]*%.*)", v[!is.na(v)])))
    # return(all(grepl("%", v[!is.na(v)])))
  }
}

maybeImgUrl <- function(x) all(grepl("^[http].+\\.((?i)jpg|png|gif|bmp|svg)$", x))

maybeTxt <- function(v){
  nwords <- function(x) vapply(strsplit(x, "[[:punct:] [^/]]"), length, integer(1))
  any(nchar(v) > 100) && any(nwords(v)>10)
}

