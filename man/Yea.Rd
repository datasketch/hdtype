% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Yea.R
\name{Yea}
\alias{Yea}
\title{Year Vectors}
\usage{
Yea(x = integer())
}
\arguments{
\item{x}{object to be created as Yea type}
}
\description{
Creates objects of type "hd_Yea". hd_Yea objects contain a "stats" attribute, with the following information: NA values (n_na), percentage weight of NA values (prop_na) and if (TRUE or FALSE) the set of given values conform a sequence (is_sequence).
}
\examples{
x <- c(1800:1803, NA)
year_values <- Yea(x)
year_values
class(year_values)
attr(year_values, "stats")$is_sequence

}
