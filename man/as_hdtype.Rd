% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdType.R
\name{as_hdtype}
\alias{as_hdtype}
\title{hdtype Vectors}
\usage{
as_hdtype(x)
}
\arguments{
\item{x}{object to be coerced}
}
\value{
attempts to coerce its argument to hdtype type
}
\description{
coerces its argument to a hdtype It is an abbreviated form of hdtype
}
\examples{

some_chr_value <- "Cat"
class(some_chr_value)

some_hdt_value <- as_hdtype(some_chr_value)
class(some_hdt_value)

}
