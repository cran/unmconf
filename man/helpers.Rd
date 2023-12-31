% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{helpers}
\alias{helpers}
\alias{expand_labels}
\alias{greek_expander}
\alias{make_greek_coefs}
\alias{drop_nulls}
\title{Convert to Greek expressions}
\usage{
expand_labels(labs)

greek_expander(s)

make_greek_coefs(mod)

drop_nulls(x)
}
\arguments{
\item{labs}{A character vector of greek symbols of the form \code{ga_x} and \code{be_1}.}

\item{s}{A character vector of Greek short hand codes, e.g. \code{"si"}.}

\item{mod}{Output from \code{\link[=unm_glm]{unm_glm()}}.}

\item{x}{Character vector.}
}
\value{
A character vector.
}
\description{
Convert to Greek expressions for plotting
}
\examples{

labs <- c("ga_1", "ga_treatment", "ga_x", "be_1",
  "be_treatment", "be_x", "la_u", "al_y", "si")
expand_labels(labs)


}
