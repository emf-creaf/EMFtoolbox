% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_functions.R
\name{public_softworks}
\alias{public_softworks}
\title{Retrieve the public softworks table.}
\usage{
public_softworks(...)
}
\arguments{
\item{...}{arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
specified and error is raised.}
}
\value{
A tibble with the public resources table queried
}
\description{
Retrieve the public softworks table.
}
\examples{
use_public_table('softworks')

}
