% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_functions.R
\name{internal_pipelines}
\alias{internal_pipelines}
\title{Retrieve the public pipelines table.}
\usage{
internal_pipelines(...)
}
\arguments{
\item{...}{arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
specified and error is raised.}
}
\value{
A tibble with the public resources table queried
}
\description{
Retrieve the public pipelines table.
}
\examples{
use_public_table('pipelines')

}
