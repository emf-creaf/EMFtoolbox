% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/web_functions.R
\name{update_emf_web}
\alias{update_emf_web}
\title{Update EMF web}
\usage{
update_emf_web(dest)
}
\arguments{
\item{dest}{path to the web server folder}
}
\value{
invisible TRUE if all goes well, error if not
}
\description{
This function retrieves the latest github version of the web and update the
server
}
\details{
In each step of the process, if anything fails an email is send
}