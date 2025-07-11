% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matomo_utils.R
\name{matomo_emf_visits}
\alias{matomo_emf_visits}
\title{Visit stats for EMF web}
\usage{
matomo_emf_visits(last = 31, period = "day")
}
\arguments{
\item{last}{number of last records to retrieve}

\item{period}{period to retrieve (day, week, month...)}
}
\description{
Connect to matomo API and retrieve EMF web visit stats
}
\details{
This function assumes some environmental variables are available
}
