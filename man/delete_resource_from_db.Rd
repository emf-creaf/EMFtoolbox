% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_functions.R
\name{delete_resource_from_db}
\alias{delete_resource_from_db}
\title{Delete resource from db by id}
\usage{
delete_resource_from_db(resource_id, con = NULL)
}
\arguments{
\item{resource_id}{character with the resource id to delete}

\item{con}{pool object connected to the database}
}
\value{
invisible TRUE if success. Invisible FALSE if deletion fails (no id found).
}
\description{
Delete resource form db by id
}
\details{
This function will delete a resource by id. If not connection is provided, it automatically
connect to db and defer the closing of the connection
}
