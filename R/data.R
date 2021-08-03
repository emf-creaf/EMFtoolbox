#' Metadata definitions table.
#'
#' A dataframe containing the metadata fields available in the EMFverse.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{Field}{ID of the metadata field}
#'   \item{Scopes}{Reources the metadata is intended for}
#'   \item{Categories}{Metadata categories}
#'   \item{Definition}{Metadata field definition}
#'   \item{Allowed values}{Allowed values for the metadata field, if needed}
#'   \item{Example value}{Example value for those fields that don't have fixed values described in \code{Allowed values}}
#' }
"metadata_table"
