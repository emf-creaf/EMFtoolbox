#' Create and modify metadata files content
#'
#' Create and modify the content for EMF metadata files. Internal templates can be
#' used.
#'
#' The purpose of these functions is to automatically create the contents for the metadata file of any EMF
#' resource
#'
#' @section Templates:
#'   The easiest way to create the metadata fields for any EMF resource is using the corresponding
#'   \code{use_*_yml} function. But in specials cases can be useful to provide a custom template. This
#'   template must be a \code{yml} file with the required metadata fields. Check \code{EMFtoolbox}
#'   predetermined templates, located in the package installation folder, to see examples.
#'
#' @param ... Name-value pairs for metadata fields to modify or add to the metadata file
#' @param .template Template file to use
#' @param .write Logical indicating if the \code{metadata.yml} file must be written (TRUE) or not (FALSE)
#'
#' @return Metadata content ready to copy&paste to the \code{metadata.yml} file printed in the
#' console and returned as a \code{yml} object.
#'
#' @examples
#' use_metadata_yml()
#' use_metadata_yml(emf_type = 'workflow', tags = c('dummy', 'foo'))
#' use_workflow_yml(tags = c('dummy', 'foo'))
#' use_data_yml(tags = c('bar', 'baz'))
#' use_model_yml(edges = c('bar', 'dummy'))
#'
#' @export
use_metadata_yml <- function(
  ...,
  .template = system.file('metadata_templates', 'core.yml', package = 'EMFtoolbox'),
  .write = FALSE
) {

  # empty template
  initial_state <- list('')

  # update template if provided
  if (!is.null(.template)) {
    # get the YAML metadata from provided template
    initial_state <- .template %>%
      rmarkdown::yaml_front_matter()
  }

  # We need the lists converted to yml, add/modify fields and copy to the clipboard
  metadata_yml <- ymlthis::as_yml(initial_state) %>%
    ymlthis::yml_replace(...) %>%
    suppressMessages(ymlthis::use_yml())

  # if write is TRUE, write it and return it, if not, just return it
  if (isTRUE(.write)) {
    ymlthis::use_yml_file(metadata_yml, 'metadata.yml', build_ignore = TRUE)
  }

  return(metadata_yml)
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'workflow.yml', package = 'EMFtoolbox')}
#'
#' @export
use_workflow_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'workflow.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'tech_doc.yml', package = 'EMFtoolbox')}
#'
#' @export
use_techdoc_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'tech_doc.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'data.yml', package = 'EMFtoolbox')}
#'
#' @export
use_data_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'data.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'model.yml', package = 'EMFtoolbox')}
#'
#' @export
use_model_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'model.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'softwork.yml', package = 'EMFtoolbox')}
#'
#' @export
use_softwork_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'softwork.yml', package = 'EMFtoolbox'),
    .write = .write
  )
}


#' Collect metadata from yml files
#'
#' Collect metadata from yml files
#'
#' This function will collect the metadata present in the current project (via the \code{metadata.yml} file)
#' and update the metadata database.
#'
#' @param con Connection to the metadata database (DBI or pool object)
#' @param .dry Logical indicating if this is a dry-run (no permanent modification of the database is done) or
#'   not (permanent modification of the database is done by adding the metadata).
#'
#' @return Invisible TRUE if everything goes correctly.
#'
#' @examples
#' collect_metadata(con = emf_database, .dry = TRUE)
#' collect_metadata(con = emf_database, .dry = FALSE)
#'
#' @export
collect_metadata <- function(con, .dry = TRUE) {

  # collect the metadata
  metadata_yml <- read_metadata_file()

  # if draft, dry
  if (isTRUE(metadata_yml$emf_draft)) {
    message("This resource is in draft mode, forcing a dry metadata collection.")
    .dry <- TRUE
  }

  # if dry, return the metadata and exit
  if (isTRUE(.dry)) {
    return(metadata_yml)
  } else {
    message("Updating metadata database...\n")
  }

  # prepare the updating tables
  message("  - reading and preparing the updated tables\n")
  update_tables_list <- prepare_update_metadata_tables(metadata_yml)

  # check if the new data is the same as the old data
  update_info <- compare_metadata_tables(update_tables_list, con, metadata_yml$id)
  valid_update_list <- update_info$valid_update_list

  # prepare and execute the queries to insert if don't exists or update if exists.
  message("  - updating the following tables:\n")
  message(glue::glue("    {names(update_tables_list[valid_update_list])}\n"))
  update_metadata_queries(update_tables_list, update_info, con, metadata_yml)

  # check the db is correctly updated (if not dry)
  message("  - checking if updating went well\n")
  final_check <- compare_metadata_tables(update_tables_list, con, metadata_yml$id)$valid_update_list

  if (any(final_check)) {
    ## TODO delete from resources con cascade para eliminar lo insertado si ha habido algún problema
    stop("Some tables have not been updated, please check the database")
  }

  return(invisible(TRUE))

}


# Read the metadata.yml file in the project.
read_metadata_file <- function(yml_file = './metadata.yml') {
  readLines(yml_file) %>%
    glue::glue_collapse(sep = '\n') %>%
    ymlthis::as_yml()
}

# Prepare the update tables
prepare_update_metadata_tables <- function(metadata_yml) {

  # prepare the updating tables
  update_tables_list <- list(
    resources_update_table = metadata_yml[
      !names(metadata_yml) %in% c('requirements', 'authors', 'authors_aff', 'tags', 'edges')
    ] %>%
      tibble::as_tibble(),

    tags_update_table = tibble::tibble(
      tag = metadata_yml$tags,
      id = metadata_yml$id
    ),

    edges_update_table = tibble::tibble(
      edge = metadata_yml$edges,
      id = metadata_yml$id
    ),

    authors_update_table = tibble::tibble(
      author = metadata_yml$authors,
      author_aff = metadata_yml$authors_aff,
      id = metadata_yml$id
    ),

    requirements_update_table = tibble::tibble(
      requirement = metadata_yml$requirements,
      id = metadata_yml$id
    )
  )

  return(update_tables_list)
}

# Check if updated and database are the same
compare_metadata_tables <- function(update_tables_list, con, resource_id) {

  # check if the new data is the same as the old data
  db_tables_list <- list(
    resources_old_table = dplyr::tbl(con, 'resources') %>%
      dplyr::filter(id == resource_id) %>%
      dplyr::mutate(date = as.character(date), date_lastmod = as.character(date_lastmod)) %>%
      dplyr::collect(),
    tags_old_table = dplyr::tbl(con, 'tags') %>%
      dplyr::filter(id == resource_id) %>%
      dplyr::select(-tag_id) %>%
      dplyr::collect(),
    edges_old_table = dplyr::tbl(con, 'edges') %>%
      dplyr::filter(id == resource_id) %>%
      dplyr::select(-edge_id) %>%
      dplyr::collect(),
    authors_old_table = dplyr::tbl(con, 'authors') %>%
      dplyr::filter(id == resource_id) %>%
      dplyr::select(-author_id) %>%
      dplyr::collect(),
    requirements_old_table = dplyr::tbl(con, 'requirements') %>%
      dplyr::filter(id == resource_id) %>%
      dplyr::select(-requirement_id) %>%
      dplyr::collect()
  )

  valid_update_list <- purrr::map2_lgl(
    update_tables_list, db_tables_list,
    ~ dplyr::if_else(is.logical(dplyr::all_equal(.x, .y)), FALSE, TRUE)
  )

  resources_columns_to_add <-
    names(update_tables_list$resources_update_table)[
      which(!names(update_tables_list$resources_update_table) %in% names(db_tables_list$resources_old_table))
    ]

  resources_columns_to_add_type <-
    sapply(update_tables_list$resources_update_table, class)[resources_columns_to_add] %>%
    translate_r2sql_types()



  res <- list(
    valid_update_list = valid_update_list,
    resources_columns_to_add = resources_columns_to_add,
    resources_columns_to_add_type = resources_columns_to_add_type
  )
  return(res)
}

update_metadata_queries <- function(update_tables_list, update_info, con, metadata_yml) {
  # prepare the valid update list
  valid_update_list <- c(
    update_info$valid_update_list[1],
    update_info$valid_update_list[2:5],
    update_info$valid_update_list[2:5]
  )

  # Before to prepare queries and update tables, we must check if we are adding new columns to the resources
  # table (new metadata fields not present yet in the table)
  if (length(update_info$resources_columns_to_add) > 0) {
    alter_resources_query <- glue::glue_sql(
      "ALTER TABLE resources
          {columns_to_add*};",
      columns_to_add = glue::glue_sql(
        glue::glue_collapse(glue::glue('{glue::glue_sql(
        "ADD COLUMN {`update_info$resources_columns_to_add`}",
        .con = con
      )} {update_info$resources_columns_to_add_type}'), ', '), .con = con
      ),
      .con = con
    )
    DBI::dbExecute(con, alter_resources_query)
  }


  # prepare the queries to insert if don't exists or update if exists.
  # This is tricky. Child tables don't have a unique id for the resource (they have with the *_id (i.e tag_id)
  # column, but we can not know that beforehand). Also, for example, for tags, we maybe want to remove some
  # tags and create other ones. So, for the resources table, must be an "upsert", inserting or updating the
  # resource, but for the child tables (tags, requirements, edges, authors...) must be a two-step process,
  # first deleting the old records and then creating the new ones.
  # But this must be done only in the tables that have updates, to avoid extra overhead in the db.
  resources_upsert_query <- glue::glue_sql(
    "INSERT INTO resources ({`columns`*}) VALUES ({values*})
          ON CONFLICT (id) DO UPDATE SET {insert_resources_subquery*};",
    columns = names(update_tables_list$resources_update_table),
    values = as.list(dplyr::slice(update_tables_list$resources_update_table, 1)),
    insert_resources_subquery = glue::glue_sql(
      "{`names(update_tables_list$resources_update_table)`} = {as.list(dplyr::slice(update_tables_list$resources_update_table, 1))}",
      .con = con
    ),
    .con = con
  )

  delete_child_tables_queries <- list(
    delete_old_tags = glue::glue_sql(
      "DELETE FROM tags WHERE id = {metadata_yml$id};",
      .con = con
    ),
    delete_old_edges = glue::glue_sql(
      "DELETE FROM edges WHERE id = {metadata_yml$id};",
      .con = con
    ),
    delete_old_authors = glue::glue_sql(
      "DELETE FROM authors WHERE id = {metadata_yml$id};",
      .con = con
    ),
    delete_old_requirements = glue::glue_sql(
      "DELETE FROM requirements WHERE id = {metadata_yml$id};",
      .con = con
    )
  )

  insert_child_tables_queries <- list(
    insert_new_tags = glue::glue_sql(
      "INSERT INTO tags (tag, id) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$tags_update_table$tag}, {update_tables_list$tags_update_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_new_edges = glue::glue_sql(
      "INSERT INTO edges (edge, id) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$edges_update_table$edge}, {update_tables_list$edges_update_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_new_authors = glue::glue_sql(
      "INSERT INTO authors (author, author_aff, id) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$authors_update_table$author}, {update_tables_list$authors_update_table$author_aff}, {update_tables_list$authors_update_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_new_requirements = glue::glue_sql(
      "INSERT INTO requirements (requirement, id) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$requirements_update_table$requirement}, {update_tables_list$requirements_update_table$id})",
        .con = con
      ),
      .con = con
    )
  )

  # run the queries, but only for the tables that must be updated
  c(resources_upsert_query, delete_child_tables_queries, insert_child_tables_queries)[valid_update_list] %>%
    purrr::walk(
      ~ DBI::dbExecute(con, .x)
    )

  return(invisible(TRUE))
}

translate_r2sql_types <- function(types) {
  dplyr::case_when(
    types == 'character' ~ 'TEXT',
    types == 'integer' ~ 'INT',
    types %in% c('numeric', 'double') ~ 'FLOAT',
    types == 'logical' ~ 'BOOL',
    TRUE ~ 'TEXT'
  )
}
