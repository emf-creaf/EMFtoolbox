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
#' use_model_yml(nodes = c('bar', 'dummy'))
#'
#' @export
use_metadata_yml <- function(
  ...,
  .template = system.file('metadata_templates', 'core.yml', package = 'EMFtoolbox'),
  .write = FALSE
) {

  # empty template
  initial_state <- list()

  # update template if provided
  if (!is.null(.template)) {
    # get the YAML metadata from provided template
    initial_state <- read_metadata_file(.template)
  }

  # We need the lists converted to yml, add/modify fields and copy to the clipboard
  metadata_yml <- initial_state |>
    ymlthis::yml_replace(...) |>
    suppressMessages(ymlthis::use_yml())

  ## Ensure date is always set
  if (metadata_yml$date == '') {
    metadata_yml$date <- as.character(Sys.Date())
  }

  if (metadata_yml$date_lastmod == '') {
    metadata_yml$date_lastmod <- as.character(Sys.Date())
  }

  # if write is TRUE, write it and return it, if not, just return it
  if (isTRUE(.write)) {
    ymlthis::use_yml_file(metadata_yml, 'metadata.yml', build_ignore = TRUE)
  }

  return(metadata_yml)
}

#' @describeIn use_metadata_yml Equivalent to using \code{use_metadata_yml} with
#'   \code{.template = system.file('metadata_templates', 'pipeline.yml', package = 'EMFtoolbox')}
#'
#' @export
use_pipeline_yml <- function(..., .write = FALSE) {
  use_metadata_yml(
    ...,
    .template = system.file('metadata_templates', 'pipeline.yml', package = 'EMFtoolbox'),
    .write = .write
  )
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
use_tech_doc_yml <- function(..., .write = FALSE) {
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
#' @param ... arguments passed to \code{\link{read_metadata_file}}
#'
#' @return Invisible TRUE if update is needed and performed correctly, invisible FALSE if update is not needed
#'   and process is performed correctly. Metadata \code{yml} object if \code{.dry} i TRUE. Error if the update
#'   failed and there is a mismatch between the updated db tables and the metadata file.
#'
#' @examples
#' collect_metadata(con = emf_database, .dry = TRUE)
#' collect_metadata(con = emf_database, .dry = FALSE)
#'
#' @export
collect_metadata <- function(con = NULL, ..., .dry = TRUE) {

  # collect the metadata
  metadata_yml <- read_metadata_file(...)

  # if draft, dry
  if (isTRUE(metadata_yml$emf_draft)) {
    cli::cli_alert_warning("This resource is in draft mode, forcing a dry metadata collection.")
    # usethis::ui_warn("This resource is in draft mode, forcing a dry metadata collection.")
    .dry <- TRUE
  }

  # if dry, return the metadata and exit
  if (isTRUE(.dry)) {
    return(metadata_yml)
  } else {
    cli::cli_alert_info("Collecting {metadata_yml$id}:\n")
    # usethis::ui_info("Collecting {metadata_yml$id}:\n")
  }

  # create the connection if con = null
  # (here because this way there is no connection if dry is TRUE)
  if (is.null(con)) {
    # connect to database
    con <- metadata_db_con()
  }

  # prepare the updating tables
  cli::cli_alert_info("  - reading and preparing the updated tables\n")
  # usethis::ui_info("  - reading and preparing the updated tables\n")
  update_tables_list <- prepare_update_metadata_tables(metadata_yml)

  # check if the new data is the same as the old data
  update_info <- compare_metadata_tables(update_tables_list, con, metadata_yml$id)
  valid_update_list <- update_info$valid_update_list
  if (!any(valid_update_list)) {
    cli::cli_alert_success("No updates needed, exiting.")
    # usethis::ui_done("No updates needed, exiting.")
    return(invisible(FALSE))
  }

  # prepare and execute the queries to insert if don't exists or update if exists.
  cli::cli_alert_info("- updating the following tables:\n")
  # usethis::ui_info("- updating the following tables:\n")
  cli::cli_ul(names(update_tables_list[valid_update_list]))
  # names(update_tables_list[valid_update_list]) |>
  #   purrr::walk(usethis::ui_todo)
  update_metadata_queries(update_tables_list, update_info, con, metadata_yml)

  ## TODO this final check logic must be inside `update_metadata_queries`, and in case of error, it should
  ## not remove all the resource from the db, but reverting the changes instead (if it was a new resource
  ## then deleting is fine)
  # check the db is correctly updated (if not dry)
  # usethis::ui_info("- checking if the update went well\n")
  # final_check <- compare_metadata_tables(update_tables_list, con, metadata_yml$id)$valid_update_list
  #
  # if (any(final_check)) {
  #   delete_resource_from_db(metadata_yml$id, con)
  #   usethis::ui_stop(
  #     "Something happened when updating the database. Removing {metadata_yml$id} from the resources and children tables."
  #   )
  # }

  cli::cli_alert_success("Everything ok.")
  # usethis::ui_done("Everything ok.")
  return(invisible(TRUE))
}

#' Collect metadata for external models
#'
#' Collect metadata for external models
#'
#' This function will collect the metadata present in the selected excel file.
#'
#' @param con Connection to the metadata database (DBI or pool object)
#' @param .dry Logical indicating if this is a dry-run (no permanent modification of the database is done) or
#'   not (permanent modification of the database is done by adding the metadata).
#' @param ... arguments passed to \code{\link{external_models_transform}}
#'
#' @return Invisible TRUE if update is needed and performed correctly, invisible FALSE if update is not needed
#'   and process is performed correctly. Metadata \code{yml} object if \code{.dry} i TRUE. Error if the update
#'   failed and there is a mismatch between the updated db tables and the metadata file.
#'
#' @examples
#' collect_metadata_external_models(con = emf_database, .dry = TRUE)
#'
#' @export
collect_metadata_external <- function(external_type, con = NULL, ..., .dry = TRUE) {
  # load the original table and transform it
  external_metadata <- switch(
    external_type,
    "models" = external_models_transform(...),
    "data" = external_data_transform(...)
  )

  # Iterate **safely** by rows calling collect_metadata. slider::slide does not accept
  # safe versions of the function, so we are gonna use the ol'good map
  # create a safe version of collect_metadata
  safe_collect_metadata <- purrr::safely(collect_metadata)

  # safely loop the ext_models metadata rows
  updated_external <-
    1:nrow(external_metadata) |>
    purrr::map(~ safe_collect_metadata(con = con, .dry = .dry, yml_file = external_metadata[.x,]))

  return(purrr::map(updated_external, 'result'))

}

#' Collect metadata for creaf data and models resources
#'
#' Collect metadata for creaf data and models resources
#'
#' This function will collect the metadatas present in the selected folder.
#'
#' @param ... arguments passed to \code{\link{collect_metadata}}
#'
#' @return Invisible TRUE if update is needed and performed correctly, invisible FALSE if update is not needed
#'   and process is performed correctly. Metadata \code{yml} object if \code{.dry} i TRUE. Error if the update
#'   failed and there is a mismatch between the updated db tables and the metadata file.
#'
#' @examples
#' collect_metadata_creaf(con = emf_database, .dry = TRUE)
#'
#' @export
collect_metadata_creaf <- function(con = NULL, .dry = TRUE) {
  # Iterate **safely** by rows calling collect_metadata. slider::slide does not accept
  # safe versions of the function, so we are gonna use the ol'good map
  # create a safe version of collect_metadata
  safe_collect_metadata <- purrr::safely(collect_metadata)

  # safely loop the folders in the respository
  updated_creaf <- fs::dir_ls(recurse = TRUE, regexp = "metadata\\.yml$") |>
    purrr::map(~ safe_collect_metadata(con = con, yml_file = .x, .dry = .dry))

  return(purrr::map(updated_creaf, 'result'))
}


#' Read the metadata.yml file in the project
#'
#' Load yml from file or metadata tibbles
#'
#' This function takes a \code{yml} file or a metadata tibble and converts it
#' to an \code{\link[ymlthis]{yml}} object
#'
#' @param yml_file If a character, is interpreted as a path to a file containing
#'   the metadata in yml format. If a tibble, is interpreted as a tibble
#'   containing the metadata fields (external resources have this)
#'
#' @return An \code{\link[ymlthis]{yml}} object with the metadata
#'
#' @noRd
read_metadata_file <- function(yml_file = './metadata.yml') {

  if(is.character(yml_file)) {
    readLines(yml_file) |>
      glue::glue_collapse(sep = '\n') |>
      ymlthis::as_yml()
  } else {
    yml_file |>
      dplyr::mutate(tags = purrr::map(tags, ~ purrr::discard(.x, .p = is.na))) |>
      purrr::flatten() |>
      ymlthis::as_yml()
  }

}

#' Prepare the update tables
#'
#' Build the tables for updating the metadata database based on the yml metadata
#'
#' Build and prepare the updated values for the db tables
#'
#' @param metadata_yml \code{yml} object with the resource metadata
#'
#' @return a named list of tibbles with update tables
#'
#' @noRd
prepare_update_metadata_tables <- function(metadata_yml) {

  # prepare the updating tables
  update_tables_list <- list(
    resources_update_table = metadata_yml[
      !names(metadata_yml) %in%
        c('authors', 'authors_aff', 'tags', 'requirements', 'nodes', 'links')
    ] |>
      tibble::as_tibble(),

    # tags_update_table = tibble::tibble(
    #   tag_id = metadata_yml$tags,
    #   tag_description = ''
    # ),

    resource_tags_update_table = tibble::tibble(
      id = metadata_yml$id,
      tag_id = metadata_yml$tags
    ),

    nodes_update_table = tibble::tibble(
      node = metadata_yml$nodes,
      id = metadata_yml$id
    ),

    # authors_update_table = tibble::tibble(
    #   author_id = names(metadata_yml$authors),
    #   name = purrr::map_chr(metadata_yml$authors, "name"),
    #   surname = purrr::map_chr(metadata_yml$authors, "surname"),
    #   aff = purrr::map_chr(metadata_yml$authors, "aff"),
    #   aff_link = purrr::map_chr(metadata_yml$authors, "aff_link"),
    #   summary = purrr::map_chr(metadata_yml$authors, "summary")
    # ),

    resource_authors_update_table = tibble::tibble(
      id = metadata_yml$id,
      author_id = metadata_yml$authors
    ),

    requirements_update_table = tibble::tibble(
      requirement = metadata_yml$requirements,
      id = metadata_yml$id
    ),

    links_update_table = tibble::tibble(
      id = metadata_yml$id,
      url_pdf = ifelse(
        is_na_or_null(metadata_yml$links$url_pdf),
        NA_character_, metadata_yml$links$url_pdf
      ),
      url_doi = ifelse(
        is_na_or_null(metadata_yml$links$url_doi),
        NA_character_, metadata_yml$links$url_doi
      ),
      url_docs = ifelse(
        is_na_or_null(metadata_yml$links$url_docs),
        NA_character_, metadata_yml$links$url_docs
      ),
      url_source = ifelse(
        is_na_or_null(metadata_yml$links$url_source),
        NA_character_, metadata_yml$links$url_source
      )
    )
  )

  return(update_tables_list)
}

#' Check if updated and database are the same
#'
#' Compare the update list with the tables in the db for a given resource
#'
#' This function takes a list of tibbles as the one returned by
#' \code{\link{prepare_update_metadata_tables}} and compare those tables with
#' the ones present in the database.
#'
#' @param update_tables_list A list of tibbles as the one returned by
#'   \code{\link{prepare_update_metadata_tables}}
#' @param con database connection
#' @param resource_id ID of the resource to compare tables for
#'
#' @return A list with three elements, \code{valid_update_list}  a logical vector
#'   with the tables that need update (differences are found).
#'   \code{resource_columns_to_add} with names of columns that need to be added
#'   to the db (new metadata fields).
#'   \code{resource_columns_to_add_type} with type of columns that need to be added
#'   to the db (new metadata fields).
#'
#' @noRd
compare_metadata_tables <- function(update_tables_list, con, resource_id) {

  # check if the new data is the same as the old data
  db_tables_list <- list(
    resources_old_table = dplyr::tbl(con, 'resources') |>
      dplyr::filter(id == resource_id) |>
      dplyr::mutate(date = as.character(date), date_lastmod = as.character(date_lastmod)) |>
      dplyr::select(dplyr::any_of(names(update_tables_list$resources_update_table))) |>
      dplyr::collect(),
    tags_old_table = dplyr::tbl(con, 'resource_tags') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-resource_tags_pk) |>
      dplyr::collect(),
    nodes_old_table = dplyr::tbl(con, 'nodes') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-node_pk) |>
      dplyr::collect(),
    authors_old_table = dplyr::tbl(con, 'resource_authors') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-resource_authors_pk) |>
      dplyr::collect(),
    requirements_old_table = dplyr::tbl(con, 'requirements') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-requirement_pk) |>
      dplyr::collect(),
    links_old_table = dplyr::tbl(con, 'links') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(dplyr::any_of(names(update_tables_list$links_update_table)), -link_pk) |>
      dplyr::collect()
  )

  # browser()
  valid_update_list <- purrr::map2_lgl(
    update_tables_list, db_tables_list,
    # dplyr::all_equal is deprecated, I need to find a correct solution
    # ~ dplyr::if_else(is.logical(suppressWarnings(dplyr::all_equal(.x, .y))), FALSE, TRUE)
    # ~ dplyr::if_else(is.logical(all.equal(.x, .y)), FALSE, TRUE)
    .f = \(update_table, db_table) {
      update_table <- update_table[, sort(names(update_table))]
      db_table <- db_table[, sort(names(db_table))]
      return(dplyr::if_else(is.logical(all.equal(update_table, db_table)), FALSE, TRUE))
    }
  )

  resources_columns_to_add <-
    names(update_tables_list$resources_update_table)[
      which(!names(update_tables_list$resources_update_table) %in% names(db_tables_list$resources_old_table))
    ]

  resources_columns_to_add_type <-
    sapply(update_tables_list$resources_update_table, class)[resources_columns_to_add] |>
    translate_r2sql_types()

  res <- list(
    valid_update_list = valid_update_list,
    resources_columns_to_add = resources_columns_to_add,
    resources_columns_to_add_type = resources_columns_to_add_type
  )
  return(res)
}

#' Build and execute the update metadata queries
#'
#' Build queries to upsert, remove and update the metadata for a given resource
#'
#' This function build the queries, execute them, and if something goes wrong,
#' go back to the previous values
#'
#' @param update_tables_list A list of tibbles as the one returned by
#'   \code{\link{prepare_update_metadata_tables}}
#' @param update_info A list returned by \code{\link{compare_metadata_tables}}
#' @param con db connection
#' @param metadata_yml \code{yml} object with the resource metadata
#'
#' @return invisible TRUE if everything goes right
#'
#' @noRd
update_metadata_queries <- function(update_tables_list, update_info, con, metadata_yml) {
  # First of all, create a backup of the actual tables for the resource at hand
  # backup tables
  resource_id <- metadata_yml$id
  backup_tables_list <- list(
    resources_backup_table = dplyr::tbl(con, 'resources') |>
      dplyr::filter(id == resource_id) |>
      dplyr::mutate(date = as.character(date), date_lastmod = as.character(date_lastmod)) |>
      dplyr::collect(),
    tags_backup_table = dplyr::tbl(con, 'resource_tags') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-resource_tags_pk) |>
      dplyr::collect(),
    nodes_backup_table = dplyr::tbl(con, 'nodes') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-node_pk) |>
      dplyr::collect(),
    authors_backup_table = dplyr::tbl(con, 'resource_authors') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-resource_authors_pk) |>
      dplyr::collect(),
    requirements_backup_table = dplyr::tbl(con, 'requirements') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-requirement_pk) |>
      dplyr::collect(),
    links_backup_table = dplyr::tbl(con, 'links') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-link_pk) |>
      dplyr::collect(),
    commit_backup_table = dplyr::tbl(con, 'resources_last_commit') |>
      dplyr::filter(id == resource_id) |>
      dplyr::select(-hash_pk) |>
      dplyr::collect()
  )

  # prepare the valid update list
  valid_update_list <- c(
    update_info$valid_update_list[1],
    update_info$valid_update_list[2:6],
    update_info$valid_update_list[2:6]
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
  # resource, but for the child tables (tags, requirements, nodes, authors...) must be a two-step process,
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

  # Delete child tables queries. We do it table by table, as that way we can use valid_update_list to run
  # only the necessary queries (i.e. if only updating authors, the only record deleted is in the
  # authors table)
  delete_child_tables_queries <- list(
    delete_old_tags = glue::glue_sql(
      "DELETE FROM resource_tags WHERE id = {resource_id};",
      .con = con
    ),
    delete_old_nodes = glue::glue_sql(
      "DELETE FROM nodes WHERE id = {resource_id};",
      .con = con
    ),
    delete_old_authors = glue::glue_sql(
      "DELETE FROM resource_authors WHERE id = {resource_id};",
      .con = con
    ),
    delete_old_requirements = glue::glue_sql(
      "DELETE FROM requirements WHERE id = {resource_id};",
      .con = con
    ),
    delete_old_links = glue::glue_sql(
      "DELETE FROM links WHERE id = {resource_id};",
      .con = con
    )
  )

  insert_child_tables_queries <- list(
    insert_new_tags = glue::glue_sql(
      "INSERT INTO resource_tags (tag_id, id) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$resource_tags_update_table$tag_id}, {update_tables_list$resource_tags_update_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_new_nodes = glue::glue_sql(
      "INSERT INTO nodes (node, id) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$nodes_update_table$node}, {update_tables_list$nodes_update_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_new_authors = glue::glue_sql(
      "INSERT INTO resource_authors (author_id, id) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$resource_authors_update_table$author_id}, {update_tables_list$resource_authors_update_table$id})",
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
    ),
    insert_new_links = glue::glue_sql(
      "INSERT INTO links (id, url_pdf, url_doi, url_source, url_docs) VALUES {values*};",
      values = glue::glue_sql(
        "({update_tables_list$links_update_table$id}, {update_tables_list$links_update_table$url_pdf}, {update_tables_list$links_update_table$url_doi}, {update_tables_list$links_update_table$url_source}, {update_tables_list$links_update_table$url_docs})",
        .con = con
      ),
      .con = con
    )
  )

  # run the queries, but only for the tables that must be updated
  c(resources_upsert_query, delete_child_tables_queries, insert_child_tables_queries)[valid_update_list] |>
    purrr::walk(
      ~ DBI::dbExecute(con, .x)
    )

  ## TODO: compare_metadata_tables. If something went wrong, restore the backup records
  # Now we check update went well and if not, restore the backup tables
  cli::cli_alert_info("- checking if the update went well\n")
  # usethis::ui_info("- checking if the update went well\n")
  final_check <- compare_metadata_tables(update_tables_list, con, resource_id)$valid_update_list

  if (any(final_check)) {
    # delete_resource_from_db(resource_id, con)
    restore_resource_from_backup(backup_tables_list, resource_id, con)

    cli::cli_abort(
      "Something happened when updating the database. Restoring {resource_id} to the previous state."
    )
    # usethis::ui_stop(
    #   "Something happened when updating the database. Restoring {resource_id} to the previous state."
    # )
  }

  return(invisible(TRUE))
}

#' Restore from backup
#'
#' @param backup_list list with the tables for backup
#' @param resource_id character with the resource id to restore
#' @param con db connection
#'
#' @return invisible TRUE if success, error if not
#'
#' @noRd
restore_resource_from_backup <- function(backup_list, resource_id, con) {
  list(
    delete_resource_query = glue::glue_sql(
      "DELETE FROM resources WHERE id = {resource_id};",
      .con = con
    ),
    resources_upsert_query <- glue::glue_sql(
      "INSERT INTO resources ({`columns`*}) VALUES ({values*})
          ON CONFLICT (id) DO UPDATE SET {insert_resources_subquery*};",
      columns = names(backup_list$resources_backup_table),
      values = as.list(dplyr::slice(backup_list$resources_backup_table, 1)),
      insert_resources_subquery = glue::glue_sql(
        "{`names(backup_list$resources_backup_table)`} = {as.list(dplyr::slice(backup_list$resources_backup_table, 1))}",
        .con = con
      ),
      .con = con
    ),
    insert_backup_tags = glue::glue_sql(
      "INSERT INTO resource_tags (tag_id, id) VALUES {values*};",
      values = glue::glue_sql(
        "({backup_list$tags_backup_table$tag_id}, {backup_list$tags_backup_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_backup_nodes = glue::glue_sql(
      "INSERT INTO nodes (node, id) VALUES {values*};",
      values = glue::glue_sql(
        "({backup_list$nodes_backup_table$node}, {backup_list$nodes_backup_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_backup_authors = glue::glue_sql(
      "INSERT INTO resource_authors (author_id, id) VALUES {values*};",
      values = glue::glue_sql(
        "({backup_list$resource_authors_backup_table$author_id}, {backup_list$resource_authors_backup_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_backup_requirements = glue::glue_sql(
      "INSERT INTO requirements (requirement, id) VALUES {values*};",
      values = glue::glue_sql(
        "({backup_list$requirements_backup_table$requirement}, {backup_list$requirements_backup_table$id})",
        .con = con
      ),
      .con = con
    ),
    insert_backup_links = glue::glue_sql(
      "INSERT INTO links (id, url_pdf, url_doi, url_source, url_docs) VALUES {values*};",
      values = glue::glue_sql(
        "({backup_list$links_backup_table$id}, {backup_list$links_backup_table$url_pdf}, {backup_list$links_backup_table$url_doi}, {backup_list$links_backup_table$url_source}, {backup_list$links_backup_table$url_docs})",
        .con = con
      ),
      .con = con
    ),
    insert_backup_commit = glue::glue_sql(
      "INSERT INTO resources_last_commit (id, last_commit_hash) VALUES {values*};",
      values = glue::glue_sql(
        "({backup_list$links_backup_table$id}, {backup_list$commit_backup_table$last_commit_hash})",
        .con = con
      ),
      .con = con
    )
  ) |>
    purrr::walk(
      ~ DBI::dbExecute(con, .x)
    )
}

#' Delete resource from db by id
#'
#' Delete resource form db by id
#'
#' This function will delete a resource by id. If not connection is provided, it automatically
#' connect to db and defer the closing of the connection
#'
#' @param resource_id character with the resource id to delete
#' @param con pool object connected to the database
#'
#' @return invisible TRUE if success. Invisible FALSE if deletion fails (no id found).
#' @export
delete_resource_from_db <- function(resource_id, con = NULL) {

  # create the connection if con = null
  # (here because this way there is no connection if dry is TRUE)
  if (is.null(con)) {
    # connect to database
    con <- metadata_db_con()
  }

  # query
  delete_query <- glue::glue_sql(
    "DELETE FROM resources WHERE id = {resource_id};",
    .con = con
  )
  # exec query
  db_response <- DBI::dbExecute(con, delete_query)
  # check response
  if (db_response != 1L) {
    cli::cli_alert_warning("{resource_id} can't be deleted")
    # usethis::ui_warn("{resource_id} can't be deleted")
    return(invisible(FALSE))
  }

  cli::cli_alert_success("{resource_id} deleted succesfully.")
  # usethis::ui_done("{resource_id} deleted succesfully.")
  return(invisible(TRUE))

}

#' Translate R types to SQL types
#'
#' Translate R types to SQL types
#'
#' @param types character vector with types
#'
#' @return character vector of same length as \code{types} with the SQL types
#'
#' @noRd
translate_r2sql_types <- function(types) {
  dplyr::case_when(
    types == 'character' ~ 'TEXT',
    types == 'integer' ~ 'INT',
    types %in% c('numeric', 'double') ~ 'FLOAT',
    types == 'logical' ~ 'BOOL',
    TRUE ~ 'TEXT'
  )
}

#' Connect to the EMF metadata database
#'
#' Pool connection to the EMF metadata database
#'
#' This function returns a pool object with a connection to the postgreSQL
#' metadata database. It also defers (\code{\link[withr]{defer}}) the pool
#' closing to the parent environment by default.
#' The connection params default to environment variables, set these with
#' \code{Sys.setenv} or \code{withr::with_envvar}, or with an \code{.Renviron}
#' file.
#'
#' @param host db host address/name to resolve
#' @param user db user
#' @param password db user password
#' @param dbname db name to connect
#' @param .envir environment in which the defer call takes place
#'
#' @return A \code{pool} object with the database connection
#'
#' @export
metadata_db_con <- function(
  host = Sys.getenv('EMF_DATABASE_HOST'),
  user = Sys.getenv('EMF_DATABASE_USER'),
  password = Sys.getenv('EMF_DATABASE_PASS'),
  dbname = Sys.getenv('EMF_DATABASE'),
  .envir = parent.frame()
) {

  db_conn <- pool::dbPool(
    drv = RPostgres::Postgres(),
    host = host,
    user = user,
    password = password,
    dbname = dbname
  )
  # defer the closing of the pool
  withr::defer(pool::poolClose(db_conn), envir = .envir)
  # return the db object
  return(db_conn)
}

#' Access the public_* tables.
#'
#' Access to the public resources tables
#'
#' @param resource_type Character indicating the type of resource to query
#' @param ... arguments for \code{dplyr::filter} to filter the results
#' @param .con Connection to the database (pool). NULL by default, connecting with the environment variables.
#'
#' @return A tibble with the public resources table queried
#'
#' @examples
#' use_public_table('workflows')
#'
#' @export
use_public_table <- function(
  resource_type = c('workflows', 'pipelines', 'tech_docs', 'models', 'data', 'softworks', 'all'),
  ...,
  .con = NULL
) {

  # match argument
  resource_type <- match.arg(resource_type)

  # connect to database if needed
  if (is.null(.con)) {
    cli::cli_alert_info(
      "Connection to the database not provided. Attempting to connect using environment variables."
    )
    # usethis::ui_info("Connection to the database not provided. Attempting to connect using environment variables.")
    .con <- metadata_db_con()
  }

  # all is special, we need the the resources table, instead of the public_* tables.
  table_name <- 'resources'
  if (resource_type != 'all') {
    table_name <- glue::glue("public_{resource_type}")
    if (resource_type == "pipelines") {
      table_name <- glue::glue("internal_{resource_type}")
      res <- dplyr::tbl(.con, table_name) |>
        dplyr::filter(...) |>
        dplyr::collect()
      return(res)
    }
  }

  res <- dplyr::tbl(.con, table_name) |>
    dplyr::filter(..., emf_public == TRUE) |>
    dplyr::collect()
  return(res)
}

#' Retrieve the public workflows table.
#'
#' Retrieve the public workflows table.
#'
#' @param ... arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
#'   specified and error is raised.
#'
#' @return A tibble with the public resources table queried
#'
#' @examples
#' use_public_table('workflows')
#'
#' @export
public_workflows <- function(...) {
  use_public_table(resource_type = 'workflows', ...)
}

#' Retrieve the public pipelines table.
#'
#' Retrieve the public pipelines table.
#'
#' @param ... arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
#'   specified and error is raised.
#'
#' @return A tibble with the public resources table queried
#'
#' @examples
#' use_public_table('pipelines')
#'
#' @export
internal_pipelines <- function(...) {
  use_public_table(resource_type = 'pipelines', ...)
}

#' Retrieve the public softworks table.
#'
#' Retrieve the public softworks table.
#'
#' @param ... arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
#'   specified and error is raised.
#'
#' @return A tibble with the public resources table queried
#'
#' @examples
#' use_public_table('softworks')
#'
#' @export
public_softworks <- function(...) {
  use_public_table(resource_type = 'softworks', ...)
}

#' Retrieve the public data table.
#'
#' Retrieve the public data table.
#'
#' @param ... arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
#'   specified and error is raised.
#'
#' @return A tibble with the public resources table queried
#'
#' @examples
#' use_public_table('data')
#'
#' @export
public_data <- function(...) {
  use_public_table(resource_type = 'data', ...)
}

#' Retrieve the public tech_docs table.
#'
#' Retrieve the public tech_docs table.
#'
#' @param ... arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
#'   specified and error is raised.
#'
#' @return A tibble with the public resources table queried
#'
#' @examples
#' use_public_table('tech_docs')
#'
#' @export
public_tech_docs <- function(...) {
  use_public_table(resource_type = 'tech_docs', ...)
}

#' Retrieve the public models table.
#'
#' Retrieve the public models table.
#'
#' @param ... arguments for \code{\link{use_public_tables}} except for \code{resource_type}. If the former is
#'   specified and error is raised.
#'
#' @return A tibble with the public resources table queried
#'
#' @examples
#' use_public_table('models')
#'
#' @export
public_models <- function(...) {
  use_public_table(resource_type = 'models', ...)
}

#' Transform the excel file for external data
#'
#' Transform the excel file for external data
#'
#' Make the necessary transformations from the excel file in the repository to
#' a tibble suitable to update the metadata database
#'
#' @param external_data_file Character with the file name in the external data
#'   repository
#'
#' @return A tibble suitable for metadata collection
#'
#' @noRd
external_data_transform <- function(external_data_file = 'ExternalDataSources.xlsx') {

  original_table <-
    readxl::read_xlsx(path = external_data_file, sheet = 1, .name_repair = 'universal')

  original_table |>
    # remove those not external, or that dont have URL or DOI
    dplyr::filter(
      !(is.na(URLsource)) | !(is.na(DOI)), is.na(Source) | Source != "CREAF"
    ) |>
    # create all necessary variables/metadata
    dplyr::mutate(
      # descr & title
      description = Description,
      title = DataSourceName,
      # the necessary emf metadata
      emf_type = 'data',
      emf_public = TRUE,
      emf_automatized = TRUE,
      emf_reproducible = FALSE,
      emf_draft = FALSE,
      emf_data_type = 'external_data',
      # get the url and create the metadata var.
      # for that, we choose between url and doi, with preference for the URL
      data_repository = dplyr::if_else(!is.na(URLsource), URLsource, DOI),
      # tags, built from model type, level and code language
      tags = purrr::pmap(
        list(ThematicCategory, ThematicSubcategory),
        .f = function(x,y) {return(c(x,y))}
      ),
      nodes = list(""),
      authors = list(""),
      requirements = list(""),
      links = purrr::pmap(
        list(DOI, URLsource),
        .f = function(x,y) {return(list(url_doi = x, url_source = y))}
      )
    ) |>
    dplyr::select(
      id, description, title, emf_type, emf_public, emf_automatized,
      emf_reproducible, emf_draft, emf_data_type, data_repository, tags,
      nodes, authors, requirements, links
    )

}


#' Transform the excel file for external models
#'
#' Transform the excel file for external models
#'
#' Make the necessary transformations from the excel file in the repository to
#' a tibble suitable to update the metadata database
#'
#' @param external_models_file Character with the file name in the external
#'   models repository
#'
#' @return A tibble suitable for metadata collection
#'
#' @noRd
external_models_transform <- function(external_models_file = 'ProcessBasedModelsDatabase.xlsx') {

  original_table <-
    readxl::read_xlsx(path = external_models_file,sheet = 1, skip = 1, .name_repair = 'universal')

  original_table |>
    # ensure external catalog entry is logical
    dplyr::mutate(External.catalog.entry = as.logical(External.catalog.entry)) |>
    # remove those without DOI or URL, as then we have nothing to offer
    dplyr::filter(!(is.na(URL)) | !(is.na(DOI)), External.catalog.entry) |>
    # create all necessary variables/metadata
    dplyr::mutate(
      # id & title
      id = Model.name.acronym,
      description = Short.description,
      title = dplyr::if_else(
        !is.na(Full.name), glue::glue("{Full.name} ({Model.name.acronym})"), Model.name.acronym
      ),
      # the necessary emf metadata
      emf_type = 'model',
      emf_public = TRUE,
      emf_automatized = TRUE,
      emf_reproducible = FALSE,
      emf_draft = FALSE,
      emf_data_type = 'external_data',
      # get the url and create the metadata var.
      # for that, we choose between url and doi, with preference for the URL
      model_repository = dplyr::if_else(!is.na(URL), URL, DOI),
      # tags, built from model type, level and code language
      tags = purrr::pmap(
        list(Model.type, Level, Code.language.platform),
        .f = function(x,y,z) {return(c(x,y,z))}
      ),
      nodes = list(""),
      authors = list(""),
      requirements = list(""),
      links = purrr::pmap(
        list(DOI, URL),
        .f = function(x,y) {return(list(url_doi = x, url_source = y))}
      )
    ) |>
    dplyr::select(
      id, description, title, emf_type, emf_public, emf_automatized,
      emf_reproducible, emf_draft, emf_data_type, model_repository, tags,
      nodes, authors, requirements, links
    )
}
