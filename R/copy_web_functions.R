check_hugo_build <- function(content, public) {
  # empty vectors for errors found, to fill later
  offending_directories <- character()
  offending_indexes <- character()

  # check if all directories in content are present in public
  directories_ok <- all(
    tolower(
      fs::path_file(fs::dir_ls(fs::path(content), recurse = TRUE, type = 'directory'))
    ) %in%
      fs::path_file(fs::dir_ls(fs::path(public), recurse = TRUE, type = 'directory'))
  )

  # if not all folders are ok, get the list of offending folders
  if (!directories_ok) {
    offending_directories <-
      fs::dir_ls(fs::path(content), recurse = TRUE, type = 'directory')[which(
        !tolower(
          fs::path_file(fs::dir_ls(fs::path(content), recurse = TRUE, type = 'directory'))
        ) %in%
          fs::path_file(fs::dir_ls(fs::path(public), recurse = TRUE, type = 'directory'))
      )]
  }

  # check index.html present in all public folders
  index_files_ok <- all(
    fs::file_exists(
      fs::path(
        fs::dir_ls(
          fs::path(public), recurse = TRUE, type = 'directory',
          regexp = "page$|categories$|css|js|index_files|fonts|img",
          invert = TRUE
        ),
        'index.html'
      )
    )
  )

  # if not all indexes are ok, get the list of offending index.html's
  if (!index_files_ok) {
    offending_indexes <- names(
      which(
        !fs::file_exists(
          fs::path(
            fs::dir_ls(
              fs::path(public), recurse = TRUE, type = 'directory',
              regexp = "page$|categories$|css|js|index_files|fonts|img",
              invert = TRUE
            ),
            'index.html'
          )
        )
      )
    )
  }

  # warn about the offending folders/indexes and send the email
  if (any(length(offending_directories) > 0, length(offending_indexes) > 0)) {
    # pretty print the offending directories and the offending indexes
    cli::cli_alert_warning(c(
      "!" = "Some pages haven't been built correctly",
      "i" = "Indexes missing:",
      "{offending_indexes}",
      "i" = "Directories missing:",
      "{offending_directories}"
    ))
    # usethis::ui_warn("Some pages haven't been built correctly:")
    # usethis::ui_line("Indexes missing:")
    # usethis::ui_todo("{offending_indexes}")
    # usethis::ui_line("Directories missing:")
    # usethis::ui_todo("{offending_directories}")

    # create a list compatible with email_content argument from send_error_email
    email_content <- list(
      explanation_text = "Some pages haven't been built correctly",
      offending_indexes = offending_indexes,
      offending_directories = offending_directories
    )

    # send the email
    send_error_email(email_content, "EMF web: Hugo build failed")

    # return FALSE
    return(invisible(FALSE))
  }

  # if everything is ok, return TRUE
  return(invisible(TRUE))
}

send_error_email <- function(email_content = NULL, subject_field = "") {
  # if there is no content, don't do anything
  if (is.null(email_content)) {
    return(invisible(FALSE))
  }

  # to/from fields
  to_field <- "victorgrandagarcia@gmail.com"
  cc_field <- "v.granda@creaf.uab.cat"
  from_field <- "victorgrandagarcia@gmail.com"

  # transform the content list in character for the email body
  body_field <- glue::glue(
    "{email_content[['explanation_text']]}!!\n",
    "{glue::glue_collapse(glue::glue('{names(email_content[-1])}:\n{email_content[-1]}'), sep = '\n')}\n"
  )

  # build the email
  alarm_email <- gmailr::gm_mime(
    To = to_field,
    Cc = cc_field,
    From = from_field,
    Subject = subject_field,
    body = body_field
  )

  # auth the sending
  gmailr::gm_auth_configure(
    path = Sys.getenv("GMAILR_TOKEN")
  )
  gmailr::gm_auth(email = "victorgrandagarcia@gmail.com")

  # send the email
  gmailr::gm_send_message(alarm_email)

  # if everything is ok, return TRUE
  return(invisible(TRUE))
}

copy_web <- function(origin, dest) {
  # we need to copy the public folder from hugo build to the web server folder,
  # but first we need to remove the old one.
  # Also, all of this with a backup in a temp folder in case anything goes
  # wrong being able to restore the previous one.
  # Finally, if something goes wrong, send an email.

  # create a backup from the destination
  backup_old_web <- create_folder_backup(dest)

  # remove dest folder contents
  fs::file_delete(fs::dir_ls(dest))

  # copy new web data
  fs::dir_copy(origin, dest, TRUE)
  # system(paste0("cp -a ", origin, "/. ", dest))

  # now check that origin and dest now are the same
  origin_folder_info <- fs::dir_info(fs::path_rel(origin, origin), recurse = TRUE) |>
    dplyr::select(-access_time)
  dest_folder_info <- fs::dir_info(fs::path_rel(dest, dest), recurse = TRUE) |>
    dplyr::select(-access_time)

  identical_check <- identical(origin_folder_info, dest_folder_info)

  # now we'll check if conectivity in the new web is correct
  connectivity_check <- check_web_conectivity()

  # if is not the same or there is connectivity issues, restore backup
  if (any(!connectivity_check, !identical_check)) {

    # prepare the explanation text for the email
    explanation_text <- ''
    if (!identical_check) {
      explanation_text <-
        glue::glue("{explanation_text} - Something went wrong copying the new web to the destination folder")
    }
    if (!connectivity_check) {
      explanation_text <-
        glue::glue("{explanation_text} - Updated web fails connectivity test")
    }

    # if no conectivity, restore backup
    fs::dir_delete(dest)
    fs::dir_copy(backup_old_web, dest, TRUE)
    send_error_email(
      list(
        explanation_text = explanation_text,
        destination = dest,
        origin = origin
      ),
      subject_field = "EMF web: Update went wrong"
    )
    return(invisible(FALSE))
  }

  # if everything is ok, return TRUE
  return(invisible(TRUE))

}

check_web_conectivity <- function() {
  # base url and its status
  base_url <- "http://emf.creaf.cat"
  status_base_url <- httr::status_code(httr::GET(base_url))

  if (status_base_url != 200) {
    cli::cli_alert_danger("No conectivity to {base_url}")
    # usethis::ui_oops("No conectivity to {base_url}")
    return(invisible(FALSE))
  }

  # TODO check different pages and their status

  # if everything is ok, return TRUE
  return(invisible(TRUE))
}


#' Update EMF web
#'
#' This function retrieves the latest github version of the web and update the
#' server
#'
#' @param dest path to the web server folder
#'
#' @return invisible TRUE if all goes well, error if not
#'
#' @details In each step of the process, if anything fails an email is send
#'
#' @export
copy_emf_web <- function(dest) {

  # clone the web repo if needed
  if (!stringr::str_detect(usethis::proj_sitrep()[['working_directory']], 'emf_web')) {
    clone_from_github(repo = 'emf_web', org = 'emf-creaf')
  }

  # build the site
  # usethis::ui_info("Building site with Hugo")
  # withr::with_options(
  #   list(blogdown.hugo.version = "0.129.0"), blogdown::build_site()
  # )
  cli::cli_alert_info("Building site with Hugo")
  xfun::system3(
    # find hugo will find the latest version installed
    blogdown::find_hugo(),
    args = c(
      # normal args
      "-d", shQuote("public"),
      "--themesDir", shQuote("themes"),
      "--theme", shQuote("emf_hugo_theme"),
      # only needed to avoid weird OOM error in Hugo
      "--templateMetrics", "--templateMetricsHints"
    )
  )

  # check the build
  build_check <- check_hugo_build(content = 'content', public = 'public')
  # if the build fails, stop
  if (!build_check) {
    # cli::cli_alert_warning("EMF web built with errors")
    cli::cli_abort("EMF web built with errors")
    # usethis::ui_warn("EMF web built with errors")
  }

  # copy the web, and if there is an error, stop
  # (possible errors are bad copy, no conectivity after copying...)
  copy_check <- copy_web(origin = 'public', dest = dest)
  # if the build fails, stop
  if (!copy_check) {
    cli::cli_abort("EMF web copy failed")
    # usethis::ui_stop("EMF web copy failed")
  }

  # return TRUE if everything is ok
  cli::cli_alert_success("Web copied without errors")
  # usethis::ui_done("Web copied without errors")
  return(invisible(TRUE))
}
