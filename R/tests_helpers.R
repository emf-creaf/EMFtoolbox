local_temp_proj <- function(dir = tempdir(), env = parent.frame()) {

  # store the current project
  old_project <- usethis::proj_get()

  # create new folder and project, remove it when finished
  usethis::create_project(dir, open = FALSE)
  withr::defer(fs::dir_delete(dir), envir = env)

  # go to the folder and do whatever it needs, but always back again to the original one when finish (defer)
  setwd(dir)
  withr::defer(setwd(old_project), envir = env)

  # switch to new project
  usethis::proj_set(dir)
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env)

  return(dir)
}
