external_models_transform <- function(external_models_file = 'ProcessBasedModelsDatabase.xlsx') {

  original_table <- readxl::read_xlsx(
    path = external_models_file,
    sheet = 1, skip = 1, .name_repair = 'universal'
  )

  original_table %>%
    # remove those without DOI or URL, as then we have nothing to offer
    dplyr::filter(
      !(is.na(URL)) | !(is.na(DOI))
    ) %>%
    # create all necessary variables/metadata
    dplyr::mutate(
      # id & title
      id = Model.name.acronym,
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
      )
    ) %>%
    dplyr::select(
      id, title, emf_type, emf_public, emf_automatized,
      emf_reproducible, emf_draft, emf_data_type, model_repository, tags
    )
}

collect_metadata_external_models <- function(con = NULL, ..., .dry = TRUE) {
  # load the original table and transform it
  external_models_metadata <- external_models_transform(...)

  # TODO find a way to iterate **safely** by rows calling collect_metadata. slider::slide does not accept
  # safe versions of the function, soo

  # # create a safe version of collect_metadata
  # safe_collect_metadata <- purrr::safely(collect_metadata)
  #
  # # now we have a dataframe with all external models metadata (simplified). We need to loop by rows (with
  # # slider::slide) and call collect metadata for each one to see if there is need of any update.
  # slider::slide(external_models_metadata, .f = safe_collect_metadata, .con = con, .dry = .dry)

}
