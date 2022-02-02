# Code to create the dummy testing metadata database
# This is intended to be a test, to prepare the real database
library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgres)
library(glue)


## Connect to the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('EMF_DATABASE_HOST'),
  user = Sys.getenv('EMF_DATABASE_USER'),
  password = Sys.getenv('EMF_DATABASE_PASS')
)

##############################################################################################################
## Delete the database (just in case)
delete_db_query <- glue::glue_sql(
  "DROP DATABASE emf_metadata_dummy;",
  .con = conn
)
DBI::dbExecute(conn, delete_db_query)
##############################################################################################################

## Create the database
create_db_query <- glue::glue_sql(
  "CREATE DATABASE emf_metadata_dummy;",
  .con = conn
)
DBI::dbExecute(conn, create_db_query)

## Disconnect and connect to the new database
DBI::dbDisconnect(conn)

emf_database <- EMFtoolbox:::metadata_db_con()
withr::defer(pool::poolClose(emf_database))

## Create the empty tables
create_queries_list <- list(
  create_resources_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS resources(
      id TEXT PRIMARY KEY,
      emf_type TEXT,
      emf_data_type TEXT,
      emf_public BOOL,
      emf_automatized BOOL,
      emf_reproducible BOOL,
      emf_draft BOOL,
      thematic TEXT,
      resource_link TEXT,
      date DATE,
      date_lastmod DATE,
      description TEXT,
      title TEXT,
      external_link TEXT,
      model_repository TEXT,
      data_repository TEXT
    );",
    .con = emf_database
  ),

  create_nodes_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS nodes(
      node_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      node TEXT,
      id TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  ),

  create_resource_authors_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS resource_authors(
      resource_authors_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      author_id TEXT,
      id TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  ),

  create_authors_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS authors(
      authors_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      author_id TEXT,
      name TEXT,
      surname TEXT,
      aff TEXT,
      aff_link TEXT
    );",
    .con = emf_database
  ),

  create_requirements_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS requirements(
      requirement_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      requirement TEXT,
      id TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  ),

  create_resource_tags_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS resource_tags(
      resource_tags_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      tag_id TEXT,
      id TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  ),

  create_tags_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS tags(
      tags_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      tag_id TEXT,
      tag_description TEXT
    );",
    .con = emf_database
  ),

  create_links_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS links(
      link_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      id TEXT,
      url_pdf TEXT,
      url_doi TEXT,
      url_source TEXT,
      url_docs TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  ),

  create_metadata_definitions_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS metadata_definitions(
      field_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      field TEXT,
      scopes TEXT,
      categories TEXT,
      definition TEXT
    );",
    .con = emf_database
  ),

  create_resources_last_commit_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS resources_last_commit(
      hash_pk INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      last_commit_hash TEXT,
      id TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  )
)

## Create the indexes
indexes_queries_list <- list(
  resources_core_index = glue::glue_sql(
    "CREATE INDEX IF NOT EXISTS resources_core_index ON resources(emf_type, emf_public, emf_automatized, emf_reproducible, emf_draft);"
  ),
  resources_type_index = glue::glue_sql(
    "CREATE INDEX IF NOT EXISTS resources_type_index ON resources(emf_type);"
  ),
  resources_public_index = glue::glue_sql(
    "CREATE INDEX IF NOT EXISTS resources_public_index ON resources(emf_public);"
  ),
  resources_automatized_index = glue::glue_sql(
    "CREATE INDEX IF NOT EXISTS resources_automatized_index ON resources(emf_automatized);"
  ),
  resources_reproducible_index = glue::glue_sql(
    "CREATE INDEX IF NOT EXISTS resources_reproducible_index ON resources(emf_reproducible);"
  ),
  nodes_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS node_index ON nodes(id);"
  ),
  authors_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS author_index ON authors(author_id);"
  ),
  resource_authors_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS resource_authors_index ON resource_authors(author_id, id);"
  ),
  requirements_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS requirement_index ON requirements(id);"
  ),
  tags_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS tag_index ON tags(tag_id);"
  ),
  resource_tags_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS resource_tags_index ON resource_tags(tag_id, id);"
  ),
  links_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS link_index ON links(id);"
  ),
  last_commit_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS last_commit_index ON resources_last_commit(id);"
  )
)

## Create the Views
create_views_list <- list(
  public_workflows = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_workflows AS
      SELECT resources.id AS workflow,
        resources.title,
        resources.thematic,
        resources.date,
        resources.date_lastmod,
        resources.resource_link AS link,
        resources.emf_draft,
        resources.description,
        resources.emf_type,
        resources.emf_public,
        resources.resource_link,
        resources.external_link,
        auth.author,
        reqs.requirement,
        tags.tag,
        edges.node
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author_id) AS author
            FROM resource_authors
            GROUP BY resource_authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag_id) AS tag
            FROM resource_tags
            GROUP BY resource_tags.id
            ) tags USING (id)
        LEFT JOIN (
            SELECT id, array_agg(node) AS node
            FROM nodes
            GROUP BY nodes.id
            ) edges USING (id)
      WHERE resources.emf_type = 'workflow';
    "
  ),
  public_tech_docs = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_tech_docs AS
      SELECT resources.id AS tech_doc,
        resources.title,
        resources.thematic,
        resources.date,
        resources.date_lastmod,
        resources.resource_link AS link,
        resources.emf_draft,
        resources.description,
        resources.emf_type,
        resources.emf_public,
        resources.resource_link,
        resources.external_link,
        auth.author,
        reqs.requirement,
        tags.tag,
        edges.node
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author_id) AS author
            FROM resource_authors
            GROUP BY resource_authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag_id) AS tag
            FROM resource_tags
            GROUP BY resource_tags.id
            ) tags USING (id)
        LEFT JOIN (
            SELECT id, array_agg(node) AS node
            FROM nodes
            GROUP BY nodes.id
            ) edges USING (id)
      WHERE resources.emf_type = 'tech_doc';
    "
  ),
  public_models = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_models AS
      SELECT resources.id AS model,
        resources.title,
        resources.thematic,
        resources.date,
        resources.date_lastmod,
        resources.resource_link AS link,
        resources.emf_draft,
        resources.description,
        resources.emf_type,
        resources.emf_data_type,
        resources.emf_public,
        resources.resource_link,
        resources.external_link,
        resources.model_repository,
        auth.author,
        reqs.requirement,
        tags.tag,
        edges.node
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author_id) AS author
            FROM resource_authors
            GROUP BY resource_authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag_id) AS tag
            FROM resource_tags
            GROUP BY resource_tags.id
            ) tags USING (id)
        LEFT JOIN (
            SELECT id, array_agg(node) AS node
            FROM nodes
            GROUP BY nodes.id
            ) edges USING (id)
      WHERE resources.emf_type = 'model';
    "
  ),
  public_data = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_data AS
      SELECT resources.id AS data,
        resources.title,
        resources.thematic,
        resources.date,
        resources.date_lastmod,
        resources.resource_link AS link,
        resources.emf_draft,
        resources.description,
        resources.emf_type,
        resources.emf_data_type,
        resources.emf_public,
        resources.resource_link,
        resources.external_link,
        resources.data_repository,
        auth.author,
        reqs.requirement,
        tags.tag,
        edges.node
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author_id) AS author
            FROM resource_authors
            GROUP BY resource_authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag_id) AS tag
            FROM resource_tags
            GROUP BY resource_tags.id
            ) tags USING (id)
        LEFT JOIN (
            SELECT id, array_agg(node) AS node
            FROM nodes
            GROUP BY nodes.id
            ) edges USING (id)
      WHERE resources.emf_type = 'data';
    "
  ),
  public_softworks = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_softworks AS
      SELECT resources.id AS softwork,
        resources.title,
        resources.thematic,
        resources.date,
        resources.date_lastmod,
        resources.resource_link AS link,
        resources.emf_draft,
        resources.description,
        resources.emf_type,
        resources.emf_public,
        resources.resource_link,
        resources.external_link,
        auth.author,
        reqs.requirement,
        tags.tag,
        edges.node
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author_id) AS author
            FROM resource_authors
            GROUP BY resource_authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag_id) AS tag
            FROM resource_tags
            GROUP BY resource_tags.id
            ) tags USING (id)
        LEFT JOIN (
            SELECT id, array_agg(node) AS node
            FROM nodes
            GROUP BY nodes.id
            ) edges USING (id)
      WHERE resources.emf_type = 'softwork';
    "
  )
)

## Execute the queries
create_queries_list %>%
  purrr::walk(
    ~ DBI::dbExecute(emf_database, .x)
  )

indexes_queries_list %>%
  purrr::walk(
    ~ DBI::dbExecute(emf_database, .x)
  )

create_views_list %>%
  purrr::walk(
    ~ DBI::dbExecute(emf_database, .x)
  )

## Updating definitions table
metadata_definitions <- readxl::read_excel('data-raw/metadata_table.xlsx') %>%
  dplyr::select(-`Allowed values`, -`Example value`) %>%
  dplyr::rename(
    field = Field,
    scopes = Scopes,
    categories = Categories,
    definition = Definition
  )

DBI::dbAppendTable(
  emf_database,
  'metadata_definitions', metadata_definitions
)

withr::deferred_run()
