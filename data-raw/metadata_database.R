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
  host = "65.21.120.62",
  user = "emf",
  password = rstudioapi::askForPassword("Database password")
)

## Create the database
create_db_query <- glue::glue_sql(
  "CREATE DATABASE emf_metadata_dummy;",
  .con = conn
)
DBI::dbExecute(conn, create_db_query)

## Delete the database (just in case)
# delete_db_query <- glue::glue_sql(
#   "DROP DATABASE emf_metadata_dummy;",
#   .con = conn
# )
# DBI::dbExecute(conn, delete_db_query)

## Disconnect and connect to the new database
DBI::dbDisconnect(conn)

emf_database <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "65.21.120.62",
  user = "emf",
  password = rstudioapi::askForPassword("Database password"),
  dbname = 'emf_metadata_dummy'
)

## Create the empty tables
create_queries_list <- list(
  create_resources_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS resources(
      id TEXT PRIMARY KEY,
      emf_type TEXT,
      emf_public BOOL,
      emf_automatized BOOL,
      emf_reproducible BOOL,
      emf_draft BOOL,
      thematic TEXT,
      resource_link TEXT,
      date DATE,
      date_lastmod DATE
    );",
    .con = emf_database
  ),

  create_edges_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS edges(
      edge_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      edge TEXT,
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
      author_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      author TEXT,
      id TEXT,
      author_aff TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  ),

  create_requirements_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS requirements(
      requirement_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      requirement TEXT,
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
      tag_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      tag TEXT,
      id TEXT,
      CONSTRAINT fk_resources
        FOREIGN KEY(id)
          REFERENCES resources(id)
          ON DELETE CASCADE
    );",
    .con = emf_database
  ),
  create_metadata_definitions_query = glue::glue_sql(
    "CREATE TABLE IF NOT EXISTS metadata_definitions(
      field_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
      field TEXT,
      scopes TEXT,
      categories TEXT,
      definition TEXT
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
  edges_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS edge_index ON edges(id);"
  ),
  authors_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS author_index ON authors(id);"
  ),
  requirements_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS requirement_index ON requirements(id);"
  ),
  tags_index = glue::glue(
    "CREATE INDEX IF NOT EXISTS tag_index ON tags(id);"
  )
)

## Create the Views
create_views_list <- list(
  public_workflows = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_workflows AS
      SELECT resources.id AS workflow,
        resources.thematic,
        resources.date_lastmod AS date,
        resources.resource_link AS link,
        auth.author,
        auth.author_aff,
        reqs.requirement,
        tgs.tag,
        edgs.edge
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author) AS author, array_agg(author_aff) AS author_aff
            FROM authors
            GROUP BY authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag) AS tag
            FROM tags
            GROUP BY tags.id
            ) tgs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(edge) AS edge
            FROM edges
            GROUP BY edges.id
            ) edgs USING (id)
      WHERE resources.emf_type = 'workflow';
    "
  ),
  public_tech_docs = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_tech_docs AS
      SELECT resources.id AS tech_doc,
        resources.thematic,
        resources.date_lastmod AS date,
        resources.resource_link AS link,
        auth.author,
        auth.author_aff,
        reqs.requirement,
        tgs.tag,
        edgs.edge
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author) AS author, array_agg(author_aff) AS author_aff
            FROM authors
            GROUP BY authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag) AS tag
            FROM tags
            GROUP BY tags.id
            ) tgs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(edge) AS edge
            FROM edges
            GROUP BY edges.id
            ) edgs USING (id)
      WHERE resources.emf_type = 'tech_doc';
    "
  ),
  public_models = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_models AS
      SELECT resources.id AS model,
        resources.thematic,
        resources.date_lastmod AS date,
        resources.resource_link AS link,
        auth.author,
        auth.author_aff,
        reqs.requirement,
        tgs.tag,
        edgs.edge
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author) AS author, array_agg(author_aff) AS author_aff
            FROM authors
            GROUP BY authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag) AS tag
            FROM tags
            GROUP BY tags.id
            ) tgs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(edge) AS edge
            FROM edges
            GROUP BY edges.id
            ) edgs USING (id)
      WHERE resources.emf_type = 'model';
    "
  ),
  public_data = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_data AS
      SELECT resources.id AS data,
        resources.thematic,
        resources.date_lastmod AS date,
        resources.resource_link AS link,
        auth.author,
        auth.author_aff,
        reqs.requirement,
        tgs.tag,
        edgs.edge
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author) AS author, array_agg(author_aff) AS author_aff
            FROM authors
            GROUP BY authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag) AS tag
            FROM tags
            GROUP BY tags.id
            ) tgs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(edge) AS edge
            FROM edges
            GROUP BY edges.id
            ) edgs USING (id)
      WHERE resources.emf_type = 'data';
    "
  ),
  public_softworks = glue::glue_sql(
    "CREATE OR REPLACE VIEW public_softworks AS
      SELECT resources.id AS softwork,
        resources.thematic,
        resources.date_lastmod AS date,
        resources.resource_link AS link,
        auth.author,
        auth.author_aff,
        reqs.requirement,
        tgs.tag,
        edgs.edge
      FROM resources
        LEFT JOIN (
            SELECT id, array_agg(author) AS author, array_agg(author_aff) AS author_aff
            FROM authors
            GROUP BY authors.id
            ) auth USING (id)
        LEFT JOIN (
            SELECT id, array_agg(requirement) AS requirement
            FROM requirements
            GROUP BY requirements.id
            ) reqs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(tag) AS tag
            FROM tags
            GROUP BY tags.id
            ) tgs USING (id)
        LEFT JOIN (
            SELECT id, array_agg(edge) AS edge
            FROM edges
            GROUP BY edges.id
            ) edgs USING (id)
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

# Close the connection to the database
DBI::dbDisconnect(emf_database)