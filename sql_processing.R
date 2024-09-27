library(DBI)
library(RSQLite)
library(dplyr)

# Function to create a connection to the SQLite database
create_db_connection <- function(db_path) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  return(con)
}

# Function to create tables in the database
create_tables <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS raw_data (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      read_id TEXT,
      sequence TEXT,
      quality TEXT
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS processed_data (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      read_id TEXT,
      sequence TEXT,
      gc_content REAL
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS featured_data (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      read_id TEXT,
      sequence TEXT,
      gc_content REAL,
      complexity REAL,
      kmer_freq TEXT,
      dna_shape TEXT
    )
  ")
}

# Function to insert raw data into the database
insert_raw_data <- function(con, raw_data) {
  dbWriteTable(con, "raw_data", raw_data, append = TRUE)
}

# Function to insert processed data into the database
insert_processed_data <- function(con, processed_data) {
  dbWriteTable(con, "processed_data", processed_data, append = TRUE)
}

# Function to insert featured data into the database
insert_featured_data <- function(con, featured_data) {
  # Convert complex columns to JSON strings
  featured_data$kmer_freq <- sapply(featured_data$kmer_freq, jsonlite::toJSON)
  featured_data$dna_shape <- sapply(featured_data$dna_shape, jsonlite::toJSON)
  
  dbWriteTable(con, "featured_data", featured_data, append = TRUE)
}

# Function to query data from the database
query_data <- function(con, table_name, limit = 100) {
  query <- sprintf("SELECT * FROM %s LIMIT %d", table_name, limit)
  result <- dbGetQuery(con, query)
  return(result)
}

# Function to perform a join operation
join_processed_and_featured <- function(con) {
  query <- "
    SELECT p.read_id, p.sequence, p.gc_content, f.complexity, f.kmer_freq, f.dna_shape
    FROM processed_data p
    JOIN featured_data f ON p.read_id = f.read_id
    LIMIT 100
  "
  result <- dbGetQuery(con, query)
  return(result)
}

# Function to close the database connection
close_db_connection <- function(con) {
  dbDisconnect(con)
}

# Example usage
if (interactive()) {
  db_path <- "ngs_data.sqlite"
  con <- create_db_connection(db_path)
  create_tables(con)
  
  # Assuming you have raw_data, processed_data, and featured_data dataframes
  insert_raw_data(con, raw_data)
  insert_processed_data(con, processed_data)
  insert_featured_data(con, featured_data)
  
  # Query data
  raw_sample <- query_data(con, "raw_data")
  processed_sample <- query_data(con, "processed_data")
  featured_sample <- query_data(con, "featured_data")
  
  # Perform a join operation
  joined_data <- join_processed_and_featured(con)
  
  print(head(joined_data))
  
  close_db_connection(con)
}