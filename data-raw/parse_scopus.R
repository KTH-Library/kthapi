library(ndjson)
library(dplyr)
library(readr)
library(jsonlite)

scopus_flatten <- function(scopus_json, chunk_size = 1000, flatten_deeply = FALSE) {

  td <- dirname(scopus_json)
  i <- 0

  pj <- function(x, pos) {
    i <<- i + 1
    out <- sprintf("%s/scopus-%05i.csv", td, i)
    chunk <- ndjson::flatten(x)
    schema <- names(chunk)
    cols <- schema
    if (flatten_deeply == FALSE)
      cols <- schema[-which(grepl("\\.", schema))]
    fields <- paste(collapse = ",", cols)
    readr::write_lines(fields, out)
    readr::write_csv(chunk[, ..cols], out, append = TRUE)
  }

  # read the json lines
  readr::read_lines_chunked(scopus_json,
    callback = SideEffectChunkCallback$new(pj),
    chunk_size = chunk_size, progress = TRUE)

  # variable nr of fields/cols -> merge into one flatfile
  csv_files <- dir(td, pattern = "scopus-\\d+.csv", full.names = TRUE)
  on.exit(unlink(csv_files))

  df <-
    csv_files %>%
    purrr::map(function(x) readr::read_csv(x, col_types =
      cols(issue = "c", article_number = "c", .default = col_guess()))) %>%
    purrr::map_df(bind_rows)

  df
}


# flat <- scopus_flatten("~/scopus/JSON_export_eids_20201009.json")
# write_csv(flat, "~/scopus/flat.csv")
# identical(flat, read_csv("~/scopus/flat.csv", guess_max = 4e4))
