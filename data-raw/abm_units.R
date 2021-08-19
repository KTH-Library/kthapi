# NB: This depends on first having the altmetric_explorer_highlights dataset present!
# so please first run altmetric_explorer.R script to make sure those are up to date

library(bibliomatrix)
library(kthapi)
library(dplyr)

# first check that kthids or descriptions have not changed when joining by slug against Catalogue API data

bibmon <- con_bib("mssql")
org <- bibmon %>% tbl("abm_org_info") %>% collect() %>% arrange(sort_order)
DBI::dbDisconnect(bibmon)
ksd <- kth_school_dep()

ok_slugmatch <-
  org %>%
  left_join(ksd, by = "slug") %>%
  mutate(is_match = (kthid.x == kthid.y & description_en == description.en)) %>%
  # exclude the root node for KTH - it has no slug/kthid
  filter(unit_code != "KTH") %>%
  pull(is_match) %>%
  all() %>%
  isTRUE()

if (!ok_slugmatch)
  warning("It seems that some slugs have been reorganized or changed kthids? Why?")

# update desc, count and href with data from Altmetric Explorer

abm_units <-
  org %>%
  left_join(ksd, by = "slug") %>%
  select(-starts_with("altmetric")) %>%
  left_join(altmetric_explorer, by = c("Diva_org_id" = "diva_id"))

# fill Altmetric root node values (for all of KTH) "manually"

idx <- which(org$unit_code == 'KTH')
abm_units[idx, ]$description.en <- abm_units[idx, ]$unit_long_en
abm_units[idx, ]$desc <- abm_units[idx, ]$unit_long_en

abm_units[idx, ]$count <-
  altmetric_explorer_highlights %>%
  filter(indicator == "Outputs with attention") %>%
  pull(value)

#This gives "Total outputs tracked", not "Outputs with attention"
#abm_units[1,]$count <- altmetric_explorer_attention

altmetric_id_root <- "a76346801b570c19effb1ae8692c87fc"
abm_units[idx, ]$level <- "department root"
abm_units[idx, ]$altmetric_id <- altmetric_id_root
abm_units[idx, ]$href <- sprintf(
    paste0("https://www.altmetric.com/explorer/highlights",
    "?department_id=9fa9aaf7-92b0-4911-9718-cf2a7e34a2f1",
    ":department:%s"), altmetric_id_root)

#View(abm_units)

# ensure that the data which has been updated with Altmetrics
# conforms to the field names used in abm_org_info table in the db

abm_units <-
  abm_units %>%
  dplyr::rename(
    "altmetric_level" = level,
    "altmetric_count" = count,
    "altmetric_href" = href,
    "altmetric_desc" = desc,
    "kthid" = kthid.x) %>%
  mutate(description_en = `description.en`) %>%
  select(-c(`description.en`, kthid.y)) %>%
  select(contains(names(org))) %>%
  arrange(sort_order)

# check if the public data in bibliomatrix matches
# otherwise recommend an update

check <-
  bibliomatrix::abm_public_kth$meta ==
  abm_units %>% select(names(bibliomatrix::abm_public_kth$meta))

if (length(which(check == FALSE)) > 0) {
  warning("Found difference with bibliomatrix package data, ",
          "recommending database update/sync and update of bibliomatrix")
  daff::render_diff(daff::diff_data(abm_units, bibliomatrix::abm_public_kth$meta))
}

check2 <- org == abm_units

if (length(which(check2 == FALSE)) > 0) {
  warning("Found difference with database table, ",
          "recommending database update/sync")
  daff::render_diff(daff::diff_data(org, abm_units))
}

usethis::use_data(abm_units, overwrite = TRUE)

# function to sync with the database

update_mssql_abm_units <- function() {

  library(odbc)

  if(startsWith(Sys.getenv("OS"), "Windows")) {
    # encoding Windows-1252 curiously gives neat UTF-8 output from DB on Windows
    # (while encoding UTF-8 does not)
    con_bibmon <- dbConnect(odbc(), driver = "ODBC Driver 17 for SQL Server",
                            server = Sys.getenv("DBHOST"), database = Sys.getenv("DBNAME"),
                            Port = 1433, UID = Sys.getenv("DBADMINUSER"), PWD = Sys.getenv("DBADMINPASS"),
                            encoding = "Windows-1252")
  } else {
    con_bibmon <- dbConnect(odbc(), driver = "ODBC Driver 17 for SQL Server",
                            server = Sys.getenv("DBHOST"), database = Sys.getenv("DBNAME"),
                            Port = 1433, UID = Sys.getenv("DBADMINUSER"), PWD = Sys.getenv("DBADMINPASS"))

  }

  dbWriteTable(con_bibmon, "abm_org_info", abm_units, overwrite = TRUE)

}

# update_mssql_abm_units()

