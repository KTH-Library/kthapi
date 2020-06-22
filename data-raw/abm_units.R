# NB: This depends on first having the altmetric_explorer_highlights dataset present!

library(bibliomatrix)
library(kthapi)

org <- bibliomatrix::abm_public_kth$meta

a <- org$unit_long_en
b <- kth_school_dep()$`description.en`

# what "catalogs" are available under "m/m"?
#c <- kth_catalog(slug = "m/m")
#c$catalogs

# if I want to merge w "b"
#additions <- c$catalogs %>% pull(`description.en`)
#b <- c(b, additions)

# there are some direct matches
a[toupper(a) %in% b]

# we try to match using low values for string distances (poor man's record linkage)
library(stringdist)
library(tidyr)

m <-
  adist(toupper(a), b, partial = FALSE, counts=T)

mt <- m %>% as_tibble()

library(purrr)

mtmin <-
  apply(mt, 1, function(x) which(min(x) == x))

altids <-
  mtmin %>% map_chr(paste, collapse = ",")

idx <-
  mtmin %>% map_int(function(x) x[1])

lookup <-
  tibble(a, b[idx], idx, altids)

df <- as.data.frame(lookup)

# manual attempts to correct some mappings

df[7,]$idx <- 17
df[10,]$idx <- 32
df[11,]$idx <- 33
df[12,]$idx <- 34
df[13,]$idx <- 35
df[16,]$idx <- 61
df[21,]$idx <- 23
df[22,]$idx <- 22
df[24,]$idx <- 25
df[25,]$idx <- 27
df[26,]$idx <- 26
df[31,]$idx <- 44

mapping <-
  tibble(a = df$a, idx = df$idx) %>%
  left_join(tibble(idx = 1:length(b), b = b)) %>%
  select(unit_long_en = a, `description.en` = b)

# combine to get kthid, slug, altmetric_id etc
connexions <-
  mapping %>%
  slice(-1) %>%
  left_join(kth_school_dep()) %>%
  select(unit_long_en, `description.en`, kthid, slug) %>%
  left_join(bibliomatrix::abm_public_kth$meta %>% select(-starts_with("altmetric"))) %>%
  left_join(altmetric_explorer, by = c("Diva_org_id" = "diva_id")) %>%
  select(unit_long_en, `description.en`, kthid, slug, desc, count, href, everything())

abm_units <- bind_rows(
  abm_public_kth$meta %>% slice(1) %>% select(-starts_with("altmetric")), connexions)


# fill Altmetric root node values (for all of KTH) "manually"

abm_units[1, ]$description.en <- abm_units[1, ]$unit_long_en
abm_units[1, ]$desc <- abm_units[1, ]$unit_long_en

#abm_units[1, ]$count <-
#  altmetric_explorer_highlights %>%
#  filter(indicator == "Outputs with attention") %>%
#  pull(value)

abm_units[1,]$count <- altmetric_explorer_attention

altmetric_id_root <- "a76346801b570c19effb1ae8692c87fc"
abm_units[1, ]$level <- "department root"
abm_units[1, ]$altmetric_id <- altmetric_id_root
abm_units[1, ]$href <- sprintf(
    paste0("https://www.altmetric.com/explorer/highlights",
    "?department_id=9fa9aaf7-92b0-4911-9718-cf2a7e34a2f1",
    ":department:%s"), altmetric_id_root)

#View(abm_units)

abm_units <-
  abm_units %>%
  dplyr::rename(
    "altmetric_level" = level,
    "altmetric_count" = count,
    "altmetric_href" = href,
    "altmetric_desc" = desc) %>%
  mutate(description_en = `description.en`) %>%
  select(-`description.en`)

check <-
  bibliomatrix::abm_public_kth$meta ==
  abm_units %>% select(names(bibliomatrix::abm_public_kth$meta))

if (length(which(check == FALSE)) > 0) {
  warning("Found difference in data, recommending data base update/sync")
  View(check)
}

#setdiff(
#  abm_units %>% select(contains("altmetric")),
#  bibliomatrix::abm_public_kth$meta %>% select(contains("altmetric"))
#)

usethis::use_data(abm_units, overwrite = TRUE)



# function to sync with the database

update_mssql_abm_units <- function() {

  library(odbc)

  con_bibmon <- dbConnect(odbc(), driver = "ODBC Driver 17 for SQL Server",
    server = Sys.getenv("DBHOST"), database = Sys.getenv("DBNAME"),
    Port = 1433, UID = Sys.getenv("DBADMINUSER"), PWD = Sys.getenv("DBADMINPASS"))

  dbWriteTable(con_bibmon, "abm_org_info", abm_units, overwrite = TRUE)

}

