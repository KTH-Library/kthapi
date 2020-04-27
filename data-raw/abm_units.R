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

# manual attempts to correct some mappings, TODO: needs review!
df[10,]$idx <- 34
df[12,]$idx <- 36
df[13,]$idx <- 37
df[22,]$idx <- 24
df[24,]$idx <- 27
df[26,]$idx <- 28
df[29,]$idx <- 35 # NB: Check this!
df[30,]$idx <- 38 # NB: Check this!
df[31,]$idx <- 54 # NB: Check this!
df[32,]$idx <- 38 # NB: Check this!
df[33,]$idx <- 45
df[34,]$idx <- 12 # NB: Check this!
df[35,]$idx <- 19 # NB: Check this!

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
  left_join(bibliomatrix::abm_public_kth$meta) %>%
  left_join(altmetric_explorer, by = c("Diva_org_id" = "diva_id")) %>%
  select(unit_long_en, `description.en`, kthid, slug, altmetric_id, desc, count, href, everything())

abm_units <- bind_rows(abm_public_kth$meta %>% slice(1), connexions)

usethis::use_data(abm_units)
