
# https://www.r-bloggers.com/how-to-build-an-api-wrapper-package-in-10-minutes/

install.packages("devtools")
install.packages("roxygen2")
install.packages("usethis")
install.packages("curl")
install.packages("httr")
install.packages("jsonlite")
install.packages("attempt")
install.packages("purrr")
devtools::install_github("r-lib/desc")



library(devtools)
library(usethis)
library(desc)

unlink("DESCRIPTION")
my_desc <- description$new("!new")
my_desc$set("Package", "kthapi")
my_desc$set("Authors@R", "person('Markus', 'Skyttner', email = 'markussk@kth.se', role = c('cre', 'aut'))")

my_desc$del("Maintainer")

my_desc$set_version("0.0.0.9000")

# https://www.kth.se/api/anvand-data-fran-kth-1.57059
my_desc$set(Title = "Data from KTH APIs")
my_desc$set(Description = "The KTH APIs provides information about employee profiles, published web content, places, course schemas and program catalogues. This R package interfaces with the API, making data available to use from R.")
my_desc$set("URL", "https://github.com/KTH-Library/kthapi")
my_desc$set("BugReports", "https://github.com/KTH-Library/kthapi/issues")
my_desc$set("License", "MIT")

use_mit_license(name = "Markus Skyttner")
#use_code_of_conduct()
use_lifecycle_badge("Experimental")
use_news_md()
my_desc$write(file = "DESCRIPTION")

use_package("httr")
use_package("jsonlite")
use_package("curl")
use_package("attempt")
use_package("purrr")
use_package("progress")
use_package("dplyr")
use_package("tibble")

use_tidy_description()

use_testthat()
use_vignette("Using KTH APIs")
use_readme_rmd()
use_test("profiles")

#install.packages("roxygen2md")


use_lifecycle_badge("experimental")

use_pkgdown()

pkgdown::build_site()

# what about the nul bytes?
#library(dplyr)
#me <- bibliomatrix::ad_kthid("markussk")
#bibliomatrix:::ad_search_kthid(kthid = me, bibliomatrix:::ldap_config()) %>% View()

# updating embedded data

source("data-raw/altmetric_explorer.R")
source("data-raw/abm_units.R")

# TODO add function that can be used to update the database...

library(sinew)
sinew::makeOxygen(config)

sinew::interOxyAddIn()



