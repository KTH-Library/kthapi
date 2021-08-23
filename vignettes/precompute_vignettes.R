library(knitr)

# NB: Remember to execute this script after changing the vignettes (*.orig)!
# Rationale: https://ropensci.org/blog/2019/12/08/precompute-vignettes/
#Sys.chmod("vignettes/precompute_vignettes.R")

scripts <- file.path("vignettes", c(
  "KTH-Departments-from-Altmetric-Explorer.Rmd",
  "Potential-Heads.Rmd",
  "Publications-API-Usage.Rmd",
  "Schools-Departments-from-KTH-Directory-API.Rmd"
))

orig <- function(x) paste0(x, ".orig")

#file.copy(scripts, orig(scripts))

reknit <- function(orig, new) knit(orig, new)

purrr::walk2(orig(scripts), scripts, reknit)

