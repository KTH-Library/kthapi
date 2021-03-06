% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diva.R
\name{kth_diva_authors}
\alias{kth_diva_authors}
\title{Retrieve DiVA authors for KTH from the KTH DiVA portal}
\usage{
kth_diva_authors(
  orgid = "177",
  year_beg = "2012",
  year_end = "2019",
  use_cache = TRUE,
  refresh_cache = FALSE
)
}
\arguments{
\item{orgid}{the DiVA organisation id, by default "177" for KTH}

\item{year_beg}{the beginning of the period, by default "2012"}

\item{year_end}{the end of the period, by default "2019"}

\item{use_cache}{logical flag to indicate if cached data should be used,
default: TRUE}

\item{refresh_cache}{logical flag to indicate if data cache should be
refreshed, default: FALSE}
}
\value{
data frame with results
}
\description{
This function returns parsed author information from DiVA data
}
