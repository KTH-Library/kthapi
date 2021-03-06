% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directory.R
\name{kth_root}
\alias{kth_root}
\title{Retrieve root data from KTH Directory API}
\usage{
kth_root(config = NULL, path, lang)
}
\arguments{
\item{config}{a configuration setting for the KTH APIs including base URL etc, by default from config()}

\item{path}{slug for directory, by default "root"}

\item{lang}{language to use, by default "en"}
}
\value{
results records returned from the search
}
\description{
See details at \url{https://api.kth.se/api/directory/swagger/}
}
\examples{
\dontrun{
kth_root()
}
}
