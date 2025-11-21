# Classify a publication record by Swedish standard subject categories

Umeå University provides a web [api](https://klassificera.ub.umu.se/)
described [here](https://klassificera.ub.umu.se/api2.html), for
classifying english and swedish language records from DiVA or Swepub
based on the MODS format (v 3.2 or later) according to the [Swedish
standard](https://www.scb.se/dokumentation/klassifikationer-och-standarder/standard-for-svensk-indelning-av-forskningsamnen/).
Classification can also be made based on a [Web of Science
record](https://klassificera.ub.umu.se/uploadFile.txt) Uploaded record
batches may not exceed 200 MB, using batches of 10 MB are recommended
(around 2-3000 records per chunk)

## Usage

``` r
classify_umu_ub(
  record,
  type = c("mods", "wos"),
  threshold = 0.2,
  email = "foo.bar@null.se"
)
```

## Arguments

- record:

  string the publication identifier string in DiVA or an export file
  from Web of Science in the "Plain text/Full record" format

- type:

  string, type of identifier, one of "mods" or "wos", default: "mods"

- threshold:

  a value in between 0.1 and 0.51, which governs to which which extent a
  record is classified with more than one subject area

- email:

  email adress for the user

## Value

a tibble with suggested classifications

## Details

The classification is based on information in there record from the
title and abstract (required) but also on keywords, ISSN/journal,
ISBN-prefix/ publisher and affiliations. Training data comes from SwePub
(July 2020). It is based on the following papers:

- Dual Coordinate Descent Methods for Logistic Regression and Maximum
  Entropy Models.
  ([doi:10.1007/s10994-010-5221-8](https://kth-library.github.io/kthapi/reference/%5Cdoi%7B10.1007/s10994-010-5221-8%7D)

- Entropy-Based Term Weighting Schemes for Text Categorization in VSM.
  ([doi:10.1109/ICTAI.2015.57](https://kth-library.github.io/kthapi/reference/%5Cdoi%7B10.1109/ICTAI.2015.57%7D)

The classification is made at the research topic level (5 digits) for
English language records and at research subject group level 3 digits
for Swedish language records.

## Examples

``` r
if (FALSE) { # \dontrun{

# classify using a DiVA record identifier
classify_umu_ub("diva2:515038", threshold = 0.3, email = "john.doe@hotmail.com")

# classify using a WoS Record in Flatfile/Plain text format
wos_record <- readr::read_lines("https://klassificera.ub.umu.se/uploadFile.txt")
classify_umu_ub(wos_record, type = "wos")
} # }
```
