# DiVA publication records from KTH with subject classification issues

DiVA publication records from KTH with subject classification issues

## Usage

``` r
issues_swepub(
  institution = "kth",
  year_beg = 2012L,
  year_end = 2020L,
  page_size = 20
)
```

## Arguments

- institution:

  the institution string, Default: 'kth'

- year_beg:

  filter from year, Default: 2012

- year_end:

  filter to year, Default: 2020

- page_size:

  for paged responses, the page length, Default: 20

## Value

a tibble with results

## Details

Data comes from [](https://bibliometri.swepub.kb.se/process)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 issues_swepub()
 }
} # }
```
