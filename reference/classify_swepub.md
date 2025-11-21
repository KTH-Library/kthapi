# Classify into subject categories using SwePub Web API

Use the title, abstract and keywords to get a suggestion for subject
categories to use from the Swedish standard set of 3 or 5 digit subject
categories.

## Usage

``` r
classify_swepub(title, abstract, keywords, level = 3)
```

## Arguments

- title:

  the title of the paper

- abstract:

  the abstract

- keywords:

  keywords used

- level:

  either 3 or 5, default: 3

## Details

Calls the API at https://swepub-qa.kb.se/api/v1/classify

## Examples

``` r
if (FALSE) { # \dontrun{
classify_swepub(
   title = "Magnetic resonance",
   abstract = "This paper deals with magnetic resonance",
   keywords = "magnetic radiotherapy nuclear",
   level = 5
)
} # }
```
