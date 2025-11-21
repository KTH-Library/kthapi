# Search Active Directory at KTH

This function uses ldapsearch to query the KTH Active Directory. It
requires environment variables to be set in .Renviron, specifically
LDAP_USER, LDAP_PASS, LDAP_HOST and LDAP_BASE for the service account
used for the queries.

## Usage

``` r
ldap_search(ldap_query, ldap_attributes = NULL, cfg = ldap_config())
```

## Arguments

- ldap_query:

  the LDAP query to issue, such as 'ugKthid=\*'

- ldap_attributes:

  set of attributes to return, by default NULL but can be a character
  vector of attributes, such as c('ugKthid', 'ugOrcid')

- cfg:

  the connection credentials, by default given from ldap_config()

## Value

tibble with results

## Details

The ldapsearch command makes use of -E pr=2147483647/noprompt to avoid
paging and returns results in LDIF format which is parsed into a tibble.

It is possible to disable ldaps TLS require certificate check, by
setting the environment variable LDAPTLS_REQCERT to "never" (can be done
in .Renviron)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 ldap_search("(&(ugOrcid=*)(ugKthid=*))", c("ugKthid", "ugOrcid"))
 }
} # }
```
