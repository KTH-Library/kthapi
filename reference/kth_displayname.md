# Retrieve display name given user (kthid or accountname) using authenticated Profiles v1 API

Retrieve display name given user (kthid or accountname) using
authenticated Profiles v1 API

## Usage

``` r
kth_displayname(user, type = c("kthid", "username"), cfg = config())
```

## Arguments

- user:

  a string with the account name or KTH user id

- type:

  one of "kthid" or "username", where "kthid" is the default if not
  specified

- cfg:

  configuration setting for the KTH APIs including base URL etc, by
  default from config()
