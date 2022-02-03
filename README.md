
# EMFtoolbox

An amazing package to do internal things for the EMF

## Installation

You can install the development version of EMFtoolbox from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("emf-creaf/EMFtoolbox")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(EMFtoolbox)
collect_metadata(.dry = TRUE)
#> ---
#> id: EMFtoolbox
#> emf_type: softwork
#> emf_public: false
#> emf_automatized: true
#> emf_reproducible: false
#> emf_draft: false
#> nodes:
#> - style_guide
#> - development_guide
#> authors:
#> - vgranda
#> - mcaceres
#> - rmolowny
#> thematic: emf_internal
#> tags:
#> - R
#> - server
#> - devops
#> requirements: R (>= 4)
#> date: '2021-08-11'
#> date_lastmod: '2022-02-02'
#> links:
#>   url_source: https://github.com/emf-creaf/EMFtoolbox
#> Package: EMFtoolbox
#> Title: Tool Box For The EMFverse
#> Version: 0.0.0.9000
#> Description: Tool box for the EMFverse.
#> License: MIT + file LICENSE
#> Encoding: UTF-8
#> LazyData: true
#> Roxygen: list(markdown = TRUE)
#> RoxygenNote: 7.1.1
#> ---
```
