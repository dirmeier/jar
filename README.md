
# jar

<!-- badges: start -->

[![Project
Status](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![R build
status](https://github.com/dirmeier/jar/workflows/R-CMD-check/badge.svg)](https://github.com/dirmeier/jar/actions)
[![Codecov test
coverage](https://codecov.io/gh/dirmeier/jar/branch/master/graph/badge.svg)](https://codecov.io/gh/dirmeier/jar?branch=master)
<!-- badges: end -->

> Reverse-mode autodiff for R

## Introduction

This package implements a proof-of-concept of reverse-mode autodiff for
R functions. The package is motivated and influenced by
[`jax`](https://github.com/google/jax) without being very mature or
computationally performant.

``` r
library(jar)

logpdf <- function(y, mu, sigma) {
  -0.5 * ((y - mu) / sigma) ^ 2 - log(sigma) - 0.5 * log(2*pi)
}

grad(logpdf, 1L)(1.0, 0.0, 1.0)
```

    ## [1] -1

``` r
grad(logpdf, 2L)(1.0, 0.0, 1.0)
```

    ## [1] 1

``` r
grad(logpdf, 3L)(1.0, 0.0, 1.0)
```

    ## [1] 0

## Installation

Get the package using:

``` r
devtools::install_github("dirmeier/jar")
```

## Author

  - Simon Dirmeier
    <a href="mailto:simon.dirmeier @ web.de">simon.dirmeier @ web.de</a>
