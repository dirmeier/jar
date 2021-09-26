
# jar

<!-- badges: start -->

[![Project
Status](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![ci](https://github.com/dirmeier/jar/workflows/ci/badge.svg)](https://github.com/dirmeier/jar/actions?query=workflow%3Aci)
[![CRAN](http://www.r-pkg.org/badges/version/ggpixel?color=white)](https://cran.r-project.org/package=ggpixel)
<!-- badges: end -->

> Autograd for R

## Introduction

This package implements a proof-of-concept of reverse-mode autodiff for
R functions. The package is motivated and influenced by
[`jax`](https://github.com/google/jax) without being very mature or
computationally performant.

``` r
library(jar)

tanh <- function(x, y) {
  (1.0 - exp(-x)) / (1.0 + exp(-y))
}

grad(tanh, 1L)(1.0, 2.0)
```

    ## [1] 0.3240271

``` r
grad(tanh, 2L)(1.0, 2.0)
```

    ## [1] -0.0663686

``` r
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

    ## [1] -3

## Installation

Get the package using:

``` r
devtools::install_github("dirmeier/jar")
```

## Author

  - Simon Dirmeier
    <a href="mailto:simon.dirmeier @ web.de">simon.dirmeier @ web.de</a>
