<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/manymome.table.svg)](https://github.com/sfcheung/manymome.table)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/manymome.table.svg)](https://github.com/sfcheung/manymome.table/commits/main)
[![R-CMD-check](https://github.com/sfcheung/manymome.table/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/manymome.table/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.2.0, updated on 2023-11-08, [release history](https://sfcheung.github.io/manymome.table/news/index.html))

# manymome.table

A collection of helper functions for converting
selected results of [`manymome`](https://sfcheung.github.io/manymome/)
([Cheung & Cheung, 2023](https://doi.org/10.3758/s13428-023-02224-z)) to publication-ready
tables.

It currently supports the `flextable` format from
the [`flextable` package](https://davidgohel.github.io/flextable/).
Results from `manymome::many_indirect_effects()`
and `manymome::cond_indirect_effects()` can be converted
to `flextable` objects using the method `as_flextable()`.
They can then be exported to other formats, such as Word.

For more information on this package, please visit its GitHub page:

https://sfcheung.github.io/manymome.table/

# Installation

Stable release versions of this package can be downloaded below:

https://github.com/sfcheung/manymome.table/releases

The latest developmental version of this package can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/manymome.table")
```

# Reference

Cheung, S. F., & Cheung, S.-H. (2023). *manymome*: An R package for computing
the indirect effects, conditional effects, and conditional indirect effects,
standardized or unstandardized, and their bootstrap confidence intervals,
in many (though not all) models. *Behavior Research Methods*.
https://doi.org/10.3758/s13428-023-02224-z

# Issues

If you have any suggestions and found any bugs, please feel
feel to open a GitHub issue. Thanks.