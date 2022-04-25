
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Summary

This repo is a performs a Type I and Type II simulation study on all
tests in the MLTesteR package.

    #> -- Attaching packages ------------------------------------------------------------------------------------------------------------------------------------------------ tidyverse 1.3.0 --
    #> v ggplot2 3.3.2     v purrr   0.3.4
    #> v tibble  3.0.3     v dplyr   1.0.0
    #> v tidyr   1.1.0     v stringr 1.4.0
    #> v readr   1.3.1     v forcats 0.5.0
    #> -- Conflicts --------------------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    #> x dplyr::filter() masks stats::filter()
    #> x dplyr::lag()    masks stats::lag()

## Gaussian

    #> `summarise()` regrouping output by 'test', 'alt', 'mu' (override with `.groups` argument)

<img src="man/figures/README-gaussainTypeI-1.png" width="100%" /><img src="man/figures/README-gaussainTypeI-2.png" width="100%" /><img src="man/figures/README-gaussainTypeI-3.png" width="100%" /><img src="man/figures/README-gaussainTypeI-4.png" width="100%" />

## Gamma

    #> `summarise()` regrouping output by 'test', 'alt', 'shape', 'rate' (override with `.groups` argument)

<img src="man/figures/README-gammaTypeI-1.png" width="100%" /><img src="man/figures/README-gammaTypeI-2.png" width="100%" /><img src="man/figures/README-gammaTypeI-3.png" width="100%" /><img src="man/figures/README-gammaTypeI-4.png" width="100%" /><img src="man/figures/README-gammaTypeI-5.png" width="100%" />

## Poisson

    #> `summarise()` regrouping output by 'test', 'alt' (override with `.groups` argument)

<img src="man/figures/README-poissonTypeI-1.png" width="100%" /><img src="man/figures/README-poissonTypeI-2.png" width="100%" /><img src="man/figures/README-poissonTypeI-3.png" width="100%" />

## Beta

    #> `summarise()` regrouping output by 'test', 'alt', 'shape1' (override with `.groups` argument)

<img src="man/figures/README-betaTypeI-1.png" width="100%" /><img src="man/figures/README-betaTypeI-2.png" width="100%" /><img src="man/figures/README-betaTypeI-3.png" width="100%" /><img src="man/figures/README-betaTypeI-4.png" width="100%" />

## Negative Binomial

    #> `summarise()` regrouping output by 'test', 'alt', 'p' (override with `.groups` argument)

<img src="man/figures/README-negativeBonimialTypeI-1.png" width="100%" /><img src="man/figures/README-negativeBonimialTypeI-2.png" width="100%" /><img src="man/figures/README-negativeBonimialTypeI-3.png" width="100%" /><img src="man/figures/README-negativeBonimialTypeI-4.png" width="100%" />

    #> `summarise()` regrouping output by 'test', 'p' (override with `.groups` argument)

<img src="man/figures/README-negativeBonimialTypeI-5.png" width="100%" />

    #> `summarise()` regrouping output by 'test', 'alt', 'p' (override with `.groups` argument)

<img src="man/figures/README-negativeBonimialTypeI-6.png" width="100%" /><img src="man/figures/README-negativeBonimialTypeI-7.png" width="100%" /><img src="man/figures/README-negativeBonimialTypeI-8.png" width="100%" /><img src="man/figures/README-negativeBonimialTypeI-9.png" width="100%" />

    #> `summarise()` regrouping output by 'test', 'p' (override with `.groups` argument)

<img src="man/figures/README-negativeBonimialTypeI-10.png" width="100%" />

    #> `summarise()` ungrouping output (override with `.groups` argument)
    #> # A tibble: 2 x 4
    #>   test                        minTypeI meanTypeI maxTypeI
    #>   <chr>                          <dbl>     <dbl>    <dbl>
    #> 1 exact                              0    0.0443   0.0542
    #> 2 negative_binomial_p_lr_test        0    0.0507   0.238

## Geometric

    #> `summarise()` regrouping output by 'test', 'alt' (override with `.groups` argument)

<img src="man/figures/README-geometricTypeI-1.png" width="100%" /><img src="man/figures/README-geometricTypeI-2.png" width="100%" /><img src="man/figures/README-geometricTypeI-3.png" width="100%" />

    #> `summarise()` regrouping output by 'test', 'alt' (override with `.groups` argument)

<img src="man/figures/README-geometricTypeI-4.png" width="100%" /><img src="man/figures/README-geometricTypeI-5.png" width="100%" /><img src="man/figures/README-geometricTypeI-6.png" width="100%" />

    #> [1] 0.0504

## Beta

    #> `summarise()` regrouping output by 'test', 'alt' (override with `.groups` argument)

<img src="man/figures/README-exponentialTypeI-1.png" width="100%" /><img src="man/figures/README-exponentialTypeI-2.png" width="100%" /><img src="man/figures/README-exponentialTypeI-3.png" width="100%" />
