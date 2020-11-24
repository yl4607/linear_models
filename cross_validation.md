Cross Validation
================
Yue Liu
2020-11-24

when you have lots of possible variable, you have to choose which ones
will fo in your model.

For nested models, you have tests \[you have to be worried about
multiple comparisons and “fishing”\]

For non-nested models, you don’t have tests\[AIC/ BIC / etc are
traditional tools; Balance goodness of fit with “complexity”\]

randomly split data into “training” and “testing”

evaluate using root mean square error

Refinements and vatiations: Individual training/testing splits are
subjects to randomness; Repeat the process(illustrates variability in
prediction accuracy)

Cross validation is general

can use to compare candidate models that are all “traditional”

comes up a lot in “modern” methods (automed variable selection (lasso),
additive models, regression trees)

## Simulate data

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )
```

Look at the data

``` r
nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />
