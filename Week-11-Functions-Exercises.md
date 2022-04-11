Functions
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
    ## ✓ readr   2.1.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
df <- tibble::tibble(
a = c(rnorm(9), -99),
b = c(-999, -99, rnorm(8)),
c = c(0, rnorm(9)),
d = rnorm(10)
)
```

``` r
(df$a <- (df$a - min(df$a)) / (max(df$a) - min(df$a)))
```

    ##  [1] 0.9890424 0.9696941 0.9999646 0.9948581 1.0000000 0.9870558 0.9795791
    ##  [8] 0.9860078 0.9859407 0.0000000

``` r
(df$b <- (df$b - min(df$b)) / (max(df$b) - min(df$b)))
```

    ##  [1] 0.0000000 0.9001287 0.9983990 0.9983867 0.9991504 1.0000000 0.9995424
    ##  [8] 0.9994799 0.9994478 0.9972975

``` r
(df$c <- (df$c - min(df$c)) / (max(df$c) - min(df$c)))
```

    ##  [1] 0.64730385 0.65409645 0.41036128 1.00000000 0.00000000 0.41695778
    ##  [7] 0.46573382 0.26597976 0.51639790 0.06161705

``` r
(df$d <- (df$d - min(df$d)) / (max(df$d) - min(df$c)))
```

    ##  [1] 2.4835223 1.3736475 1.5462923 1.4405493 0.8073599 1.3771460 1.8418871
    ##  [8] 1.7435895 0.5119855 0.0000000

Write a function to automate the calculation of the rescale:

``` r
rescale01 <- function(x) {
  rng <- range(df$a)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(df$a)
```

    ##  [1] 0.9890424 0.9696941 0.9999646 0.9948581 1.0000000 0.9870558 0.9795791
    ##  [8] 0.9860078 0.9859407 0.0000000

``` r
rescale01(df$b)
```

    ##  [1] 0.0000000 0.9001287 0.9983990 0.9983867 0.9991504 1.0000000 0.9995424
    ##  [8] 0.9994799 0.9994478 0.9972975

``` r
rescale01(df$c)
```

    ##  [1] 0.64730385 0.65409645 0.41036128 1.00000000 0.00000000 0.41695778
    ##  [7] 0.46573382 0.26597976 0.51639790 0.06161705

``` r
rescale01(df$d)
```

    ##  [1] 2.4835223 1.3736475 1.5462923 1.4405493 0.8073599 1.3771460 1.8418871
    ##  [8] 1.7435895 0.5119855 0.0000000

## Your Turn 1

Rerun the tibble to revert back to the original data. Write a function
to replace -99 with NA, called `fix_missing()`

``` r
df <- tibble::tibble(
a = c(rnorm(9), -99),
b = c(-999, -99, rnorm(8)),
c = c(0, rnorm(9)),
d = rnorm(10)
)


#Here are the repeated calculations
df$a[df$a==-99]<-NA
df$b[df$b==-99]<-NA
df$c[df$c==-99]<-NA
df$d[df$d==-99]<-NA
```

``` r
fix_missing <- function(x) {
  
  x[x==-99]<-NA
  x
  
}

fix_missing(df$a)
```

    ##  [1] -1.6756200  0.3654829 -0.4744877 -0.6850884  0.5441496  2.1893299
    ##  [7]  0.9127866  1.6965229  0.3708805         NA

Now let’s write a function that allows us to easily adapt our rescaling
to include a range (min, max) to modify the data.

``` r
#Rescale to [0, 1]
0 + (1 - 0) * ((df$a - min(df$a)) / (max(df$a) - min(df$a)))
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
# Rescale to [-1, 1]
-1 + (1 - -1) * ((df$b - min(df$b)) / (max(df$b) - min(df$b)))
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
# Rescale to [0, 10]
0 + (10 - 0) * ((df$c - min(df$c)) / (max(df$c) - min(df$c)))
```

    ##  [1]  4.082281  5.974685  0.000000  2.287181 10.000000  8.494158  5.860072
    ##  [8]  2.902594  4.324677  2.281474

``` r
rescale <- function(x, min = 0 , max = 1) {
  min + (max - min) * ((x - min(x)) / (max(x) - min(x)))
}

rescale(df$a)
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
rescale(df$b, min = -1, max = 1)
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA

``` r
rescale(df$c, max = 10)
```

    ##  [1]  4.082281  5.974685  0.000000  2.287181 10.000000  8.494158  5.860072
    ##  [8]  2.902594  4.324677  2.281474

## Your Turn 2

Expand your function from ‘Your Turn 1’ to allow for any possible
missing value type.

``` r
fix_missing <- function(x, k) {
  
  x[x==k]<-NA
  x
  
}

fix_missing(df$a, -99)
```

    ##  [1] -1.6756200  0.3654829 -0.4744877 -0.6850884  0.5441496  2.1893299
    ##  [7]  0.9127866  1.6965229  0.3708805         NA

``` r
fix_missing(df$b, -999)
```

    ##  [1]         NA         NA -0.9558427 -0.6712094 -0.1561436  1.5118198
    ##  [7]  0.8397790 -0.3882309 -0.2261429  1.3344890

# Take Aways

To write a function,

1.  Write code that solves the problem for a real object  
2.  Wrap the code in `function(){}` to save it  
3.  Add the name of the real object as the function argument

This sequence will help prevent bugs in your code (and reduce the time
you spend correcting bugs).
