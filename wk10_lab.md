pm566_wk10_lab
================
Yiping Li
2022-11-02

``` r
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ stringr 1.4.0
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.1
    ## ✔ readr   2.1.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(parallel)
```

\#problem 2: create a n x k matrix of Poisson

``` r
set.seed(1235)
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}
f1<-fun1()
mean(f1)
```

    ## [1] 4.1575

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  x<-matrix(rpois(n*k, lambda),ncol=4)
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

    ## Unit: microseconds
    ##       expr     min      lq      mean   median       uq      max neval
    ##     fun1() 301.975 440.979 643.16530 469.9595 632.1895 3890.677   100
    ##  fun1alt()  19.123  22.280  48.90293  23.6930  29.4585 2217.349   100

``` r
d <- matrix(1:16, ncol=4)
d[2,1]
```

    ## [1] 2

``` r
d[2]
```

    ## [1] 2

``` r
diag(d)
```

    ## [1]  1  6 11 16

``` r
d[c(1,6,11,16)]
```

    ## [1]  1  6 11 16

``` r
cbind(1:4,1:4)
```

    ##      [,1] [,2]
    ## [1,]    1    1
    ## [2,]    2    2
    ## [3,]    3    3
    ## [4,]    4    4

``` r
d[cbind(1:4,1:4)]
```

    ## [1]  1  6 11 16

\#problem3: find the column max (hint: Checkout the function max.col()).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
M <- matrix(runif(12), ncol=4)
M
```

    ##           [,1]      [,2]        [,3]      [,4]
    ## [1,] 0.1137034 0.6233794 0.009495756 0.5142511
    ## [2,] 0.6222994 0.8609154 0.232550506 0.6935913
    ## [3,] 0.6092747 0.6403106 0.666083758 0.5449748

``` r
# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
fun2(x=M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
fun2alt <- function(x) {
  # YOUR CODE HERE
   idx <- max.col( t(x))
   x[cbind(idx,1:4)]
}
fun2alt(x=M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
x <- matrix(rnorm(1e4), nrow=10)
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr      min       lq      mean   median        uq      max neval
    ##     fun2(x) 1054.967 1096.162 1315.5739 1177.095 1351.8680 6775.498   100
    ##  fun2alt(x)  104.636  146.665  191.4581  159.904  183.2775 2531.081   100

\#problem4: how PSOCK cluster example

``` r
library(parallel)

my_boot <- function(dat, stat, R, ncpus = 1L) 
{
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  
  cl <- makePSOCKcluster(4)  
  clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`
  # STEP 2: GOES HERE
  
  clusterExport(cl,c("stat","dat","idx"),envir=environment())
  
  # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply( cl,seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  ans
  
}
```

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 1e4
x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)

#stopCluster(cl)
# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##   0.191   0.039   2.084

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##   0.185   0.029   2.078
