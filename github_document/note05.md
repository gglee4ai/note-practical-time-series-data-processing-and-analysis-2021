R Notebook
================

``` r
load("./data-tidied/dataset.rdata")
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 5 시계열 forecasting Part I - 기초 개념

## 5.1 정상성, 비정상성

## 5.2지연과 차분

``` r
students |>
  select(연도, 학생수계) |>
  mutate(
    lag1 = lag(학생수계, 1),
    lag3 = lag(학생수계, 3)
  )
```

    ## # A tibble: 22 × 4
    ##    연도       학생수계    lag1    lag3
    ##    <date>        <dbl>   <dbl>   <dbl>
    ##  1 1999-01-01  8658358      NA      NA
    ##  2 2000-01-01  8535867 8658358      NA
    ##  3 2001-01-01  8414423 8535867      NA
    ##  4 2002-01-01  8361933 8414423 8658358
    ##  5 2003-01-01  8379775 8361933 8535867
    ##  6 2004-01-01  8371630 8379775 8414423
    ##  7 2005-01-01  8371421 8371630 8361933
    ##  8 2006-01-01  8354891 8371421 8379775
    ##  9 2007-01-01  8309932 8354891 8371630
    ## 10 2008-01-01  8187782 8309932 8371421
    ## # … with 12 more rows

``` r
library(timetk)
students |>
  select(연도, 학생수계) |>
  mutate(
    lag1 = lag_vec(학생수계, lag = 1),
    lag3 = lag_vec(학생수계, lag = 3)
  )
```

    ## # A tibble: 22 × 4
    ##    연도       학생수계    lag1    lag3
    ##    <date>        <dbl>   <dbl>   <dbl>
    ##  1 1999-01-01  8658358      NA      NA
    ##  2 2000-01-01  8535867 8658358      NA
    ##  3 2001-01-01  8414423 8535867      NA
    ##  4 2002-01-01  8361933 8414423 8658358
    ##  5 2003-01-01  8379775 8361933 8535867
    ##  6 2004-01-01  8371630 8379775 8414423
    ##  7 2005-01-01  8371421 8371630 8361933
    ##  8 2006-01-01  8354891 8371421 8379775
    ##  9 2007-01-01  8309932 8354891 8371630
    ## 10 2008-01-01  8187782 8309932 8371421
    ## # … with 12 more rows

``` r
stats::lag(students.xts$학생수계, 1)
```

    ##            학생수계
    ## 1999-01-01       NA
    ## 2000-01-01  8658358
    ## 2001-01-01  8535867
    ## 2002-01-01  8414423
    ## 2003-01-01  8361933
    ## 2004-01-01  8379775
    ## 2005-01-01  8371630
    ## 2006-01-01  8371421
    ## 2007-01-01  8354891
    ## 2008-01-01  8309932
    ## 2009-01-01  8187782
    ## 2010-01-01  8016924
    ## 2011-01-01  7807663
    ## 2012-01-01  7586266
    ## 2013-01-01  7370308
    ## 2014-01-01  7173904
    ## 2015-01-01  6973154
    ## 2016-01-01  6806411
    ## 2017-01-01  6621547
    ## 2018-01-01  6454281
    ## 2019-01-01  6295366
    ## 2020-01-01  6122198

``` r
students |>
  select(연도, 학생수계) |>
  mutate(
    lag1 = lag(학생수계, 1),
    lag3 = lag(학생수계, 3),
    diff1 = c(NA, diff(학생수계, lag = 1)),
    diff3 = c(NA, NA, NA, diff(학생수계, lag = 3))
  )
```

    ## # A tibble: 22 × 6
    ##    연도       학생수계    lag1    lag3   diff1   diff3
    ##    <date>        <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 1999-01-01  8658358      NA      NA      NA      NA
    ##  2 2000-01-01  8535867 8658358      NA -122491      NA
    ##  3 2001-01-01  8414423 8535867      NA -121444      NA
    ##  4 2002-01-01  8361933 8414423 8658358  -52490 -296425
    ##  5 2003-01-01  8379775 8361933 8535867   17842 -156092
    ##  6 2004-01-01  8371630 8379775 8414423   -8145  -42793
    ##  7 2005-01-01  8371421 8371630 8361933    -209    9488
    ##  8 2006-01-01  8354891 8371421 8379775  -16530  -24884
    ##  9 2007-01-01  8309932 8354891 8371630  -44959  -61698
    ## 10 2008-01-01  8187782 8309932 8371421 -122150 -183639
    ## # … with 12 more rows

``` r
students |>
  select(연도, 학생수계) |>
  mutate(
    diff1 = diff_vec(학생수계, lag = 1),
    diff3 = diff_vec(학생수계, lag = 3)
  )
```

    ## diff_vec(): Initial values: 8658358

    ## diff_vec(): Initial values: 8658358, 8535867, 8414423

    ## # A tibble: 22 × 4
    ##    연도       학생수계   diff1   diff3
    ##    <date>        <dbl>   <dbl>   <dbl>
    ##  1 1999-01-01  8658358      NA      NA
    ##  2 2000-01-01  8535867 -122491      NA
    ##  3 2001-01-01  8414423 -121444      NA
    ##  4 2002-01-01  8361933  -52490 -296425
    ##  5 2003-01-01  8379775   17842 -156092
    ##  6 2004-01-01  8371630   -8145  -42793
    ##  7 2005-01-01  8371421    -209    9488
    ##  8 2006-01-01  8354891  -16530  -24884
    ##  9 2007-01-01  8309932  -44959  -61698
    ## 10 2008-01-01  8187782 -122150 -183639
    ## # … with 12 more rows

``` r
diff(students.xts$학생수계, 1)
```

    ##            학생수계
    ## 1999-01-01       NA
    ## 2000-01-01  -122491
    ## 2001-01-01  -121444
    ## 2002-01-01   -52490
    ## 2003-01-01    17842
    ## 2004-01-01    -8145
    ## 2005-01-01     -209
    ## 2006-01-01   -16530
    ## 2007-01-01   -44959
    ## 2008-01-01  -122150
    ## 2009-01-01  -170858
    ## 2010-01-01  -209261
    ## 2011-01-01  -221397
    ## 2012-01-01  -215958
    ## 2013-01-01  -196404
    ## 2014-01-01  -200750
    ## 2015-01-01  -166743
    ## 2016-01-01  -184864
    ## 2017-01-01  -167266
    ## 2018-01-01  -158915
    ## 2019-01-01  -173168
    ## 2020-01-01  -126959

## 5.3 ACF와 PACF

``` r
acf(students$학생수계)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
acf(students$학생수계, plot = FALSE)
```

    ## 
    ## Autocorrelations of series 'students$학생수계', by lag
    ## 
    ##      0      1      2      3      4      5      6      7      8      9     10     11     12     13 
    ##  1.000  0.875  0.745  0.616  0.484  0.349  0.216  0.084 -0.041 -0.157 -0.255 -0.329 -0.377 -0.400

``` r
students |>
  select(학생수계) |>
  forecast::Acf()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
students |>
  select(학생수계) |>
  forecast::ggAcf()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
students |>
  select(학생수계) |>
  forecast::ggAcf(plot = FALSE)
```

    ## 
    ## Autocorrelations of series 'select(students, 학생수계)', by lag
    ## 
    ##      0      1      2      3      4      5      6      7      8      9     10     11     12     13 
    ##  1.000  0.875  0.745  0.616  0.484  0.349  0.216  0.084 -0.041 -0.157 -0.255 -0.329 -0.377 -0.400

``` r
students |>
  select(연도, 학생수계) |>
  timetk::plot_acf_diagnostics(
    .date_var = 연도,
    .value = 학생수계,
    .lag = 14,
    .show_white_noise_bars = TRUE
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
students |>
  select(학생수계) |>
  stats::pacf()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
students |>
  select(학생수계) |>
  stats::pacf(plot = FALSE)
```

    ## 
    ## Partial autocorrelations of series 'select(students, 학생수계)', by lag
    ## 
    ##      1      2      3      4      5      6      7      8      9     10     11     12     13 
    ##  0.875 -0.091 -0.069 -0.090 -0.104 -0.092 -0.105 -0.091 -0.089 -0.058 -0.033 -0.010 -0.001

``` r
students |>
  select(학생수계) |>
  forecast::Pacf()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
students |>
  select(학생수계) |>
  forecast::Pacf(plot = FALSE)
```

    ## 
    ## Partial autocorrelations of series 'select(students, 학생수계)', by lag
    ## 
    ##      1      2      3      4      5      6      7      8      9     10     11     12     13 
    ##  0.875 -0.091 -0.069 -0.090 -0.104 -0.092 -0.105 -0.091 -0.089 -0.058 -0.033 -0.010 -0.001

``` r
students |>
  select(학생수계) |>
  forecast::ggPacf()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
students |>
  select(학생수계) |>
  forecast::ggPacf(plot = FALSE)
```

    ## 
    ## Partial autocorrelations of series 'select(students, 학생수계)', by lag
    ## 
    ##      1      2      3      4      5      6      7      8      9     10     11     12     13 
    ##  0.875 -0.091 -0.069 -0.090 -0.104 -0.092 -0.105 -0.091 -0.089 -0.058 -0.033 -0.010 -0.001

## 5.4 적합값과 잔차

``` r
students.ts.lm <- forecast::tslm(students.ts[, 2] ~ trend, data = students.ts)
students.ts.lm
```

    ## 
    ## Call:
    ## forecast::tslm(formula = students.ts[, 2] ~ trend, data = students.ts)
    ## 
    ## Coefficients:
    ## (Intercept)        trend  
    ##     9118484      -132164

``` r
fitted(students.ts.lm)
```

    ## Time Series:
    ## Start = 1999 
    ## End = 2020 
    ## Frequency = 1 
    ##       1       2       3       4       5       6       7       8       9      10      11      12      13 
    ## 8986320 8854157 8721993 8589830 8457666 8325503 8193339 8061176 7929012 7796849 7664685 7532522 7400358 
    ##      14      15      16      17      18      19      20      21      22 
    ## 7268195 7136031 7003868 6871704 6739541 6607377 6475213 6343050 6210886

``` r
residuals(students.ts.lm)
```

    ## Time Series:
    ## Start = 1999 
    ## End = 2020 
    ## Frequency = 1 
    ##          1          2          3          4          5          6          7          8          9         10 
    ## -327962.15 -318289.65 -307570.14 -227896.63  -77891.12   46127.39  178081.89  293715.40  380919.91  390933.42 
    ##         11         12         13         14         15         16         17         18         19         20 
    ##  352238.93  275141.44  185907.94  102113.45   37872.96  -30713.53  -65293.02 -117993.51 -153096.01 -179847.50 
    ##         21         22 
    ## -220851.99 -215647.48

``` r
plot(students.ts[, 2])
lines(fitted(students.ts.lm), col = "red")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

## 5.5 백색잡음

``` r
library(forecast)
data(goog200, package = "fpp2")
checkresiduals(goog200)
```

    ## Warning in modeldf.default(object): Could not find appropriate degrees of freedom for this model.

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

## 5.6 시계열 분해

``` r
try(students.ts[, 2] |> decompose())
```

    ## Error in decompose(students.ts[, 2]) : 
    ##   time series has no or less than 2 periods

``` r
employees.ts[, 2] |>
  stl(s.window = "periodic") |>
  autoplot()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

## 5.7 정상성 테스트

``` r
library(urca)
employees.ts[, 2] |>
  ur.kpss() |>
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 3 lags. 
    ## 
    ## Value of test-statistic is: 1.9226 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
forecast::nsdiffs(employees.ts[, 2], alpha = 0.05)
```

    ## [1] 1

``` r
diff(employees.ts[, 2]) |>
  ur.kpss() |>
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 3 lags. 
    ## 
    ## Value of test-statistic is: 0.1348 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

## 5.8 계절성 검정

``` r
# install.packages("seastests")
library(seastests)
```

``` r
try(students.ts[, 2] |> combined_test() |> summary())
```

    ## Error in h(simpleError(msg, call)) : 
    ##   error in evaluating the argument 'object' in selecting a method for function 'summary': The number of observations per cycle (usually years) is 1 and thus too small.

``` r
employees.ts[, 2] |>
  combined_test() |>
  summary()
```

    ## Test used:  WO 
    ##  
    ## Test statistic:  1 
    ## P-value:  0 1.199041e-14 2.346908e-06 
    ##  
    ## The WO - test identifies seasonality

``` r
forecast::nsdiffs(employees.ts[, 2])
```

    ## [1] 1

``` r
employees.ts[, 3] |>
  combined_test() |>
  summary()
```

    ## Test used:  WO 
    ##  
    ## Test statistic:  0 
    ## P-value:  0.1976637 0.1976739 0.006082959 
    ##  
    ## The WO - test does not identify  seasonality

``` r
library(forecast)
employees.ts[, 2] |>
  decompose() |>
  seasadj() |>
  autoplot()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
ggseasonplot(employees.ts[, 2],
  main = "연도별 월간 플롯",
  ylab = "취업자수",
  xlab = "월",
  year.labels = TRUE
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
ggsubseriesplot(employees.ts[, 2],
  main = "월별 연간 플롯",
  ylab = "취업자수",
  xlab = "월"
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note05_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->
