note07
================

``` r
# install.packages("fable")
# install.packages("fable.prophet")
# install.packages("feasts")
```

``` r
load("./data-tidied/dataset.rdata")
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
```

    ## Loading required package: fabletools

    ## 
    ## Attaching package: 'fabletools'

    ## The following objects are masked from 'package:forecast':
    ## 
    ##     accuracy, forecast

``` r
library(fable)
library(fable.prophet)
```

    ## Loading required package: Rcpp

``` r
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 7 시계열 forecasting Part II - 시계열 분석 프레임워크

## 7.1 성능 분석 지수

## 7.2 fable 프레임워크

``` r
students.tsibble |>
  select(연도, 학생수계) |>
  mutate(year = yearmonth(연도)) |>
  as_tsibble(index = year) ->
students.selected

split <- floor(nrow(students.selected) * 0.8)
students.tsibble.tr <- students.selected[1:split, ]
students.tsibble.test <- students.selected[(split + 1):nrow(students.selected), ]
```

``` r
model.fable.students <- model(
  students.tsibble.tr,
  ets = ETS(학생수계),
  # arima = ARIMA(학생수계),
  naive = NAIVE(학생수계),
  tslm = TSLM(학생수계 ~ trend()),
  rw = RW(학생수계),
  mean = MEAN(학생수계),
  nnetar = NNETAR(학생수계),
  prophet = fable.prophet::prophet(학생수계)
)
```

    ## n.changepoints greater than number of observations. Using 12

``` r
forecast.fable.students <- forecast(model.fable.students, h = 10)
```

``` r
autoplot(forecast.fable.students, students.selected, level = NULL)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
accuracy(forecast.fable.students, students.tsibble.test) |>
  arrange(RMSE)
```

    ## Warning: The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
    ## 5 observations are missing between 2021 Jan and 2025 Jan

    ## # A tibble: 7 × 10
    ##   .model  .type        ME     RMSE      MAE      MPE   MAPE  MASE RMSSE    ACF1
    ##   <chr>   <chr>     <dbl>    <dbl>    <dbl>    <dbl>  <dbl> <dbl> <dbl>   <dbl>
    ## 1 ets     Test     -5862.   17477.   16449.  -0.0862  0.263   NaN   NaN -0.0726
    ## 2 prophet Test     96288.  116209.   96288.   1.57    1.57    NaN   NaN  0.276 
    ## 3 nnetar  Test   -449965.  499606.  449965.  -7.28    7.28    NaN   NaN  0.417 
    ## 4 tslm    Test   -499968.  505229.  499968.  -7.99    7.99    NaN   NaN  0.427 
    ## 5 naive   Test   -508685.  555965.  508685.  -8.21    8.21    NaN   NaN  0.413 
    ## 6 rw      Test   -508685.  555965.  508685.  -8.21    8.21    NaN   NaN  0.413 
    ## 7 mean    Test  -1683488. 1698372. 1683488. -26.9    26.9     NaN   NaN  0.413

``` r
best.model.fable.students <-
  model.fable.students |>
  select(ets, prophet)
```

``` r
best.model.fable.students |>
  forecast(h = 10) |>
  autoplot(students.selected, alpha = 0.6, level = NULL) +
  autolayer(fitted(best.model.fable.students))
```

    ## Plot variable not specified, automatically selected `.vars = .fitted`

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
employees$yearmonth <- yearmonth(employees$time)
employees.tsibble <- as_tsibble(employees, index = yearmonth)
```

``` r
split <- floor(nrow(employees.tsibble) * 0.9)
n <- nrow(employees.tsibble)
employees.tsibble.tr <- employees.tsibble[1:split, ]
employees.tsibble.test <- employees.tsibble[(split + 1):n, ]
```

``` r
model.fable.employees <-
  employees.tsibble.tr |>
  model(
    ets = ETS(total),
    arima = ARIMA(total),
    naive = NAIVE(total),
    tslm = TSLM(total ~ trend() + season(12)),
    rw = RW(total),
    mean = MEAN(total),
    nnetar = NNETAR(total),
    propeht = prophet(total)
  )
```

``` r
forecast.fable.employees <-
  model.fable.employees |>
  forecast(h = 24)
```

``` r
forecast.fable.employees |>
  autoplot(employees.tsibble, level = NULL) +
  labs(
    title = "fable로 생성한 8가지 모델 예측 플롯",
    x = "연월", y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
forecast.fable.employees |>
  accuracy(employees.tsibble.test) |>
  arrange(RMSE)
```

    ## Warning: The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
    ## 14 observations are missing between 2021 Jan and 2022 Feb

    ## # A tibble: 8 × 10
    ##   .model  .type      ME  RMSE   MAE    MPE  MAPE  MASE RMSSE    ACF1
    ##   <chr>   <chr>   <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 naive   Test     83.4  259.  247.  0.301 0.917   NaN   NaN  0.203 
    ## 2 rw      Test     83.4  259.  247.  0.301 0.917   NaN   NaN  0.203 
    ## 3 mean    Test    560.   611.  560.  2.07  2.07    NaN   NaN  0.203 
    ## 4 tslm    Test   -728.   738.  728. -2.70  2.70    NaN   NaN -0.366 
    ## 5 ets     Test   -730.   740.  730. -2.71  2.71    NaN   NaN -0.0628
    ## 6 propeht Test   -827.   839.  827. -3.07  3.07    NaN   NaN -0.397 
    ## 7 arima   Test   -833.   840.  833. -3.10  3.10    NaN   NaN -0.289 
    ## 8 nnetar  Test  -1033.  1190. 1033. -3.84  3.84    NaN   NaN  0.596

``` r
best.model.fable.employees <-
  model.fable.employees |>
  select(naive, rw)
```

``` r
best.model.fable.employees |>
  forecast(h = 12) |>
  autoplot(employees.tsibble, level = NULL, lwd = 1) +
  autolayer(fitted(best.model.fable.employees), lwd = 1) +
  geom_point(aes(yearmonth, total)) +
  labs(title = "전체 취업자수 예측 모델", x = "연월", y = "취업자수")
```

    ## Plot variable not specified, automatically selected `.vars = .fitted`

    ## Warning: Removed 2 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
fill.covid19.tsibble <- fill_gaps(covid19.tsibble, `0-9세` = 0)
```

``` r
split <- floor(nrow(fill.covid19.tsibble) * 0.9)
n <- nrow(fill.covid19.tsibble)
fill.covid19.tsibble.tr <- fill.covid19.tsibble[1:split, ]
fill.covid19.tsibble.test <- fill.covid19.tsibble[(split + 1):n, ]
```

``` r
model.covid19.tsibble <-
  fill.covid19.tsibble.tr |>
  model(
    ets = ETS(`0-9세`),
    arima = ARIMA(`0-9세`),
    naive = NAIVE(`0-9세`),
    tslm = TSLM(`0-9세`),
    rw = RW(`0-9세`),
    mean = MEAN(`0-9세`),
    nnetar = NNETAR(`0-9세`),
    prophet = prophet(`0-9세`)
  )
```

``` r
forecast.covid19.tsibble <-
  model.covid19.tsibble |>
  forecast(h = 120, time = 100) # reduce time for NNETAR
```

``` r
forecast.covid19.tsibble |>
  autoplot(fill.covid19.tsibble, level = NULL) +
  labs(
    title = "코로나 확진자(0-9세)에 대한 8가지 모델 예측 결과",
    x = "날짜", y = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
forecast.covid19.tsibble |>
  accuracy(fill.covid19.tsibble.test) |>
  arrange(RMSE)
```

    ## Warning: The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
    ## 89 observations are missing between 2021-02-10 and 2021-05-09

    ## # A tibble: 8 × 10
    ##   .model  .type     ME  RMSE   MAE    MPE  MAPE  MASE RMSSE  ACF1
    ##   <chr>   <chr>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 prophet Test   -6.15  10.4  8.82  -38.2  46.1   NaN   NaN 0.362
    ## 2 naive   Test   -8.39  11.4 10     -51.7  55.8   NaN   NaN 0.234
    ## 3 rw      Test   -8.39  11.4 10     -51.7  55.8   NaN   NaN 0.234
    ## 4 ets     Test  -12.4   14.6 13.1   -70.9  72.4   NaN   NaN 0.234
    ## 5 mean    Test   15.6   17.4 15.6    61.8  61.8   NaN   NaN 0.234
    ## 6 tslm    Test   15.6   17.4 15.6    61.8  61.8   NaN   NaN 0.234
    ## 7 arima   Test  -16.8   18.7 16.9   -92.1  92.5   NaN   NaN 0.281
    ## 8 nnetar  Test  -22.4   24.6 22.4  -119.  119.    NaN   NaN 0.535

``` r
best.model.covid19.tsibble <-
  model.covid19.tsibble |>
  select(prophet)
```

``` r
best.model.covid19.tsibble |>
  forecast(h = 120) |>
  autoplot(fill.covid19.tsibble, lwd = 1, alpha = 0.6) +
  autolayer(fitted(best.model.covid19.tsibble), lwd = 1) +
  geom_point(aes(date, `0-9세`)) +
  labs(
    title = "코로나 확진자수(0-9세) 예측",
    x = "연월일", y = "확진자수"
  )
```

    ## Plot variable not specified, automatically selected `.vars = .fitted`

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

## 7.3 modeltime 프레임워크

``` r
# install.packages("modeltime")
library(modeltime)
library(tidymodels)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────── tidymodels 1.0.0 ──

    ## ✔ broom        1.0.1          ✔ rsample      1.1.0     
    ## ✔ dials        1.0.0          ✔ tibble       3.1.8     
    ## ✔ infer        1.0.3          ✔ tune         1.0.0.9000
    ## ✔ modeldata    1.0.1          ✔ workflows    1.0.0     
    ## ✔ parsnip      1.0.1          ✔ workflowsets 1.0.0     
    ## ✔ purrr        0.3.4          ✔ yardstick    1.1.0     
    ## ✔ recipes      1.0.1

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ yardstick::accuracy() masks fabletools::accuracy(), forecast::accuracy()
    ## ✖ purrr::discard()      masks scales::discard()
    ## ✖ tibbletime::filter()  masks dplyr::filter(), stats::filter()
    ## ✖ xts::first()          masks dplyr::first()
    ## ✖ infer::generate()     masks fabletools::generate()
    ## ✖ infer::hypothesize()  masks fabletools::hypothesize()
    ## ✖ dplyr::lag()          masks stats::lag()
    ## ✖ xts::last()           masks dplyr::last()
    ## ✖ parsnip::null_model() masks fabletools::null_model()
    ## ✖ rsample::populate()   masks Rcpp::populate()
    ## ✖ recipes::step()       masks stats::step()
    ## • Use tidymodels_prefer() to resolve common conflicts.

``` r
splits.students <- initial_time_split(students, prop = 0.8)
splits.students
```

    ## <Training/Testing/Total>
    ## <17/5/22>

``` r
model_fit_arima <-
  arima_reg() |>
  set_engine("auto_arima") |>
  fit(학생수계 ~ 연도, data = training(splits.students))
```

    ## frequency = 5 observations per 5 years

``` r
model_fit_ets <-
  exp_smoothing() |>
  set_engine("ets") |>
  fit(학생수계 ~ 연도, data = training(splits.students))
```

    ## frequency = 5 observations per 5 years

``` r
model_fit_prophet <-
  prophet_reg() |>
  set_engine("prophet") |>
  fit(학생수계 ~ 연도, data = training(splits.students))
```

    ## Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

    ## n.changepoints greater than number of observations. Using 12

``` r
model_fit_lm <- linear_reg() |>
  set_engine("lm") |>
  fit(학생수계 ~ 연도, data = training(splits.students))
```

``` r
model_fit_nnetar <- nnetar_reg() |>
  set_engine("nnetar") |>
  fit(학생수계 ~ 연도, data = training(splits.students))
```

    ## frequency = 5 observations per 5 years

``` r
model_fit_tbats <- seasonal_reg() |>
  set_engine("tbats") |>
  fit(학생수계 ~ 연도, data = training(splits.students))
```

    ## frequency = 5 observations per 5 years

``` r
models_tbl <-
  modeltime_table(
    model_fit_arima,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm,
    model_fit_nnetar,
    model_fit_tbats
  )
models_tbl
```

    ## # Modeltime Table
    ## # A tibble: 6 × 3
    ##   .model_id .model   .model_desc             
    ##       <int> <list>   <chr>                   
    ## 1         1 <fit[+]> ARIMA(1,2,0)            
    ## 2         2 <fit[+]> ETS(A,A,N)              
    ## 3         3 <fit[+]> PROPHET                 
    ## 4         4 <fit[+]> LM                      
    ## 5         5 <fit[+]> NNAR(1,1,10)[5]         
    ## 6         6 <fit[+]> BATS(0.732, {0,0}, 1, -)

``` r
calibration_tbl <-
  models_tbl |>
  modeltime_calibrate(new_data = testing(splits.students))
calibration_tbl
```

    ## # Modeltime Table
    ## # A tibble: 6 × 5
    ##   .model_id .model   .model_desc              .type .calibration_data
    ##       <int> <list>   <chr>                    <chr> <list>           
    ## 1         1 <fit[+]> ARIMA(1,2,0)             Test  <tibble [5 × 4]> 
    ## 2         2 <fit[+]> ETS(A,A,N)               Test  <tibble [5 × 4]> 
    ## 3         3 <fit[+]> PROPHET                  Test  <tibble [5 × 4]> 
    ## 4         4 <fit[+]> LM                       Test  <tibble [5 × 4]> 
    ## 5         5 <fit[+]> NNAR(1,1,10)[5]          Test  <tibble [5 × 4]> 
    ## 6         6 <fit[+]> BATS(0.732, {0,0}, 1, -) Test  <tibble [5 × 4]>

``` r
calibration_tbl |>
  modeltime_forecast(
    new_data = testing(splits.students),
    actual_data = students
  ) |>
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) +
  labs(
    title = "modeltime을 사용한 전체 학생수 6개 모델 예측 결과",
    x = "연도",
    y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
calibration_tbl |>
  modeltime_accuracy() |>
  arrange(rmse)
```

    ## # A tibble: 6 × 9
    ##   .model_id .model_desc              .type     mae  mape  mase smape    rmse   rsq
    ##       <int> <chr>                    <chr>   <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>
    ## 1         2 ETS(A,A,N)               Test   16449. 0.263 0.105 0.263  17477. 0.998
    ## 2         6 BATS(0.732, {0,0}, 1, -) Test   18767. 0.302 0.120 0.301  21983. 0.998
    ## 3         1 ARIMA(1,2,0)             Test   79663. 1.28  0.509 1.27   85226. 0.998
    ## 4         3 PROPHET                  Test  101824. 1.65  0.650 1.67  115023. 0.996
    ## 5         5 NNAR(1,1,10)[5]          Test  255194. 4.14  1.63  4.03  293077. 0.963
    ## 6         4 LM                       Test  500003. 7.99  3.19  7.67  505266. 0.998

``` r
model_fit_ets2 <-
  exp_smoothing() |>
  set_engine("ets") |>
  fit(학생수계 ~ 연도, data = students)
```

    ## frequency = 5 observations per 5 years

``` r
model_fit_tbats2 <-
  seasonal_reg() |>
  set_engine("tbats") |>
  fit(학생수계 ~ 연도, data = students)
```

    ## frequency = 5 observations per 5 years

``` r
best_models_tbl <-
  modeltime_table(
    model_fit_ets2,
    model_fit_tbats2
  )
```

``` r
best_models_tbl |>
  modeltime_forecast(h = "10 years", actual_data = students) |>
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
splits.employees <- initial_time_split(employees, prop = 0.9)
splits.employees
```

    ## <Training/Testing/Total>
    ## <86/10/96>

``` r
model_fit_arima <- arima_reg() |>
  set_engine("auto_arima") |>
  fit(total ~ time, data = training(splits.employees))
```

    ## frequency = 12 observations per 1 year

``` r
model_fit_ets <- exp_smoothing() |>
  set_engine("ets") |>
  fit(total ~ time, data = training(splits.employees))
```

    ## frequency = 12 observations per 1 year

``` r
model_fit_prophet <- prophet_reg() |>
  set_engine(engine = "prophet") |>
  fit(total ~ time, data = training(splits.employees))
```

    ## Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
model_fit_lm <- linear_reg() |>
  set_engine("lm") |>
  fit(total ~ time + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
    data = training(splits.employees)
  )
```

``` r
model_fit_nnetar <- nnetar_reg() |>
  set_engine("nnetar") |>
  fit(total ~ time, data = training(splits.employees))
```

    ## frequency = 12 observations per 1 year

``` r
model_fit_tbats <- seasonal_reg() |>
  set_engine("tbats") |>
  fit(total ~ time, data = training(splits.employees))
```

    ## frequency = 12 observations per 1 year

``` r
models_tbl <-
  modeltime_table(
    model_fit_arima,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm,
    model_fit_nnetar,
    model_fit_tbats
  )
models_tbl
```

    ## # Modeltime Table
    ## # A tibble: 6 × 3
    ##   .model_id .model   .model_desc                     
    ##       <int> <list>   <chr>                           
    ## 1         1 <fit[+]> ARIMA(0,1,1)(0,1,1)[12]         
    ## 2         2 <fit[+]> ETS(A,AD,A)                     
    ## 3         3 <fit[+]> PROPHET                         
    ## 4         4 <fit[+]> LM                              
    ## 5         5 <fit[+]> NNAR(1,1,10)[12]                
    ## 6         6 <fit[+]> TBATS(1, {0,0}, 0.971, {<12,3>})

``` r
calibration_tbl <- models_tbl |>
  modeltime_calibrate(new_data = testing(splits.employees))
calibration_tbl
```

    ## # Modeltime Table
    ## # A tibble: 6 × 5
    ##   .model_id .model   .model_desc                      .type .calibration_data
    ##       <int> <list>   <chr>                            <chr> <list>           
    ## 1         1 <fit[+]> ARIMA(0,1,1)(0,1,1)[12]          Test  <tibble [10 × 4]>
    ## 2         2 <fit[+]> ETS(A,AD,A)                      Test  <tibble [10 × 4]>
    ## 3         3 <fit[+]> PROPHET                          Test  <tibble [10 × 4]>
    ## 4         4 <fit[+]> LM                               Test  <tibble [10 × 4]>
    ## 5         5 <fit[+]> NNAR(1,1,10)[12]                 Test  <tibble [10 × 4]>
    ## 6         6 <fit[+]> TBATS(1, {0,0}, 0.971, {<12,3>}) Test  <tibble [10 × 4]>

``` r
calibration_tbl |>
  modeltime_forecast(
    new_data = testing(splits.employees),
    actual_data = employees
  ) |>
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) +
  labs(
    title = "modeltime을 사용한 신규 취업자수 6개 모델 예측 결과",
    x = "연도", y = "취업자수"
  ) +
  theme(text = element_text(family = "NanumGothic"))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
calibration_tbl |>
  modeltime_accuracy() |>
  arrange(rmse)
```

    ## # A tibble: 6 × 9
    ##   .model_id .model_desc                      .type   mae  mape  mase smape  rmse   rsq
    ##       <int> <chr>                            <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1         5 NNAR(1,1,10)[12]                 Test   589.  2.19  3.25  2.16  662. 0.222
    ## 2         4 LM                               Test   728.  2.71  4.02  2.67  738. 0.762
    ## 3         2 ETS(A,AD,A)                      Test   730.  2.71  4.03  2.68  740. 0.757
    ## 4         3 PROPHET                          Test   824.  3.06  4.55  3.02  833. 0.760
    ## 5         1 ARIMA(0,1,1)(0,1,1)[12]          Test   833.  3.10  4.60  3.05  840. 0.795
    ## 6         6 TBATS(1, {0,0}, 0.971, {<12,3>}) Test   903.  3.36  4.99  3.30  913. 0.724

``` r
model_fit_lm3 <- linear_reg() |>
  set_engine("lm") |>
  fit(total ~ time + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
    data = employees
  )

model_fit_nnetar3 <- nnetar_reg() |>
  set_engine("nnetar") |>
  fit(total ~ time, data = employees)
```

    ## frequency = 12 observations per 1 year

``` r
best_models_tbl <-
  modeltime_table(
    model_fit_lm3,
    model_fit_nnetar3
  )
```

``` r
best_models_tbl |>
  modeltime_forecast(
    h = "3 years",
    actual_data = employees
  ) |>
  plot_modeltime_forecast(
    .interactive = FALSE
  )
```

    ## Warning: Expecting the following names to be in the data frame: .conf_hi, .conf_lo. 
    ## Proceeding with '.conf_interval_show = FALSE' to visualize the forecast without confidence intervals.
    ## Alternatively, try using `modeltime_calibrate()` before forecasting to add confidence intervals.

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
splits.covid19 <- initial_time_split(covid19, prop = 0.9)
splits.covid19
```

    ## <Training/Testing/Total>
    ## <271/31/302>

``` r
model_fit_arima <- arima_reg() |>
  set_engine("auto_arima")

model_fit_ets <- exp_smoothing() |>
  set_engine("ets")

model_fit_prophet <- prophet_reg() |>
  set_engine("prophet")

model_fit_lm <- linear_reg() |>
  set_engine("lm")

model_fit_nnetar <- nnetar_reg() |>
  set_engine("nnetar")

model_fit_tbats <- seasonal_reg() |>
  set_engine("tbats")

model_list <-
  list(
    model_fit_arima,
    model_fit_prophet,
    model_fit_ets,
    model_fit_lm,
    model_fit_nnetar,
    model_fit_tbats
  )

fitted_model_list <- lapply(model_list, function(m) fit(m, `0-9세` ~ date, data = training(splits.covid19)))
```

    ## frequency = 7 observations per 1 week

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

    ## frequency = 7 observations per 1 week
    ## frequency = 7 observations per 1 week
    ## frequency = 7 observations per 1 week

``` r
models_tbl <- do.call(modeltime_table, fitted_model_list)
models_tbl
```

    ## # Modeltime Table
    ## # A tibble: 6 × 3
    ##   .model_id .model   .model_desc         
    ##       <int> <list>   <chr>               
    ## 1         1 <fit[+]> ARIMA(3,1,1)        
    ## 2         2 <fit[+]> PROPHET             
    ## 3         3 <fit[+]> ETS(A,N,N)          
    ## 4         4 <fit[+]> LM                  
    ## 5         5 <fit[+]> NNAR(1,1,10)[7]     
    ## 6         6 <fit[+]> BATS(1, {2,2}, -, -)

``` r
calibration_tbl <- models_tbl |>
  modeltime_calibrate(new_data = testing(splits.covid19))
```

``` r
calibration_tbl |>
  modeltime_forecast(
    new_data = testing(splits.covid19),
    actual_data = covid19
  ) |>
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) +
  labs(title = "modeltime을 사용한 코로나 확진자수") +
  theme(text = element_text(family = "NanumGothic"))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
calibration_tbl |>
  modeltime_accuracy() |>
  arrange(rmse)
```

    ## Warning: A correlation computation is required, but `estimate` is constant and has 0 standard deviation,
    ## resulting in a divide by 0 error. `NA` will be returned.

    ## # A tibble: 6 × 9
    ##   .model_id .model_desc          .type   mae  mape  mase smape  rmse     rsq
    ##       <int> <chr>                <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1         4 LM                   Test   6.81  34.8 0.909  28.9  8.25  0.0947
    ## 2         3 ETS(A,N,N)           Test  13.8   76.2 1.84   49.1 15.4  NA     
    ## 3         1 ARIMA(3,1,1)         Test  13.9   76.8 1.86   49.4 15.5   0.0227
    ## 4         6 BATS(1, {2,2}, -, -) Test  13.9   76.9 1.86   49.4 15.5   0.0365
    ## 5         5 NNAR(1,1,10)[7]      Test  16.8   91.5 2.24   56.1 18.4   0.129 
    ## 6         2 PROPHET              Test  27.2  143.  3.63   75.3 29.1   0.0781

``` r
best_models <- model_list[c(4, 3)]
best_fitted_model_list <- lapply(best_models, function(m) {
  fit(m, `0-9세` ~ date,
    data = covid19
  )
})
```

    ## frequency = 7 observations per 1 week

``` r
best_fitted_model <- do.call(modeltime_table, best_fitted_model_list)
best_fitted_model |>
  modeltime_forecast(
    h = "3 months",
    actual_data = covid19
  ) |>
  plot_modeltime_forecast(
    .interactive = FALSE
  ) +
  labs(
    title = "코로나 확진자수(0-9세) 모델 예측 결과",
    x = "날짜", y = "확진자수"
  ) +
  theme(text = element_text(family = "NanumGothic"))
```

    ## Warning: Expecting the following names to be in the data frame: .conf_hi, .conf_lo. 
    ## Proceeding with '.conf_interval_show = FALSE' to visualize the forecast without confidence intervals.
    ## Alternatively, try using `modeltime_calibrate()` before forecasting to add confidence intervals.

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note07_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->
