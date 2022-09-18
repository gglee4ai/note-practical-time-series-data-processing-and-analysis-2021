note06
================

``` r
load("./data-tidied/dataset.rdata")
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
library(tidyr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 6 시계열 forecasting Part II - 시계열 예측 모델

## 6.1 평균 모델

``` r
library(forecast)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
stu1 <-
  students.ts[, 2] |>
  meanf() |>
  summary()
stu1
```

    ## 
    ## Forecast method: Mean
    ## 
    ## Model Information:
    ## $mu
    ## [1] 7598603
    ## 
    ## $mu.se
    ## [1] 189914
    ## 
    ## $sd
    ## [1] 890775.4
    ## 
    ## $bootstrap
    ## [1] FALSE
    ## 
    ## $call
    ## meanf(y = students.ts[, 2])
    ## 
    ## attr(,"class")
    ## [1] "meanf"
    ## 
    ## Error measures:
    ##                         ME     RMSE    MAE       MPE    MAPE     MASE      ACF1
    ## Training set -2.328306e-10 870295.1 780669 -1.422719 10.7441 6.074563 0.8753895
    ## 
    ## Forecasts:
    ##      Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
    ## 2021        7598603 6393450 8803757 5704501 9492706
    ## 2022        7598603 6393450 8803757 5704501 9492706
    ## 2023        7598603 6393450 8803757 5704501 9492706
    ## 2024        7598603 6393450 8803757 5704501 9492706
    ## 2025        7598603 6393450 8803757 5704501 9492706
    ## 2026        7598603 6393450 8803757 5704501 9492706
    ## 2027        7598603 6393450 8803757 5704501 9492706
    ## 2028        7598603 6393450 8803757 5704501 9492706
    ## 2029        7598603 6393450 8803757 5704501 9492706
    ## 2030        7598603 6393450 8803757 5704501 9492706

``` r
stu1 |>
  autoplot(
    main = "학생수 평균 모델 플롯",
    xlab = "연도",
    ylab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
students.ts[, 2] |>
  meanf(bootstrap = TRUE) |>
  autoplot(
    main = "학생수 평균 모델 플롯(부트스트랩)",
    xlab = "연도",
    ylab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  meanf() |>
  autoplot(
    main = "신규 취업자수 평균 모델 플롯",
    xlab = "연도",
    ylab = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
covid19.ts[, 2] |>
  meanf(bootstrap = TRUE) |>
  autoplot(
    main = "코로나 확진자(0-9세) 평균 모델 플롯(부트스트랩)",
    xlab = "기간",
    ylab = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## 6.2 단순 모델

``` r
students.ts[, 2] |>
  naive() |>
  summary()
```

    ## 
    ## Forecast method: Naive method
    ## 
    ## Model Information:
    ## Call: naive(y = students.ts[, 2]) 
    ## 
    ## Residual sd: 147831.235 
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE MASE      ACF1
    ## Training set -126815.2 147831.2 128514.4 -1.771615 1.791893    1 0.8922526
    ## 
    ## Forecasts:
    ##      Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
    ## 2021        5995239 5805786 6184692 5705495 6284983
    ## 2022        5995239 5727312 6263166 5585479 6404999
    ## 2023        5995239 5667096 6323382 5493388 6497090
    ## 2024        5995239 5616332 6374146 5415751 6574727
    ## 2025        5995239 5571608 6418870 5347352 6643126
    ## 2026        5995239 5531175 6459303 5285514 6704964
    ## 2027        5995239 5493993 6496485 5228649 6761829
    ## 2028        5995239 5459384 6531094 5175720 6814758
    ## 2029        5995239 5426879 6563599 5126007 6864471
    ## 2030        5995239 5396135 6594343 5078988 6911490

``` r
students.ts[, 2] |>
  naive() |>
  autoplot(
    main = "전체 학생수 단순 모델 플롯",
    xlab = "연도",
    ylab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  naive() |>
  summary()
```

    ## 
    ## Forecast method: Naive method
    ## 
    ## Model Information:
    ## Call: naive(y = employees.ts[, 2]) 
    ## 
    ## Residual sd: 261.4803 
    ## 
    ## Error measures:
    ##                    ME     RMSE      MAE        MPE      MAPE      MASE      ACF1
    ## Training set 23.56842 261.4803 194.5579 0.08782727 0.7411892 0.6138395 0.4208691
    ## 
    ## Forecasts:
    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Jan 2021          26526 26190.90 26861.10 26013.51 27038.49
    ## Feb 2021          26526 26052.10 26999.90 25801.23 27250.77
    ## Mar 2021          26526 25945.59 27106.41 25638.34 27413.66
    ## Apr 2021          26526 25855.80 27196.20 25501.02 27550.98
    ## May 2021          26526 25776.69 27275.31 25380.03 27671.97
    ## Jun 2021          26526 25705.17 27346.83 25270.66 27781.34
    ## Jul 2021          26526 25639.41 27412.59 25170.07 27881.93
    ## Aug 2021          26526 25578.19 27473.81 25076.45 27975.55
    ## Sep 2021          26526 25520.70 27531.30 24988.52 28063.48
    ## Oct 2021          26526 25466.32 27585.68 24905.36 28146.64

``` r
employees.ts[, 2] |>
  naive() |>
  autoplot(
    main = "신규 취업자수 단순 모델 플롯",
    xlab = "연도",
    ylab = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
covid19.ts[, 2] |>
  naive() |>
  summary()
```

    ## 
    ## Forecast method: Naive method
    ## 
    ## Model Information:
    ## Call: naive(y = covid19.ts[, 2]) 
    ## 
    ## Residual sd: 5.9931 
    ## 
    ## Error measures:
    ##                      ME     RMSE      MAE MPE MAPE MASE       ACF1
    ## Training set 0.04318937 5.993075 3.465116 NaN  Inf  NaN -0.3127329
    ## 
    ## Forecasts:
    ##           Point Forecast      Lo 80    Hi 80      Lo 95    Hi 95
    ## 2020.8356             15  7.3195658 22.68043   3.253790 26.74621
    ## 2020.8384             15  4.1382258 25.86177  -1.611650 31.61165
    ## 2020.8411             15  1.6970978 28.30290  -5.345033 35.34503
    ## 2020.8438             15 -0.3608683 30.36087  -8.492421 38.49242
    ## 2020.8466             15 -2.1739729 32.17397 -11.265325 41.26533
    ## 2020.8493             15 -3.8131447 33.81314 -13.772222 43.77222
    ## 2020.8521             15 -5.3205188 35.32052 -16.077552 46.07755
    ## 2020.8548             15 -6.7235483 36.72355 -18.223300 48.22330
    ## 2020.8575             15 -8.0413025 38.04130 -20.238631 50.23863
    ## 2020.8603             15 -9.2876654 39.28767 -22.144779 52.14478

``` r
covid19.ts[, 2] |>
  naive() |>
  autoplot(
    main = "코로나19 확진자(0-9세)의 단순 모델 플롯",
    xlab = "기간",
    ylab = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## 6.3 계절성 단순 모델

``` r
students.ts[, 2] |>
  snaive(10) |>
  summary()
```

    ## 
    ## Forecast method: Seasonal naive method
    ## 
    ## Model Information:
    ## Call: snaive(y = students.ts[, 2], h = 10) 
    ## 
    ## Residual sd: 147831.235 
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE MASE      ACF1
    ## Training set -126815.2 147831.2 128514.4 -1.771615 1.791893    1 0.8922526
    ## 
    ## Forecasts:
    ##      Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
    ## 2021        5995239 5805786 6184692 5705495 6284983
    ## 2022        5995239 5727312 6263166 5585479 6404999
    ## 2023        5995239 5667096 6323382 5493388 6497090
    ## 2024        5995239 5616332 6374146 5415751 6574727
    ## 2025        5995239 5571608 6418870 5347352 6643126
    ## 2026        5995239 5531175 6459303 5285514 6704964
    ## 2027        5995239 5493993 6496485 5228649 6761829
    ## 2028        5995239 5459384 6531094 5175720 6814758
    ## 2029        5995239 5426879 6563599 5126007 6864471
    ## 2030        5995239 5396135 6594343 5078988 6911490

``` r
students.ts[, 2] |>
  snaive(10) |>
  autoplot(
    main = "전체 학생수 계절성 단순 모델 플롯",
    xlab = "연도",
    ylab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  snaive(10) |>
  summary()
```

    ## 
    ## Forecast method: Seasonal naive method
    ## 
    ## Model Information:
    ## Call: snaive(y = employees.ts[, 2], h = 10) 
    ## 
    ## Residual sd: 361.904 
    ## 
    ## Error measures:
    ##                    ME    RMSE      MAE       MPE    MAPE MASE      ACF1
    ## Training set 229.2857 361.904 316.9524 0.8720369 1.19817    1 0.8173272
    ## 
    ## Forecasts:
    ##          Point Forecast   Lo 80   Hi 80    Lo 95    Hi 95
    ## Jan 2021          26800 26336.2 27263.8 26090.68 27509.32
    ## Feb 2021          26838 26374.2 27301.8 26128.68 27547.32
    ## Mar 2021          26609 26145.2 27072.8 25899.68 27318.32
    ## Apr 2021          26562 26098.2 27025.8 25852.68 27271.32
    ## May 2021          26930 26466.2 27393.8 26220.68 27639.32
    ## Jun 2021          27055 26591.2 27518.8 26345.68 27764.32
    ## Jul 2021          27106 26642.2 27569.8 26396.68 27815.32
    ## Aug 2021          27085 26621.2 27548.8 26375.68 27794.32
    ## Sep 2021          27012 26548.2 27475.8 26302.68 27721.32
    ## Oct 2021          27088 26624.2 27551.8 26378.68 27797.32

``` r
employees.ts[, 2] |>
  snaive(10) |>
  autoplot(
    main = "신규 취업자수 계절성 단순 모델 플롯",
    xlab = "연도",
    ylab = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## 6.4 랜덤워크 모델

``` r
students.ts[, 2] |>
  rwf() |>
  summary()
```

    ## 
    ## Forecast method: Random walk
    ## 
    ## Model Information:
    ## Call: rwf(y = students.ts[, 2]) 
    ## 
    ## Residual sd: 147831.235 
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE MASE      ACF1
    ## Training set -126815.2 147831.2 128514.4 -1.771615 1.791893    1 0.8922526
    ## 
    ## Forecasts:
    ##      Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
    ## 2021        5995239 5805786 6184692 5705495 6284983
    ## 2022        5995239 5727312 6263166 5585479 6404999
    ## 2023        5995239 5667096 6323382 5493388 6497090
    ## 2024        5995239 5616332 6374146 5415751 6574727
    ## 2025        5995239 5571608 6418870 5347352 6643126
    ## 2026        5995239 5531175 6459303 5285514 6704964
    ## 2027        5995239 5493993 6496485 5228649 6761829
    ## 2028        5995239 5459384 6531094 5175720 6814758
    ## 2029        5995239 5426879 6563599 5126007 6864471
    ## 2030        5995239 5396135 6594343 5078988 6911490

``` r
students.ts[, 2] |>
  rwf() |>
  autoplot(
    main = "전체 학생수 련덤위크 모델 플롯",
    xlab = "연도",
    ylab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
students.ts[, 2] |>
  rwf(drift = TRUE) |>
  summary()
```

    ## 
    ## Forecast method: Random walk with drift
    ## 
    ## Model Information:
    ## Call: rwf(y = students.ts[, 2], drift = TRUE) 
    ## 
    ## Drift: -126815.1905  (se 16988.204)
    ## Residual sd: 77849.7308 
    ## 
    ## Error measures:
    ##                         ME     RMSE      MAE         MPE      MAPE      MASE      ACF1
    ## Training set -1.717186e-10 75973.56 63881.97 -0.06808499 0.8320398 0.4970802 0.8922526
    ## 
    ## Forecasts:
    ##      Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
    ## 2021        5868424 5768655 5968192 5715841 6021006
    ## 2022        5741609 5597194 5886023 5520746 5962471
    ## 2023        5614793 5433948 5795639 5338214 5891373
    ## 2024        5487978 5274664 5701292 5161743 5814214
    ## 2025        5361163 5117753 5604573 4988899 5733427
    ## 2026        5234348 4962425 5506271 4818478 5650218
    ## 2027        5107533 4808227 5406838 4649785 5565281
    ## 2028        4980717 4654875 5306560 4482385 5479050
    ## 2029        4853902 4502177 5205628 4315985 5391820
    ## 2030        4727087 4349998 5104176 4150379 5303795

``` r
students.ts[, 2] |>
  rwf(drift = TRUE) |>
  autoplot(
    main = "전체 학생수 련덤위크 모델 플롯",
    xlab = "연도",
    ylab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  rwf() |>
  summary()
```

    ## 
    ## Forecast method: Random walk
    ## 
    ## Model Information:
    ## Call: rwf(y = employees.ts[, 2]) 
    ## 
    ## Residual sd: 261.4803 
    ## 
    ## Error measures:
    ##                    ME     RMSE      MAE        MPE      MAPE      MASE      ACF1
    ## Training set 23.56842 261.4803 194.5579 0.08782727 0.7411892 0.6138395 0.4208691
    ## 
    ## Forecasts:
    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Jan 2021          26526 26190.90 26861.10 26013.51 27038.49
    ## Feb 2021          26526 26052.10 26999.90 25801.23 27250.77
    ## Mar 2021          26526 25945.59 27106.41 25638.34 27413.66
    ## Apr 2021          26526 25855.80 27196.20 25501.02 27550.98
    ## May 2021          26526 25776.69 27275.31 25380.03 27671.97
    ## Jun 2021          26526 25705.17 27346.83 25270.66 27781.34
    ## Jul 2021          26526 25639.41 27412.59 25170.07 27881.93
    ## Aug 2021          26526 25578.19 27473.81 25076.45 27975.55
    ## Sep 2021          26526 25520.70 27531.30 24988.52 28063.48
    ## Oct 2021          26526 25466.32 27585.68 24905.36 28146.64

``` r
employees.ts[, 2] |>
  rwf() |>
  autoplot(
    main = "신규 취업자수 랜덤워크 모델 플롯",
    xlab = "연도",
    ylab = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  rwf(drift = TRUE) |>
  summary()
```

    ## 
    ## Forecast method: Random walk with drift
    ## 
    ## Model Information:
    ## Call: rwf(y = employees.ts[, 2], drift = TRUE) 
    ## 
    ## Drift: 23.5684  (se 26.8599)
    ## Residual sd: 261.7975 
    ## 
    ## Error measures:
    ##                        ME    RMSE      MAE        MPE      MAPE      MASE      ACF1
    ## Training set 1.030362e-12 260.416 190.0696 -0.0013627 0.7245059 0.5996787 0.4208691
    ## 
    ## Forecasts:
    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Jan 2021       26549.57 26214.06 26885.08 26036.45 27062.68
    ## Feb 2021       26573.14 26096.17 27050.11 25843.68 27302.60
    ## Mar 2021       26596.71 26009.50 27183.91 25698.66 27494.75
    ## Apr 2021       26620.27 25938.75 27301.80 25577.97 27662.58
    ## May 2021       26643.84 25877.99 27409.69 25472.58 27815.10
    ## Jun 2021       26667.41 25824.24 27510.58 25377.89 27956.93
    ## Jul 2021       26690.98 25775.71 27606.25 25291.19 28090.76
    ## Aug 2021       26714.55 25731.25 27697.84 25210.72 28218.37
    ## Sep 2021       26738.12 25690.07 27786.16 25135.27 28340.96
    ## Oct 2021       26761.68 25651.60 27871.77 25063.95 28459.41

``` r
employees.ts[, 2] |>
  rwf(drift = TRUE) |>
  autoplot(
    main = "신규 취업자수 랜덤워크 모델 플롯",
    xlab = "연도",
    ylab = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
covid19.ts[, 2] |>
  rwf(30) |>
  summary()
```

    ## 
    ## Forecast method: Random walk
    ## 
    ## Model Information:
    ## Call: rwf(y = covid19.ts[, 2], h = 30) 
    ## 
    ## Residual sd: 5.9931 
    ## 
    ## Error measures:
    ##                      ME     RMSE      MAE MPE MAPE MASE       ACF1
    ## Training set 0.04318937 5.993075 3.465116 NaN  Inf  NaN -0.3127329
    ## 
    ## Forecasts:
    ##           Point Forecast       Lo 80    Hi 80      Lo 95    Hi 95
    ## 2020.8356             15   7.3195658 22.68043   3.253790 26.74621
    ## 2020.8384             15   4.1382258 25.86177  -1.611650 31.61165
    ## 2020.8411             15   1.6970978 28.30290  -5.345033 35.34503
    ## 2020.8438             15  -0.3608683 30.36087  -8.492421 38.49242
    ## 2020.8466             15  -2.1739729 32.17397 -11.265325 41.26533
    ## 2020.8493             15  -3.8131447 33.81314 -13.772222 43.77222
    ## 2020.8521             15  -5.3205188 35.32052 -16.077552 46.07755
    ## 2020.8548             15  -6.7235483 36.72355 -18.223300 48.22330
    ## 2020.8575             15  -8.0413025 38.04130 -20.238631 50.23863
    ## 2020.8603             15  -9.2876654 39.28767 -22.144779 52.14478
    ## 2020.8630             15 -10.4731184 40.47312 -23.957773 53.95777
    ## 2020.8658             15 -11.6058044 41.60580 -25.690067 55.69007
    ## 2020.8685             15 -12.6921992 42.69220 -27.351564 57.35156
    ## 2020.8712             15 -13.7375533 43.73755 -28.950295 58.95030
    ## 2020.8740             15 -14.7461936 44.74619 -30.492877 60.49288
    ## 2020.8767             15 -15.7217367 45.72174 -31.984842 61.98484
    ## 2020.8795             15 -16.6672414 46.66724 -33.430866 63.43087
    ## 2020.8822             15 -17.5853225 47.58532 -34.834950 64.83495
    ## 2020.8849             15 -18.4782364 48.47824 -36.200544 66.20054
    ## 2020.8877             15 -19.3479458 49.34795 -37.530650 67.53065
    ## 2020.8904             15 -20.1961710 50.19617 -38.827898 68.82790
    ## 2020.8932             15 -21.0244295 51.02443 -40.094611 70.09461
    ## 2020.8959             15 -21.8340683 51.83407 -41.332846 71.33285
    ## 2020.8986             15 -22.6262895 52.62629 -42.544444 72.54444
    ## 2020.9014             15 -23.4021709 53.40217 -43.731052 73.73105
    ## 2020.9041             15 -24.1626837 54.16268 -44.894156 74.89416
    ## 2020.9068             15 -24.9087066 54.90871 -46.035100 76.03510
    ## 2020.9096             15 -25.6410376 55.64104 -47.155103 77.15510
    ## 2020.9123             15 -26.3604038 56.36040 -48.255279 78.25528
    ## 2020.9151             15 -27.0674705 57.06747 -49.336644 79.33664

``` r
covid19.ts[, 2] |>
  rwf(30) |>
  autoplot(
    main = "코로나19 확진자(0-9세)의 랜덤워크 모델 플롯",
    xlab = "기간",
    ylab = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
covid19.ts[, 2] |>
  rwf(30, drift = TRUE) |>
  summary()
```

    ## 
    ## Forecast method: Random walk with drift
    ## 
    ## Model Information:
    ## Call: rwf(y = covid19.ts[, 2], h = 30, drift = TRUE) 
    ## 
    ## Drift: 0.0432  (se 0.346)
    ## Residual sd: 6.0029 
    ## 
    ## Error measures:
    ##                        ME     RMSE      MAE MPE MAPE MASE       ACF1
    ## Training set 2.124546e-16 5.992919 3.475017 NaN  Inf  NaN -0.3127329
    ## 
    ## Forecasts:
    ##           Point Forecast      Lo 80    Hi 80      Lo 95    Hi 95
    ## 2020.8356       15.04319   7.350165 22.73621   3.277724 26.80866
    ## 2020.8384       15.08638   4.188742 25.98402  -1.580119 31.75288
    ## 2020.8411       15.12957   1.760664 28.49847  -5.316406 35.57554
    ## 2020.8438       15.17276  -0.289776 30.63529  -8.475147 38.82066
    ## 2020.8466       15.21595  -2.100101 32.53200 -11.266664 41.69856
    ## 2020.8493       15.25914  -3.740715 34.25899 -13.798629 44.31690
    ## 2020.8521       15.30233  -5.253365 35.85802 -16.134890 46.73954
    ## 2020.8548       15.34551  -6.665203 37.35623 -18.316974 49.00800
    ## 2020.8575       15.38870  -7.995057 38.77247 -20.373672 51.15108
    ## 2020.8603       15.43189  -9.256607 40.12039 -22.325910 53.18970
    ## 2020.8630       15.47508 -10.460165 41.41033 -24.189456 55.13962
    ## 2020.8658       15.51827 -11.613726 42.65027 -25.976539 57.01308
    ## 2020.8685       15.56146 -12.723638 43.84656 -27.696865 58.81979
    ## 2020.8712       15.60465 -13.795036 45.00434 -29.358290 60.56759
    ## 2020.8740       15.64784 -14.832146 46.12783 -30.967276 62.26296
    ## 2020.8767       15.69103 -15.838493 47.22055 -32.529214 63.91127
    ## 2020.8795       15.73422 -16.817052 48.28549 -34.048655 65.51709
    ## 2020.8822       15.77741 -17.770363 49.32518 -35.529481 67.08430
    ## 2020.8849       15.82060 -18.700612 50.34181 -36.975037 68.61623
    ## 2020.8877       15.86379 -19.609697 51.33727 -38.388226 70.11580
    ## 2020.8904       15.90698 -20.499280 52.31323 -39.771588 71.58554
    ## 2020.8932       15.95017 -21.370823 53.27115 -41.127361 73.02769
    ## 2020.8959       15.99336 -22.225620 54.21233 -42.457524 74.44424
    ## 2020.8986       16.03654 -23.064827 55.13792 -43.763843 75.83693
    ## 2020.9014       16.07973 -23.889475 56.04894 -45.047896 77.20736
    ## 2020.9041       16.12292 -24.700492 56.94634 -46.311103 78.55695
    ## 2020.9068       16.16611 -25.498718 57.83094 -47.554747 79.88697
    ## 2020.9096       16.20930 -26.284912 58.70352 -48.779990 81.19859
    ## 2020.9123       16.25249 -27.059765 59.56475 -49.987889 82.49287
    ## 2020.9151       16.29568 -27.823909 60.41527 -51.179408 83.77077

``` r
covid19.ts[, 2] |>
  rwf(30, drift = TRUE) |>
  autoplot(
    main = "코로나19 확진자(0-9세)의 랜덤워크 모델 플롯",
    xlab = "기간",
    ylab = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
set.seed(345)
whitenoise <- ts(rnorm(100), start = 1)
ts.plot(whitenoise, ylab = "")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
whitenoise.to.randomwalk <- cumsum(whitenoise)
ts.plot(whitenoise.to.randomwalk, ylab = "")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
randomwalk.to.whitenoise <- diff(whitenoise.to.randomwalk)
ts.plot(randomwalk.to.whitenoise, ylab = "")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
autoplot(students.ts[, 2]) +
  autolayer(meanf(students.ts[, 2], h = 10), PI = FALSE, series = "평균") +
  autolayer(naive(students.ts[, 2], h = 10), PI = FALSE, series = "단순") +
  autolayer(snaive(students.ts[, 2], h = 10), PI = FALSE, series = "계절성 단순") +
  autolayer(rwf(students.ts[, 2], h = 10), PI = FALSE, series = "랜덤워크") +
  autolayer(rwf(students.ts[, 2], h = 10, drift = TRUE), PI = FALSE, series = "랜덤워크-드리프트") +
  labs(
    title = "전체 학생수의 평균, 단순, 계절성 단순, 랜덤워크 모델 예측값",
    x = "연도",
    y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
autoplot(employees.ts[, 2]) +
  autolayer(meanf(employees.ts[, 2], h = 10), PI = FALSE, series = "평균") +
  autolayer(naive(employees.ts[, 2], h = 10), PI = FALSE, series = "단순") +
  autolayer(snaive(employees.ts[, 2], h = 10), PI = FALSE, series = "계절성 단순") +
  autolayer(rwf(employees.ts[, 2], h = 10), PI = FALSE, series = "랜덤워크") +
  autolayer(rwf(employees.ts[, 2], h = 10, drift = TRUE), PI = FALSE, series = "랜덤워크-드리프트") +
  labs(
    title = "신규 취업자수의 평균, 단순, 계절성 단순, 랜덤워크 모델 예측값",
    x = "연도",
    y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
autoplot(covid19.ts[, 2]) +
  autolayer(meanf(covid19.ts[, 2], h = 30), PI = FALSE, series = "평균") +
  autolayer(naive(covid19.ts[, 2], h = 30), PI = FALSE, series = "단순") +
  # autolayer(snaive(covid19.ts[,2], h = 30), PI = FALSE, series = "계절성 단순") +
  autolayer(rwf(covid19.ts[, 2], h = 30), PI = FALSE, series = "랜덤워크") +
  autolayer(rwf(covid19.ts[, 2], h = 30, drift = TRUE), PI = FALSE, series = "랜덤워크-드리프트") +
  labs(
    title = "전체 학생수의 평균, 단순, 계절성 단순, 랜덤워크 모델 예측값",
    x = "연도",
    y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

## 6.5 회귀 모델

``` r
student.ts.lm <- tslm(students.ts[, 2] ~ trend, data = students.ts)
summary(student.ts.lm)
```

    ## 
    ## Call:
    ## tslm(formula = students.ts[, 2] ~ trend, data = students.ts)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -327962 -206697  -48003  183951  390933 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  9118484     107928   84.49  < 2e-16 ***
    ## trend        -132164       8218  -16.08 6.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 244500 on 20 degrees of freedom
    ## Multiple R-squared:  0.9282, Adjusted R-squared:  0.9246 
    ## F-statistic: 258.7 on 1 and 20 DF,  p-value: 6.608e-13

``` r
student.ts.lm |>
  forecast() |>
  autoplot() +
  labs(
    title = "전체 학생수에 대한 시계열 선형 회귀 예측 결과",
    x = "연도",
    y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
student.ts.lm <- tslm(students.ts[, 4] ~ students.ts[, 3] + trend, data = students.ts)
student.ts.lm |>
  forecast(h = 22) |>
  autoplot(
    main = "유치원 학생수와 추세를 활용한 초등학생수 시계열 선형 회귀 예측 결과",
    xlab = "연도",
    ylab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
try(student.ts.lm <- tslm(students.ts[, 2] ~ trend + season, data = students.ts))
```

    ## Error in tslm(students.ts[, 2] ~ trend + season, data = students.ts) : 
    ##   Non-seasonal data cannot be modelled using a seasonal factor

``` r
employee.total.ts.lm <- tslm(employees.ts[, 2] ~ trend, data = employees.ts)
summary(employee.total.ts.lm)
```

    ## 
    ## Call:
    ## tslm(formula = employees.ts[, 2] ~ trend, data = employees.ts)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1256.4  -264.1   119.9   350.6   525.7 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 25430.605     85.545  297.28   <2e-16 ***
    ## trend          20.394      1.531   13.32   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 415.8 on 94 degrees of freedom
    ## Multiple R-squared:  0.6536, Adjusted R-squared:  0.6499 
    ## F-statistic: 177.3 on 1 and 94 DF,  p-value: < 2.2e-16

``` r
employee.total.ts.lm |>
  forecast() |>
  autoplot() +
  labs(
    title = "신규 취업자수에 대한 시계열 선형 회귀 예측 결과",
    x = "시간", y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
employee.total.ts.lm <- tslm(employees.ts[, 2] ~ trend + season, data = employees.ts)
summary(employee.total.ts.lm)
```

    ## 
    ## Call:
    ## tslm(formula = employees.ts[, 2] ~ trend + season, data = employees.ts)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -675.70  -42.60   76.66  147.08  312.95 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 24850.616     96.043 258.744  < 2e-16 ***
    ## trend          19.457      0.919  21.171  < 2e-16 ***
    ## season2         1.168    123.798   0.009  0.99249    
    ## season3       318.462    123.808   2.572  0.01189 *  
    ## season4       629.005    123.825   5.080 2.29e-06 ***
    ## season5       869.174    123.849   7.018 5.63e-10 ***
    ## season6       919.342    123.880   7.421 9.13e-11 ***
    ## season7       935.260    123.918   7.547 5.14e-11 ***
    ## season8       803.429    123.962   6.481 6.13e-09 ***
    ## season9       854.597    124.013   6.891 9.94e-10 ***
    ## season10      890.516    124.071   7.177 2.75e-10 ***
    ## season11      898.809    124.135   7.241 2.07e-10 ***
    ## season12      385.477    124.207   3.104  0.00261 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 247.6 on 83 degrees of freedom
    ## Multiple R-squared:  0.8915, Adjusted R-squared:  0.8759 
    ## F-statistic: 56.86 on 12 and 83 DF,  p-value: < 2.2e-16

``` r
employee.total.ts.lm |>
  forecast() |>
  autoplot() +
  labs(
    title = "신규 취업자수에 대한 시계열 계절성 선형 회귀 예측 결과",
    x = "시간",
    y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
checkresiduals(tslm(students.ts[, 2] ~ trend, data = students.ts))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 5
    ## 
    ## data:  Residuals from Linear regression model
    ## LM test = 19.584, df = 5, p-value = 0.001496

``` r
library(timetk)
library(lubridate)
```

``` r
plot_time_series_regression(
  .data = students,
  .date_var = 연도,
  .formula = 학생수계 ~ 연도,
  .interactive = FALSE,
  .show_summary = TRUE
) +
  labs(
    title = "timetk를 사용한 전체 학생수 시계열 회귀 모델",
    x = "연도",
    y = "학생수"
  )
```

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -328001 -206778  -47994  183960  390806 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 12819016.6   328759.6   38.99  < 2e-16 ***
    ## 연도            -361.8       22.5  -16.08 6.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 244500 on 20 degrees of freedom
    ## Multiple R-squared:  0.9282, Adjusted R-squared:  0.9246 
    ## F-statistic: 258.7 on 1 and 20 DF,  p-value: 6.612e-13

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
employees |>
  mutate(date = zoo::as.yearmon(time, "%Y. %m")) |>
  plot_time_series_regression(
    .data = _,
    .date_var = time,
    .formula = total ~ as.numeric((date)),
    .interactive = FALSE
  ) +
  labs(
    title = "timetk를 사용한 신규 취업자수 시계열 회귀 모델",
    x = "연도",
    y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
employees |>
  mutate(date = zoo::as.yearmon(time, "%Y. %m")) |>
  plot_time_series_regression(
    .data = _,
    .date_var = time,
    .formula = total ~ year(date) + month(date, label = TRUE),
    .interactive = FALSE
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

## 6.6 지수 평활 모델

``` r
ses(students.ts[, 2]) |>
  summary()
```

    ## 
    ## Forecast method: Simple exponential smoothing
    ## 
    ## Model Information:
    ## Simple exponential smoothing 
    ## 
    ## Call:
    ##  ses(y = students.ts[, 2]) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.9999 
    ## 
    ##   Initial states:
    ##     l = 8394600.8229 
    ## 
    ##   sigma:  162570.5
    ## 
    ##      AIC     AICc      BIC 
    ## 599.8562 601.1896 603.1294 
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE     MASE      ACF1
    ## Training set -109072.2 155004.8 134671.7 -1.552769 1.849053 1.047911 0.4105529
    ## 
    ## Forecasts:
    ##      Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
    ## 2021        5995252 5786909 6203594 5676619 6313884
    ## 2022        5995252 5700626 6289878 5544660 6445843
    ## 2023        5995252 5634416 6356087 5443401 6547102
    ## 2024        5995252 5578598 6411905 5358035 6632468
    ## 2025        5995252 5529421 6461082 5282825 6707678
    ## 2026        5995252 5484962 6505542 5214830 6775673
    ## 2027        5995252 5444077 6546427 5152302 6838201
    ## 2028        5995252 5406022 6584481 5094103 6896401
    ## 2029        5995252 5370280 6620223 5039440 6951063
    ## 2030        5995252 5336474 6654029 4987739 7002765

``` r
autoplot(students.ts[, 2]) +
  autolayer(fitted(ses(students.ts[, 2])), series = "적합값")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
autoplot(students.ts[, 2]) +
  autolayer(fitted(ses(students.ts[, 2])), series = "적합값") +
  autolayer(ses(students.ts[, 2]), PI = FALSE, series = "0.99") +
  autolayer(ses(students.ts[, 2], alpha = 0.5), PI = FALSE, series = "0.5") +
  autolayer(ses(students.ts[, 2], alpha = 0.3), PI = FALSE, series = "0.3") +
  labs(
    title = "alpha값에 따른 단순 지수 평활 모델(전체 학생수)",
    x = "연도",
    y = "학생수",
    color = "alpha"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

``` r
autoplot(employees.ts[, 2]) +
  autolayer(fitted(ses(employees.ts[, 2])), series = "적합값") +
  autolayer(ses(employees.ts[, 2]), PI = FALSE, series = "0.99") +
  autolayer(ses(employees.ts[, 2], alpha = 0.3), PI = FALSE, series = "0.3") +
  autolayer(ses(employees.ts[, 2], alpha = 0.5), PI = FALSE, series = "0.5") +
  autolayer(ses(employees.ts[, 2], alpha = 0.7), PI = FALSE, series = "0.7") +
  labs(
    title = "alpha값에 따른 단순 지수 평활 모델(신규 취업자수)",
    x = "연도",
    y = "취업자수",
    color = "alpha"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

``` r
autoplot(covid19.ts[, 2]) +
  autolayer(fitted(ses(covid19.ts[, 2])), series = "적합값") +
  autolayer(ses(covid19.ts[, 2], h = 30), PI = FALSE, series = "0.99") +
  autolayer(ses(covid19.ts[, 2], alpha = 0.3, h = 30), PI = FALSE, series = "0.3") +
  autolayer(ses(covid19.ts[, 2], alpha = 0.5, h = 30), PI = FALSE, series = "0.5") +
  autolayer(ses(covid19.ts[, 2], alpha = 0.7, h = 30), PI = FALSE, series = "0.7") +
  labs(
    title = "alpha값에 따른 단순 지수 평활 모델(신규 확진자수)",
    x = "연도",
    y = "확진자수",
    color = "alpha"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
autoplot(students.ts[, 2], color = "black") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.1)), series = "0.1") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.2)), series = "0.2") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.3)), series = "0.3") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.4)), series = "0.4") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.5)), series = "0.5") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.6)), series = "0.6") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.7)), series = "0.7") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.8)), series = "0.8") +
  autolayer(fitted(ses(students.ts[, 2], alpha = 0.9)), series = "0.9") +
  autolayer(ses(students.ts[, 2], alpha = 0.1, h = 30), PI = FALSE, series = "0.1") +
  autolayer(ses(students.ts[, 2], alpha = 0.2, h = 30), PI = FALSE, series = "0.2") +
  autolayer(ses(students.ts[, 2], alpha = 0.3, h = 30), PI = FALSE, series = "0.3") +
  autolayer(ses(students.ts[, 2], alpha = 0.4, h = 30), PI = FALSE, series = "0.4") +
  autolayer(ses(students.ts[, 2], alpha = 0.5, h = 30), PI = FALSE, series = "0.5") +
  autolayer(ses(students.ts[, 2], alpha = 0.6, h = 30), PI = FALSE, series = "0.6") +
  autolayer(ses(students.ts[, 2], alpha = 0.7, h = 30), PI = FALSE, series = "0.7") +
  autolayer(ses(students.ts[, 2], alpha = 0.8, h = 30), PI = FALSE, series = "0.8") +
  autolayer(ses(students.ts[, 2], alpha = 0.9, h = 30), PI = FALSE, series = "0.9") +
  labs(
    title = "alpha값에 따른 단순 지수 평활 모델(신규 확진자수)",
    x = "연도",
    y = "확진자수",
    color = "alpha"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

``` r
summary(holt(students.ts[, 2]))
```

    ## 
    ## Forecast method: Holt's method
    ## 
    ## Model Information:
    ## Holt's method 
    ## 
    ## Call:
    ##  holt(y = students.ts[, 2]) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.8654 
    ##     beta  = 0.8654 
    ## 
    ##   Initial states:
    ##     l = 8966192.6427 
    ##     b = -36934.0002 
    ## 
    ##   sigma:  82114.94
    ## 
    ##      AIC     AICc      BIC 
    ## 571.4867 575.2367 576.9419 
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE        MPE      MAPE     MASE        ACF1
    ## Training set -5106.807 74275.76 46971.89 -0.0472354 0.5836661 0.365499 -0.00685211
    ## 
    ## Forecasts:
    ##      Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
    ## 2021        5855531 5750297 5960766 5694589 6016474
    ## 2022        5721369 5511013 5931725 5399658 6043081
    ## 2023        5587207 5242397 5932018 5059866 6114549
    ## 2024        5453045 4951452 5954638 4685925 6220166
    ## 2025        5318883 4641431 5996336 4282810 6354957
    ## 2026        5184721 4314366 6055077 3853627 6515816
    ## 2027        5050559 3971709 6129409 3400601 6700518
    ## 2028        4916397 3614582 6218212 2925443 6907352
    ## 2029        4782235 3243885 6320586 2429531 7134940
    ## 2030        4648073 2860364 6435783 1914007 7382139

``` r
autoplot(students.ts[, 2]) +
  autolayer(fitted(holt(students.ts[, 2])), series = "적합값") +
  autolayer(holt(students.ts[, 2]), series = "예측값") +
  labs(title = "전체 학생수에 대한 홀트 지수 평활 모델", x = "연도", y = "학생수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
autoplot(employees.ts[, 2]) +
  autolayer(fitted(holt(employees.ts[, 2])), series = "적합값") +
  autolayer(holt(employees.ts[, 2]), series = "예측값") +
  labs(
    title = "신규 취업자수에 대한 홀트 지수 평활 모델",
    x = "연도",
    y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

``` r
autoplot(covid19.ts[, 2]) +
  autolayer(fitted(holt(covid19.ts[, 2])), series = "적합값") +
  autolayer(holt(covid19.ts[, 2], h = 30), series = "예측값") +
  labs(
    title = "코로나 확진수(0-9세)에 대한 홀트 지수 평활 모델",
    x = "연도",
    y = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
autoplot(students.ts[, 2]) +
  autolayer(fitted(holt(students.ts[, 2])), series = "홀트 적합") +
  autolayer(fitted(holt(students.ts[, 2], damped = TRUE)), series = "감쇠 적합") +
  autolayer(holt(students.ts[, 2]), series = "홀트 예측", PI = FALSE) +
  autolayer(holt(students.ts[, 2], damped = TRUE), series = "감쇠 예측", PI = FALSE) +
  labs(
    title = "전체 학생수에 대한 감쇠 홀트 지수 평활 모델",
    x = "연도", y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

``` r
autoplot(employees.ts[, 2]) +
  autolayer(fitted(holt(employees.ts[, 2])), series = "홀트 적합") +
  autolayer(fitted(holt(employees.ts[, 2], damped = TRUE)), series = "감쇠 적합") +
  autolayer(holt(employees.ts[, 2]), series = "홀트 예측", PI = FALSE) +
  autolayer(holt(employees.ts[, 2], damped = TRUE), series = "감쇠 예측", PI = FALSE) +
  labs(
    title = "신규 취업자수에 대한 감쇠 홀트 지수 평활 모델",
    x = "연도",
    y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

``` r
autoplot(covid19.ts[, 2]) +
  autolayer(fitted(holt(covid19.ts[, 2])), series = "홀트 적합") +
  autolayer(fitted(holt(covid19.ts[, 2], damped = TRUE)), series = "감쇠 적합") +
  autolayer(holt(covid19.ts[, 2], h = 30), series = "홀트 예측", PI = FALSE) +
  autolayer(holt(covid19.ts[, 2], h = 30, damped = TRUE), series = "감쇠 예측", PI = FALSE) +
  labs(
    title = "코로나 확진수(0-9세)에 대한 홀트 지수 평활 모델",
    x = "연도",
    y = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
autoplot(students.ts[, 2], color = "black") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.1)), series = "0.1") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.2)), series = "0.2") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.3)), series = "0.3") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.4)), series = "0.4") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.5)), series = "0.5") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.6)), series = "0.6") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.7)), series = "0.7") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.8)), series = "0.8") +
  autolayer(fitted(holt(students.ts[, 2], beta = 0.9)), series = "0.9") +
  autolayer(holt(students.ts[, 2], beta = 0.1, h = 30), PI = FALSE, series = "0.1") +
  autolayer(holt(students.ts[, 2], beta = 0.2, h = 30), PI = FALSE, series = "0.2") +
  autolayer(holt(students.ts[, 2], beta = 0.3, h = 30), PI = FALSE, series = "0.3") +
  autolayer(holt(students.ts[, 2], beta = 0.4, h = 30), PI = FALSE, series = "0.4") +
  autolayer(holt(students.ts[, 2], beta = 0.5, h = 30), PI = FALSE, series = "0.5") +
  autolayer(holt(students.ts[, 2], beta = 0.6, h = 30), PI = FALSE, series = "0.6") +
  autolayer(holt(students.ts[, 2], beta = 0.7, h = 30), PI = FALSE, series = "0.7") +
  autolayer(holt(students.ts[, 2], beta = 0.8, h = 30), PI = FALSE, series = "0.8") +
  autolayer(holt(students.ts[, 2], beta = 0.9, h = 30), PI = FALSE, series = "0.9") +
  labs(
    title = "alpha값에 따른 단순 지수 평활 모델(신규 확진자수)",
    x = "연도",
    y = "확진자수",
    color = "beta"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

``` r
autoplot(employees.ts[, 2]) +
  autolayer(fitted(hw(employees.ts[, 2])), series = "홀트 윈터 적합값") +
  autolayer(hw(employees.ts[, 2], seasonal = "additive"), PI = FALSE, series = "덧셈방법") +
  autolayer(hw(employees.ts[, 2], seasonal = "multiplicative"), PI = FALSE, series = "곱셈방법") +
  labs(
    title = "신규 취업자수에 대한 홀트 윈터 지수 평활 모델",
    x = "연도",
    y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

``` r
ets(students.ts[, 2])
```

    ## ETS(A,Ad,N) 
    ## 
    ## Call:
    ##  ets(y = students.ts[, 2]) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.8929 
    ##     beta  = 0.8929 
    ##     phi   = 0.9401 
    ## 
    ##   Initial states:
    ##     l = 8930071.6714 
    ##     b = -36934.5936 
    ## 
    ##   sigma:  74403.08
    ## 
    ##      AIC     AICc      BIC 
    ## 567.8898 573.4898 574.4361

``` r
ets(students.ts[, 2]) |>
  autoplot()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
ets(students.ts[, 2]) |>
  forecast() |>
  autoplot() +
  labs(
    title = "전체 학생수에 대한 ets(A, Ad, N) 모델 예측 결과",
    x = "연도", y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
ets(employees.ts[, 2])
```

    ## ETS(M,Ad,A) 
    ## 
    ## Call:
    ##  ets(y = employees.ts[, 2]) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.9989 
    ##     beta  = 1e-04 
    ##     gamma = 2e-04 
    ##     phi   = 0.9744 
    ## 
    ##   Initial states:
    ##     l = 25055.6086 
    ##     b = 53.1073 
    ##     s = -222.3076 255.8486 261.7129 227.3178 163.5351 319.1916
    ##            274.8844 226.215 -5.605 -294.4144 -588.041 -618.3374
    ## 
    ##   sigma:  0.0045
    ## 
    ##      AIC     AICc      BIC 
    ## 1372.968 1381.851 1419.126

``` r
ets(employees.ts[, 2]) |>
  autoplot()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

``` r
ets(employees.ts[, 2]) |>
  forecast() |>
  autoplot() +
  labs(
    title = "신규 취업자에 대한 ets(M, Ad, A) 예측 결과",
    x = "연도", y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

## 6.7 ARIMA 모델

``` r
library(tseries)
```

    ## 
    ##     'tseries' version: 0.10-51
    ## 
    ##     'tseries' is a package for time series analysis and computational finance.
    ## 
    ##     See 'library(help="tseries")' for details.

``` r
set.seed(345)
arima100 <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.9), n = 200)
arima100 |> autoplot(main = "AR(1) 모델")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

``` r
urca::ur.kpss(arima100) |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.2684 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
ndiffs(arima100, test = "kpss")
```

    ## [1] 0

``` r
set.seed(345)
arima110 <- arima.sim(model = list(order = c(1, 1, 0), ar = 0.9), n = 200)
arima110 |> autoplot(main = "AR(1), 차분 1 모델")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

``` r
urca::ur.kpss(arima110) |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 3.2912 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
ndiffs(arima110, test = "kpss")
```

    ## [1] 1

``` r
urca::ur.kpss(diff(arima110)) |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.2684 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
arima100 |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-77-1.png)<!-- -->

``` r
arima110 |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

``` r
set.seed(345)
arima001 <- arima.sim(model = list(order = c(0, 0, 1), ma = 0.9), n = 200)
arima001 |> autoplot(main = "MA(1) 모델")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-79-1.png)<!-- -->

``` r
urca::ur.kpss(arima001) |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.0906 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
ndiffs(arima001)
```

    ## [1] 0

``` r
set.seed(345)
arima011 <- arima.sim(model = list(order = c(0, 1, 1), ma = 0.9), n = 200)
arima011 |> autoplot(main = "MA(1), 차분 1모델")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

``` r
urca::ur.kpss(arima011) |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 3.5785 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
ndiffs(arima011, test = "kpss")
```

    ## [1] 1

``` r
arima001 |> ggtsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-85-1.png)<!-- -->

``` r
arima011 |> ggtsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

``` r
set.seed(345)
arima101 <- arima.sim(model = list(order = c(1, 0, 1), ar = 0.9, ma = 0.9), n = 200)
arima101 |> autoplot(main = "AR(1), MA(1) 모델")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-87-1.png)<!-- -->

``` r
arima101 |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-88-1.png)<!-- -->

``` r
# install.packages("astsa")
library(astsa)
```

    ## 
    ## Attaching package: 'astsa'

    ## The following object is masked from 'package:forecast':
    ## 
    ##     gas

``` r
sarima(arima101, p = 1, d = 0, q = 1)
```

    ## initial  value 1.359489 
    ## iter   2 value -0.018418
    ## iter   3 value -0.028037
    ## iter   4 value -0.028204
    ## iter   5 value -0.028466
    ## iter   6 value -0.028483
    ## iter   7 value -0.028509
    ## iter   8 value -0.028558
    ## iter   9 value -0.028579
    ## iter  10 value -0.028582
    ## iter  11 value -0.028593
    ## iter  12 value -0.028606
    ## iter  13 value -0.028620
    ## iter  14 value -0.028666
    ## iter  15 value -0.028698
    ## iter  16 value -0.028709
    ## iter  17 value -0.028710
    ## iter  18 value -0.028710
    ## iter  19 value -0.028711
    ## iter  20 value -0.028713
    ## iter  21 value -0.028715
    ## iter  22 value -0.028717
    ## iter  23 value -0.028717
    ## iter  24 value -0.028717
    ## iter  25 value -0.028717
    ## iter  26 value -0.028717
    ## iter  27 value -0.028717
    ## iter  28 value -0.028717
    ## iter  29 value -0.028717
    ## iter  29 value -0.028717
    ## final  value -0.028717 
    ## converged
    ## initial  value -0.024032 
    ## iter   2 value -0.024200
    ## iter   3 value -0.024313
    ## iter   4 value -0.024363
    ## iter   5 value -0.024425
    ## iter   6 value -0.024425
    ## iter   7 value -0.024425
    ## iter   8 value -0.024425
    ## iter   9 value -0.024425
    ## iter  10 value -0.024425
    ## iter  11 value -0.024425
    ## iter  12 value -0.024425
    ## iter  13 value -0.024425
    ## iter  14 value -0.024426
    ## iter  15 value -0.024426
    ## iter  15 value -0.024426
    ## final  value -0.024426 
    ## converged

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-89-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S), 
    ##     xreg = xmean, include.mean = FALSE, transform.pars = trans, fixed = fixed, 
    ##     optim.control = list(trace = trc, REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##          ar1     ma1    xmean
    ##       0.8943  0.8966  -2.4748
    ## s.e.  0.0320  0.0399   1.1803
    ## 
    ## sigma^2 estimated as 0.9315:  log likelihood = -278.9,  aic = 565.81
    ## 
    ## $degrees_of_freedom
    ## [1] 197
    ## 
    ## $ttable
    ##       Estimate     SE t.value p.value
    ## ar1     0.8943 0.0320 27.9324  0.0000
    ## ma1     0.8966 0.0399 22.4571  0.0000
    ## xmean  -2.4748 1.1803 -2.0968  0.0373
    ## 
    ## $AIC
    ## [1] 2.829026
    ## 
    ## $AICc
    ## [1] 2.829638
    ## 
    ## $BIC
    ## [1] 2.894992

``` r
students.ts[, 2] |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-90-1.png)<!-- -->

``` r
urca::ur.kpss(students.ts[, 2]) |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 2 lags. 
    ## 
    ## Value of test-statistic is: 0.7939 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
ndiffs(students.ts[, 2], test = "kpss")
```

    ## [1] 2

``` r
sarima(students.ts[, 2], p = 1, d = 2, q = 0)
```

    ## initial  value 10.520394 
    ## iter   2 value 10.438482
    ## iter   3 value 10.437906
    ## iter   4 value 10.437883
    ## iter   4 value 10.437883
    ## final  value 10.437883 
    ## converged
    ## initial  value 10.416785 
    ## iter   2 value 10.416539
    ## iter   3 value 10.416530
    ## iter   3 value 10.416530
    ## iter   3 value 10.416530
    ## final  value 10.416530 
    ## converged

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-93-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S), 
    ##     include.mean = !no.constant, transform.pars = trans, fixed = fixed, optim.control = list(trace = trc, 
    ##         REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##          ar1
    ##       0.3861
    ## s.e.  0.2075
    ## 
    ## sigma^2 estimated as 1.107e+09:  log likelihood = -236.71,  aic = 477.42
    ## 
    ## $degrees_of_freedom
    ## [1] 19
    ## 
    ## $ttable
    ##     Estimate     SE t.value p.value
    ## ar1   0.3861 0.2075  1.8609  0.0783
    ## 
    ## $AIC
    ## [1] 23.87094
    ## 
    ## $AICc
    ## [1] 23.88205
    ## 
    ## $BIC
    ## [1] 23.97051

``` r
Arima(students.ts[, 2], order = c(1, 2, 0))
```

    ## Series: students.ts[, 2] 
    ## ARIMA(1,2,0) 
    ## 
    ## Coefficients:
    ##          ar1
    ##       0.3861
    ## s.e.  0.2075
    ## 
    ## sigma^2 = 1.174e+09:  log likelihood = -236.71
    ## AIC=477.42   AICc=478.12   BIC=479.41

``` r
Arima(students.ts[, 2], order = c(1, 2, 0)) |>
  forecast() |>
  autoplot() +
  labs(
    title = "전체 학생수에 대한 ARIMA(1, 2, 0) 모델 예측 결과",
    x = "연도",
    y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-95-1.png)<!-- -->

``` r
sarima(students.ts[, 2], p = 1, d = 2, q = 0)
```

    ## initial  value 10.520394 
    ## iter   2 value 10.438482
    ## iter   3 value 10.437906
    ## iter   4 value 10.437883
    ## iter   4 value 10.437883
    ## final  value 10.437883 
    ## converged
    ## initial  value 10.416785 
    ## iter   2 value 10.416539
    ## iter   3 value 10.416530
    ## iter   3 value 10.416530
    ## iter   3 value 10.416530
    ## final  value 10.416530 
    ## converged

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-96-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S), 
    ##     include.mean = !no.constant, transform.pars = trans, fixed = fixed, optim.control = list(trace = trc, 
    ##         REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##          ar1
    ##       0.3861
    ## s.e.  0.2075
    ## 
    ## sigma^2 estimated as 1.107e+09:  log likelihood = -236.71,  aic = 477.42
    ## 
    ## $degrees_of_freedom
    ## [1] 19
    ## 
    ## $ttable
    ##     Estimate     SE t.value p.value
    ## ar1   0.3861 0.2075  1.8609  0.0783
    ## 
    ## $AIC
    ## [1] 23.87094
    ## 
    ## $AICc
    ## [1] 23.88205
    ## 
    ## $BIC
    ## [1] 23.97051

``` r
auto.arima(students.ts[, 2])
```

    ## Series: students.ts[, 2] 
    ## ARIMA(1,2,0) 
    ## 
    ## Coefficients:
    ##          ar1
    ##       0.3861
    ## s.e.  0.2075
    ## 
    ## sigma^2 = 1.174e+09:  log likelihood = -236.71
    ## AIC=477.42   AICc=478.12   BIC=479.41

``` r
auto.arima(students.ts[, 2]) |>
  forecast() |>
  autoplot() +
  labs(
    title = "전체 학생수에 대한 ARIMA(1, 2, 0) 모델 예측 결과",
    x = "연도",
    y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-98-1.png)<!-- -->

``` r
urca::ur.kpss(covid19.ts[, 2]) |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 5 lags. 
    ## 
    ## Value of test-statistic is: 3.2656 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
ndiffs(covid19.ts[, 2], test = "kpss")
```

    ## [1] 1

``` r
diff(covid19.ts[, 2]) |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-101-1.png)<!-- -->

``` r
auto.arima(covid19.ts[, 2])
```

    ## Series: covid19.ts[, 2] 
    ## ARIMA(2,1,1) 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1
    ##       0.1126  -0.1775  -0.6535
    ## s.e.  0.0868   0.0688   0.0726
    ## 
    ## sigma^2 = 26.74:  log likelihood = -920.47
    ## AIC=1848.93   AICc=1849.07   BIC=1863.76

``` r
sarima(covid19.ts[, 2], 2, 1, 1)
```

    ## initial  value 1.793862 
    ## iter   2 value 1.678611
    ## iter   3 value 1.660151
    ## iter   4 value 1.655522
    ## iter   5 value 1.643145
    ## iter   6 value 1.641880
    ## iter   7 value 1.641239
    ## iter   8 value 1.640877
    ## iter   9 value 1.640615
    ## iter  10 value 1.640567
    ## iter  11 value 1.640564
    ## iter  12 value 1.640564
    ## iter  13 value 1.640564
    ## iter  13 value 1.640564
    ## iter  13 value 1.640564
    ## final  value 1.640564 
    ## converged
    ## initial  value 1.638392 
    ## iter   2 value 1.638388
    ## iter   3 value 1.638387
    ## iter   4 value 1.638387
    ## iter   5 value 1.638387
    ## iter   5 value 1.638387
    ## iter   5 value 1.638387
    ## final  value 1.638387 
    ## converged

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-103-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S), 
    ##     xreg = constant, transform.pars = trans, fixed = fixed, optim.control = list(trace = trc, 
    ##         REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1  constant
    ##       0.1150  -0.1768  -0.6576    0.0631
    ## s.e.  0.0866   0.0688   0.0723    0.0963
    ## 
    ## sigma^2 estimated as 26.43:  log likelihood = -920.25,  aic = 1850.51
    ## 
    ## $degrees_of_freedom
    ## [1] 297
    ## 
    ## $ttable
    ##          Estimate     SE t.value p.value
    ## ar1        0.1150 0.0866  1.3287  0.1850
    ## ar2       -0.1768 0.0688 -2.5694  0.0107
    ## ma1       -0.6576 0.0723 -9.0980  0.0000
    ## constant   0.0631 0.0963  0.6552  0.5128
    ## 
    ## $AIC
    ## [1] 6.147873
    ## 
    ## $AICc
    ## [1] 6.148322
    ## 
    ## $BIC
    ## [1] 6.209453

``` r
employees |>
  mutate(
    year = year(time),
    qtr = quarter(time)
  ) |>
  group_by(year, qtr) |>
  summarize(sum = sum(total), .groups = "drop") |>
  ungroup() |>
  ts(frequency = 4, start = c(2013, 1)) ->
qtr.employees.ts
auto.arima(qtr.employees.ts[, 3]) |>
  summary()
```

    ## Series: qtr.employees.ts[, 3] 
    ## ARIMA(0,1,0)(0,1,0)[4] 
    ## 
    ## sigma^2 = 240342:  log likelihood = -205.52
    ## AIC=413.04   AICc=413.2   BIC=414.33
    ## 
    ## Training set error measures:
    ##                    ME     RMSE      MAE        MPE      MAPE      MASE       ACF1
    ## Training set -119.056 450.3206 266.8772 -0.1506314 0.3348234 0.2848644 0.02400751

``` r
qtr.employees.ts[, 3] |>
  tsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-105-1.png)<!-- -->

``` r
qtr.employees.ts[, 3] |>
  urca::ur.kpss() |>
  urca::summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 3 lags. 
    ## 
    ## Value of test-statistic is: 0.8663 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
qtr.employees.ts[, 3] |> ndiffs()
```

    ## [1] 1

``` r
qtr.employees.ts[, 3] |>
  diff() |>
  tsdisplay(lag.max = 36)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-108-1.png)<!-- -->

``` r
sarima(qtr.employees.ts[, 3], p = 0, d = 1, q = 0, P = 1, D = 1, Q = 0, S = 4)
```

    ## initial  value 6.201317 
    ## iter   2 value 6.192952
    ## iter   3 value 6.185212
    ## iter   4 value 6.185187
    ## iter   4 value 6.185187
    ## final  value 6.185187 
    ## converged
    ## initial  value 6.179834 
    ## iter   2 value 6.179786
    ## iter   3 value 6.179732
    ## iter   3 value 6.179732
    ## iter   3 value 6.179732
    ## final  value 6.179732 
    ## converged

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-109-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S), 
    ##     include.mean = !no.constant, transform.pars = trans, fixed = fixed, optim.control = list(trace = trc, 
    ##         REPORT = 1, reltol = tol))
    ## 
    ## Coefficients:
    ##          sar1
    ##       -0.2955
    ## s.e.   0.3359
    ## 
    ## sigma^2 estimated as 230020:  log likelihood = -205.16,  aic = 414.33
    ## 
    ## $degrees_of_freedom
    ## [1] 26
    ## 
    ## $ttable
    ##      Estimate     SE t.value p.value
    ## sar1  -0.2955 0.3359 -0.8799   0.387
    ## 
    ## $AIC
    ## [1] 15.34549
    ## 
    ## $AICc
    ## [1] 15.35141
    ## 
    ## $BIC
    ## [1] 15.44148

``` r
auto.arima(qtr.employees.ts[, 3])
```

    ## Series: qtr.employees.ts[, 3] 
    ## ARIMA(0,1,0)(0,1,0)[4] 
    ## 
    ## sigma^2 = 240342:  log likelihood = -205.52
    ## AIC=413.04   AICc=413.2   BIC=414.33

``` r
sarima(qtr.employees.ts[, 3], 0, 1, 0, 0, 1, 0, 4)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-111-1.png)<!-- -->

    ## $fit
    ## 
    ## Call:
    ## arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S), 
    ##     include.mean = !no.constant, transform.pars = trans, fixed = fixed, optim.control = list(trace = trc, 
    ##         REPORT = 1, reltol = tol))
    ## 
    ## 
    ## sigma^2 estimated as 239349:  log likelihood = -205.52,  aic = 413.04
    ## 
    ## $degrees_of_freedom
    ## [1] 27
    ## 
    ## $ttable
    ##      Estimate p.value
    ## 
    ## $AIC
    ## [1] 15.29763
    ## 
    ## $AICc
    ## [1] 15.29763
    ## 
    ## $BIC
    ## [1] 15.34562

``` r
arima010110 <- Arima(qtr.employees.ts[, 3], order = c(0, 1, 0), seasonal = c(1, 1, 0))
summary(arima010110)
```

    ## Series: qtr.employees.ts[, 3] 
    ## ARIMA(0,1,0)(1,1,0)[4] 
    ## 
    ## Coefficients:
    ##          sar1
    ##       -0.2955
    ## s.e.   0.3359
    ## 
    ## sigma^2 = 239898:  log likelihood = -205.16
    ## AIC=414.33   AICc=414.83   BIC=416.92
    ## 
    ## Training set error measures:
    ##                     ME     RMSE      MAE        MPE      MAPE    MASE        ACF1
    ## Training set -127.5149 441.4946 260.5119 -0.1615043 0.3268788 0.27807 -0.05923251

``` r
arima010010 <- Arima(qtr.employees.ts[, 3], order = c(0, 1, 0), seasonal = c(0, 1, 0))
summary(arima010010)
```

    ## Series: qtr.employees.ts[, 3] 
    ## ARIMA(0,1,0)(0,1,0)[4] 
    ## 
    ## sigma^2 = 240342:  log likelihood = -205.52
    ## AIC=413.04   AICc=413.2   BIC=414.33
    ## 
    ## Training set error measures:
    ##                    ME     RMSE      MAE        MPE      MAPE      MASE       ACF1
    ## Training set -119.056 450.3206 266.8772 -0.1506314 0.3348234 0.2848644 0.02400751

``` r
forecast010110 <- arima010110 |> forecast()
forecast010010 <- arima010010 |> forecast()
autoplot(qtr.employees.ts[, 3]) +
  autolayer(forecast010110, PI = F, series = "010110") +
  autolayer(forecast010010, PI = F, series = "010010") +
  labs(
    title = "분기별 취업자수에 대한 ARIMA(0,1,0)(0,1,0)[4]와 ARIMA(0,1,0)(1,1,0)[4] 예측결과",
    x = "연동", y = "취업자수", color = "모델"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-114-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  auto.arima()
```

    ## Series: employees.ts[, 2] 
    ## ARIMA(0,1,0)(0,1,1)[12] 
    ## 
    ## Coefficients:
    ##          sma1
    ##       -0.4246
    ## s.e.   0.1648
    ## 
    ## sigma^2 = 15320:  log likelihood = -518.14
    ## AIC=1040.27   AICc=1040.42   BIC=1045.11

``` r
employees.ts[, 2] |>
  tsdisplay()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-116-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  urca::ur.kpss() |>
  urca::summary()
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
employees.ts[, 2] |>
  ndiffs()
```

    ## [1] 1

``` r
employees.ts[, 2] |>
  diff() |>
  tsdisplay(lag.max = 36)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-119-1.png)<!-- -->

``` r
arima011110 <- employees.ts[, 2] |>
  Arima(order = c(0, 1, 1), seasonal = c(1, 1, 0))
summary(arima011110)
```

    ## Series: employees.ts[, 2] 
    ## ARIMA(0,1,1)(1,1,0)[12] 
    ## 
    ## Coefficients:
    ##          ma1     sar1
    ##       0.0087  -0.3649
    ## s.e.  0.1385   0.1395
    ## 
    ## sigma^2 = 15667:  log likelihood = -518.22
    ## AIC=1042.45   AICc=1042.75   BIC=1049.7
    ## 
    ## Training set error measures:
    ##                     ME    RMSE      MAE         MPE      MAPE      MASE        ACF1
    ## Training set -15.58899 114.974 74.83796 -0.05996543 0.2828592 0.2361174 -0.02801497

``` r
arima010011 <- employees.ts[, 2] |>
  Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1))
summary(arima010011)
```

    ## Series: employees.ts[, 2] 
    ## ARIMA(0,1,0)(0,1,1)[12] 
    ## 
    ## Coefficients:
    ##          sma1
    ##       -0.4246
    ## s.e.   0.1648
    ## 
    ## sigma^2 = 15320:  log likelihood = -518.14
    ## AIC=1040.27   AICc=1040.42   BIC=1045.11
    ## 
    ## Training set error measures:
    ##                     ME     RMSE      MAE         MPE      MAPE      MASE        ACF1
    ## Training set -17.24831 114.3934 73.75538 -0.06622242 0.2787589 0.2327018 -0.01264952

``` r
forecast011110 <- arima011110 |> forecast()
forecast010011 <- arima010011 |> forecast()
```

``` r
autoplot(employees.ts[, 2]) +
  autolayer(forecast011110, PI = F, series = "011110") +
  autolayer(forecast010011, PI = F, series = "010011")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-123-1.png)<!-- -->

``` r
dts <- employees.ts[, 2]
autoplot(employees.ts[, 2]) +
  autolayer(forecast010011, PI = F, series = "010011") +
  autolayer(forecast(Arima(dts, order = c(1, 1, 0), seasonal = c(0, 1, 1))), PI = F, series = "110011") +
  autolayer(forecast(Arima(dts, order = c(0, 1, 1), seasonal = c(0, 1, 1))), PI = F, series = "011011") +
  autolayer(forecast(Arima(dts, order = c(1, 1, 1), seasonal = c(0, 1, 1))), PI = F, series = "111011") +
  autolayer(forecast(Arima(dts, order = c(1, 1, 1), seasonal = c(1, 1, 1))), PI = F, series = "111111") +
  labs(title = "월별 신규 취업자에 대한 ARIMA 모델 비교", x = "연도", y = "취업자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-124-1.png)<!-- -->

## 6.8 TBATS 모델

``` r
covid19.ts[, 2] |>
  tbats() |>
  forecast() |>
  autoplot() +
  labs(
    title = "코로나 확진자(0-9세)에 대한 TBATS 모델 예측",
    x = "시간",
    y = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-125-1.png)<!-- -->

## 6.9 prophet 모델

``` r
# install.packages("prophet")
library(prophet)
```

    ## Loading required package: Rcpp

    ## Loading required package: rlang

``` r
students.prophet <- data.frame(ds = students$연도, y = students$학생수계)
model.prophet.students <- prophet(students.prophet)
```

    ## Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

    ## n.changepoints greater than number of observations. Using 16

``` r
future.students <- make_future_dataframe(model.prophet.students, periods = 10, freq = "year")
forecast.students <- predict(model.prophet.students, future.students)
```

``` r
plot(model.prophet.students, forecast.students) +
  ggrepel::geom_text_repel(
    aes(label = scales::number(y, big.mark = ",", accuracy = 1)),
    vjust = 1, size = 3
  ) +
  labs(
    title = "전체 학생수에 대한 prophet 모델 예측 결과",
    x = "연도",
    y = "학생수"
  ) +
  scale_y_continuous(labels = scales::label_comma())
```

    ## Warning: Removed 10 rows containing missing values (geom_text_repel).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-128-1.png)<!-- -->

``` r
prophet_plot_components(model.prophet.students, forecast.students)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-129-1.png)<!-- -->

``` r
employees.prophet <- data.frame(ds = employees[, 1], y = employees[, 2])
model.prophet.employees <- prophet(employees.prophet)
```

    ## Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future.employees <- make_future_dataframe(
  model.prophet.employees,
  periods = 10, freq = "month"
)
forecast.employees <- predict(model.prophet.employees, future.employees)
plot(model.prophet.employees, forecast.employees) +
  labs(
    title = "월별 전체 취업자수에 대한 prophet 모델 예측 결과",
    x = "연월", y = "취업자수"
  ) +
  scale_y_continuous(labels = scales::label_comma())
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-130-1.png)<!-- -->

``` r
prophet_plot_components(model.prophet.employees, forecast.employees)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-131-1.png)<!-- -->

``` r
covid.prophet <- data.frame(ds = covid19$date, y = covid19$`0-9세`)
model.prophet.covid <- prophet(covid.prophet,
  yearly.seasonality = TRUE,
  daily.seasonality = TRUE,
  weekly.seasonality = TRUE
)
future.covid <- make_future_dataframe(
  model.prophet.covid,
  periods = 100, freq = "day"
)
forecast.covid <- predict(model.prophet.covid, future.covid)
plot(model.prophet.covid, forecast.covid) +
  labs(
    title = "일별 코로나 확진자수에 대한 prophet 모델 예측 결과(0-9세)",
    x = "연월",
    y = "확진자수"
  ) +
  scale_y_continuous(labels = scales::label_comma())
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-132-1.png)<!-- -->

``` r
prophet_plot_components(model.prophet.covid, forecast.covid)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-133-1.png)<!-- -->

## 6.10 신경망 모델

``` r
students.ts[, 2] |>
  nnetar() |>
  forecast(PI = TRUE) |>
  autoplot() +
  labs(
    title = "전체 학생수에 대한 신경망 모델 예측 결과",
    x = "연도", y = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-134-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  nnetar() |>
  forecast(PI = TRUE) |>
  autoplot() +
  labs(
    title = "신규 취업자수에 대한 신경망 모델 예측 결과",
    x = "연도",
    y = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-135-1.png)<!-- -->

``` r
covid19.ts[, 2] |>
  nnetar(p = 22, P = 12) |>
  forecast(h = 100, PI = TRUE) |>
  autoplot() +
  labs(
    title = "코로나 확진자(0-9세)에 대한 신경망 모델 예측 결과",
    x = "연도",
    y = "확진자수"
  )
```

    ## Warning in nnetar(covid19.ts[, 2], p = 22, P = 12): Series too short for seasonal lags

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note06_files/figure-gfm/unnamed-chunk-136-1.png)<!-- -->
