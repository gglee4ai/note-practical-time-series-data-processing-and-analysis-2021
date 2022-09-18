note04
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

# 4 시계열 데이터 처리

## 4.1 오늘 며칠일까?: 시간 정보 추출

``` r
now.date <- Sys.time()
now.date
```

    ## [1] "2022-09-18 13:05:12 KST"

``` r
now.datec <- as.character(now.date)
now.datec
```

    ## [1] "2022-09-18 13:05:12"

``` r
year(now.date)
```

    ## [1] 2022

``` r
month(now.date)
```

    ## [1] 9

``` r
day(now.date)
```

    ## [1] 18

``` r
wday(now.date)
```

    ## [1] 1

``` r
hour(now.date)
```

    ## [1] 13

``` r
minute(now.date)
```

    ## [1] 5

``` r
second(now.date)
```

    ## [1] 12.39248

``` r
yday(now.date)
```

    ## [1] 261

``` r
week(now.date)
```

    ## [1] 38

``` r
quarter(now.date)
```

    ## [1] 3

``` r
semester(now.date)
```

    ## [1] 2

``` r
am(now.date)
```

    ## [1] FALSE

``` r
pm(now.date)
```

    ## [1] TRUE

``` r
leap_year(now.date)
```

    ## [1] FALSE

``` r
wday(now.date, label = TRUE, abbr = FALSE)
```

    ## [1] Sunday
    ## Levels: Sunday < Monday < Tuesday < Wednesday < Thursday < Friday < Saturday

## 4.2 며칠 지났을까: 시간 기간 연산

``` r
as.Date("2021-01-01") - as.Date("1980-01-01")
```

    ## Time difference of 14976 days

``` r
today <- today()
today
```

    ## [1] "2022-09-18"

``` r
now <- now()
now
```

    ## [1] "2022-09-18 13:05:12 KST"

``` r
today + 100
```

    ## [1] "2022-12-27"

``` r
today - months(2)
```

    ## [1] "2022-07-18"

``` r
class(months(2))
```

    ## [1] "Period"
    ## attr(,"package")
    ## [1] "lubridate"

``` r
today - years(1)
```

    ## [1] "2021-09-18"

``` r
int <- interval(as.Date("1980-01-01"), as.Date("2021-12-31"))
int
```

    ## [1] 1980-01-01 UTC--2021-12-31 UTC

``` r
as.period(int)
```

    ## [1] "41y 11m 30d 0H 0M 0S"

``` r
as.duration(int)
```

    ## [1] "1325376000s (~42 years)"

``` r
int1 <- "1980-01-01" %--% "2021-12-31"
int1
```

    ## [1] 1980-01-01 UTC--2021-12-31 UTC

``` r
leap_year(2020)
```

    ## [1] TRUE

``` r
as.Date("2020-01-01") + years(1)
```

    ## [1] "2021-01-01"

``` r
as.Date("2021-01-01") + dyears(1)
```

    ## [1] "2022-01-01 06:00:00 UTC"

``` r
as.Date("2020-02-01") + months(1)
```

    ## [1] "2020-03-01"

``` r
as.Date("2020-02-01") + dmonths(1)
```

    ## [1] "2020-03-02 10:30:00 UTC"

``` r
as.Date("2021-02-01") + months(1)
```

    ## [1] "2021-03-01"

``` r
as.Date("2021-02-01") + dmonths(1)
```

    ## [1] "2021-03-03 10:30:00 UTC"

## 4.3 이번 주 마지막 날은 며칠일까?: 시간 반올림

``` r
x <- as.Date("2022-09-14 13:45:40")
x
```

    ## [1] "2022-09-14"

``` r
round_date(x, "week")
```

    ## [1] "2022-09-11"

``` r
floor_date(x, "week")
```

    ## [1] "2022-09-11"

``` r
ceiling_date(x, "week")
```

    ## [1] "2022-09-18"

``` r
round_date(x, "month")
```

    ## [1] "2022-09-01"

``` r
floor_date(x, "month")
```

    ## [1] "2022-09-01"

``` r
ceiling_date(x, "month")
```

    ## [1] "2022-10-01"

``` r
round_date(x, "year")
```

    ## [1] "2022-01-01"

``` r
floor_date(x, "year")
```

    ## [1] "2022-01-01"

``` r
ceiling_date(x, "year")
```

    ## [1] "2023-01-01"

``` r
days_in_month(as.Date("2021-09-01"))
```

    ## Sep 
    ##  30

## 4.4 주간, 월간 데이터 합계, 평균은?: 시간 그루핑

``` r
employees.by.year <-
  employees |>
  mutate(year = year(time)) |>
  group_by(year) |>
  summarize(
    total.year = sum(total),
    employees.edu = sum(employees.edu)
  )
employees.by.year
```

    ## # A tibble: 8 × 3
    ##    year total.year employees.edu
    ##   <dbl>      <int>         <int>
    ## 1  2013     303592         21192
    ## 2  2014     310766         21957
    ## 3  2015     314133         22028
    ## 4  2016     316910         22350
    ## 5  2017     320698         22886
    ## 6  2018     321866         22165
    ## 7  2019     325474         22600
    ## 8  2020     322852         21570

``` r
employees.by.year |>
  ggplot(aes(as.factor(year), total.year)) +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = scales::number(total.year, big.mark = ",")),
    size = 3, vjust = 1.5
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "연도별 취업자수", x = "연도", y = "취업자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
library(tsibble)
```

    ## 
    ## Attaching package: 'tsibble'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     interval

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, union

``` r
mean.covid19.by.age <-
  covid19 |>
  mutate(yearmon = yearmonth(date)) |>
  group_by(yearmon) |>
  summarize(
    `01대` = mean(`0-9세`),
    `10대` = mean(`10-19세`),
    `20대` = mean(`20-29세`),
    `30대` = mean(`30-39세`),
    `40대` = mean(`40-49세`),
    `50대` = mean(`50-59세`),
    `60대` = mean(`60-69세`),
    `70대` = mean(`70-79세`),
    `80대` = mean(`80세 이상`),
  )
mean.covid19.by.age
```

    ## # A tibble: 11 × 10
    ##     yearmon `01대` `10대` `20대` `30대` `40대` `50대` `60대` `70대` `80대`
    ##       <mth>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ##  1 2020 Apr  0.636   1.91   5.45   2.55   1.82   1.86   1.64  0.773  0.682
    ##  2 2020 May  0.533   2.07   7.13   4.13   3.03   2.4    1.8   0.5    0.4  
    ##  3 2020 Jun  1.11    1.71   6.11   6.71   4.89   7.61   8.39  3.93   2    
    ##  4 2020 Jul  1.71    2.39   8.32  10.1    8.03   7.55   5.97  3.13   1.35 
    ##  5 2020 Aug  6.32   11.7   22.6   22     24.6   36.1   36.0  16.7    6.06 
    ##  6 2020 Sep  4.53    5.37  14.7   13.9   16.2   26.1   27.1  14.7    6.17 
    ##  7 2020 Oct  3.73    4.57  12.9   12.1   12.4   14.5   13.2   6.63   5.77 
    ##  8 2020 Nov 10.0    19.8   43.3   36.5   41.1   44.9   34.4  16.9    9.47 
    ##  9 2020 Dec 33.9    53.0  100.   103.   118.   158.   134.   65.9   48.4  
    ## 10 2021 Jan 28.8    46.2   72.3   72.5   83.5  106.    85.2  40.0   29.1  
    ## 11 2021 Feb 20.9    NA     45.8   47.7   56.7   66.7   59.1  26     14.3

``` r
mean.covid19.by.age |>
  tidyr::pivot_longer(2:10, names_to = "category") |>
  ggplot(aes(yearmon, value)) +
  geom_line(aes(group = category, color = category)) +
  labs(
    title = "월간 평균 코로나 확진자수",
    x = "일자",
    y = "평균 확진자",
    color = "세대"
  )
```

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
# install.packages("tibbletime")
library(tibbletime)
```

    ## 
    ## Attaching package: 'tibbletime'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
as_tbl_time(covid19, index = date) |>
  collapse_by("weekly") |>
  group_by(date) |>
  summarize(
    `01대` = mean(`0-9세`),
    `10대` = mean(`10-19세`),
    `20대` = mean(`20-29세`),
    `30대` = mean(`30-39세`),
    `40대` = mean(`40-49세`),
    `50대` = mean(`50-59세`),
    `60대` = mean(`60-69세`),
    `70대` = mean(`70-79세`),
    `80대` = mean(`80세 이상`),
  ) |>
  tidyr::gather(category, value, 2:10) |>
  ggplot(aes(date, value)) +
  geom_line(aes(group = category, color = category)) +
  labs(
    title = "주간 평균 코로나 확진자수",
    x = "월",
    y = "평균 확진자",
    color = "세대"
  )
```

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
library(timetk)
covid19 |>
  summarize_by_time(
    .date_var = date,
    .by = "week",
    `01대` = mean(`0-9세`),
    `10대` = mean(`10-19세`),
    `20대` = mean(`20-29세`),
    `30대` = mean(`30-39세`),
    `40대` = mean(`40-49세`),
    `50대` = mean(`50-59세`),
    `60대` = mean(`60-69세`),
    `70대` = mean(`70-79세`),
    `80대` = mean(`80세 이상`)
  ) |>
  tidyr::gather(category, value, 2:10) |>
  ggplot(aes(date, value)) +
  geom_line(aes(group = category, color = category)) +
  labs(
    title = "주간 평균 코로나 확진자수",
    x = "월",
    y = "평균 확진자",
    color = "세대"
  )
```

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
employees |>
  summarize_by_time(
    .date_var = time,
    .by = "month",
    total.year = sum(total),
    employees.edu = sum(employees.edu)
  ) |>
  head(10)
```

    ## # A tibble: 10 × 3
    ##    time       total.year employees.edu
    ##    <date>          <int>         <int>
    ##  1 2013-01-01      24287          1710
    ##  2 2013-02-01      24215          1681
    ##  3 2013-03-01      24736          1716
    ##  4 2013-04-01      25322          1745
    ##  5 2013-05-01      25610          1774
    ##  6 2013-06-01      25686          1786
    ##  7 2013-07-01      25681          1813
    ##  8 2013-08-01      25513          1811
    ##  9 2013-09-01      25701          1794
    ## 10 2013-10-01      25798          1790

``` r
employees.tsibble |>
  index_by(yearqtr = ~ yearquarter(.)) |>
  summarize(sum.qtrly = sum(total))
```

    ## # A tsibble: 32 x 2 [1Q]
    ##    yearqtr sum.qtrly
    ##      <qtr>     <int>
    ##  1 2013 Q1     73238
    ##  2 2013 Q2     76618
    ##  3 2013 Q3     76895
    ##  4 2013 Q4     76841
    ##  5 2014 Q1     75629
    ##  6 2014 Q2     78275
    ##  7 2014 Q3     78676
    ##  8 2014 Q4     78186
    ##  9 2015 Q1     76629
    ## 10 2015 Q2     79024
    ## # … with 22 more rows

``` r
covid19.tsibble[, c(1, 3)] |>
  index_by(yw = ~ yearweek(.)) |>
  summarize(sum.weekly = sum(`0-9세`))
```

    ## # A tsibble: 45 x 2 [1W]
    ##          yw sum.weekly
    ##      <week>      <dbl>
    ##  1 2020 W15          4
    ##  2 2020 W16          8
    ##  3 2020 W17          3
    ##  4 2020 W18         -1
    ##  5 2020 W19          1
    ##  6 2020 W20          6
    ##  7 2020 W21          1
    ##  8 2020 W22          8
    ##  9 2020 W23          7
    ## 10 2020 W24          3
    ## # … with 35 more rows

``` r
covid19.tsibble[, c(1, 3)] |>
  index_by(twomonth = ~ floor_date(., "2 month")) |>
  summarize(sum.2month = sum(`0-9세`))
```

    ## # A tsibble: 6 x 2 [1D]
    ##   twomonth   sum.2month
    ##   <date>          <dbl>
    ## 1 2020-03-01         14
    ## 2 2020-05-01         47
    ## 3 2020-07-01        249
    ## 4 2020-09-01        248
    ## 5 2020-11-01       1317
    ## 6 2021-01-01       1081

``` r
covid19.tsibble[, c(1, 3)] |>
  index_by(fourday = ~ floor_date(., "4 day")) |>
  summarize(sum.4days = sum(`0-9세`))
```

    ## # A tsibble: 81 x 2 [1D]
    ##    fourday    sum.4days
    ##    <date>         <dbl>
    ##  1 2020-04-09         4
    ##  2 2020-04-13         5
    ##  3 2020-04-17         3
    ##  4 2020-04-21         0
    ##  5 2020-04-25         2
    ##  6 2020-04-29         0
    ##  7 2020-05-01         0
    ##  8 2020-05-05         1
    ##  9 2020-05-09         1
    ## 10 2020-05-13         5
    ## # … with 71 more rows

``` r
library(xts)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following object is masked from 'package:tsibble':
    ## 
    ##     index

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

``` r
apply.quarterly(employees.xts, sum)
```

    ##             [,1]
    ## 2013-03-01 78345
    ## 2013-06-01 81923
    ## 2013-09-01 82313
    ## 2013-12-01 82203
    ## 2014-03-01 80977
    ## 2014-06-01 83779
    ## 2014-09-01 84226
    ## 2014-12-01 83741
    ## 2015-03-01 82095
    ## 2015-06-01 84488
    ## 2015-09-01 84895
    ## 2015-12-01 84683
    ## 2016-03-01 82700
    ## 2016-06-01 85250
    ## 2016-09-01 85762
    ## 2016-12-01 85548
    ## 2017-03-01 83981
    ## 2017-06-01 86591
    ## 2017-09-01 86693
    ## 2017-12-01 86319
    ## 2018-03-01 84335
    ## 2018-06-01 86583
    ## 2018-09-01 86620
    ## 2018-12-01 86493
    ## 2019-03-01 84927
    ## 2019-06-01 87459
    ## 2019-09-01 87867
    ## 2019-12-01 87821
    ## 2020-03-01 85689
    ## 2020-06-01 85949
    ## 2020-09-01 86595
    ## 2020-12-01 86189

``` r
apply.yearly(employees.xts, sum) |>
  plot.xts()
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
apply.monthly(covid19.xts[, 1], sum) |>
  plot(main = "월별 0-9세 코로나 확진자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
apply.quarterly(covid19.xts[, 1], sum) |>
  plot.xts(main = "분기별 0-9세 코로나 확진자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

## 4.5 주식 시가, 고가, 저가, 종가는 어떻게 구할까? OHLC

``` r
as_tbl_time(covid19, index = date) |>
  collapse_by("weekly") |>
  group_by(date) |>
  summarize(
    Open = first(`0-9세`),
    High = max(`0-9세`),
    Low = min(`0-9세`),
    Close = last(`0-9세`)
  )
```

    ## # A time tibble: 45 × 5
    ## # Index: date
    ##    date        Open  High   Low Close
    ##    <date>     <dbl> <dbl> <dbl> <dbl>
    ##  1 2020-04-11     2     2     1     1
    ##  2 2020-04-18     0     3     0     2
    ##  3 2020-04-25     0     1     0     1
    ##  4 2020-05-02     2     2    -1     0
    ##  5 2020-05-09     0     1     0     0
    ##  6 2020-05-16     0     2     0     2
    ##  7 2020-05-23     0     1     0     0
    ##  8 2020-05-30     0     3     0     1
    ##  9 2020-06-06     0     3     0     3
    ## 10 2020-06-13     1     1     0     0
    ## # … with 35 more rows

``` r
to.period(covid19.xts, method = "months", OHLC = TRUE)
```

    ## Warning in to.period(covid19.xts, method = "months", OHLC = TRUE): missing values removed from data

    ##            covid19.xts.Open covid19.xts.High covid19.xts.Low covid19.xts.Close
    ## 2020-04-30                2                5               1                 2
    ## 2020-05-31                0               11               0                 7
    ## 2020-06-30                0                6               0                 6
    ## 2020-07-31                0               10              -4                13
    ## 2020-08-31                1               35               0                15
    ## 2020-09-30               14               17               0                20
    ## 2020-10-31                6               11               0                22
    ## 2020-11-30                5               57               9                32
    ## 2020-12-31               15               86               0               132
    ## 2021-01-31               41              138              44                49
    ## 2021-02-08               20               41              30                31

## 4.6 3일 평균, 5일 합계는: 시간 롤링

``` r
library(zoo)
employees |>
  mutate(
    ma3 = rollmean(total, k = 3, fill = NA),
    sum3 = rollapply(total, 3, sum, fill = NA)
  ) |>
  select(time, total, ma3, sum3) |>
  ggplot(aes(time)) +
  geom_line(aes(y = total, group = 1, color = "월간")) +
  geom_line(aes(y = ma3, group = 1, color = "3개월 평균")) +
  labs(y = "취업자수", x = "연도") +
  scale_color_manual(values = c(
    "월간" = "red",
    "3개월 평균" = "blue"
  ))
```

    ## Warning: Removed 2 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
ma3 <- slidify(mean, .period = 3, .align = "center")
sum3 <- slidify(sum, .period = 3, .align = "center")
class(ma3)
```

    ## [1] "function"

``` r
employees |>
  mutate(
    ma3 = ma3(total),
    sum3 = sum3(total)
  ) |>
  ggplot(aes(time)) +
  geom_line(aes(y = total, group = 1, color = "월간")) +
  geom_line(aes(y = ma3, group = 1, color = "3개월 평균")) +
  labs(y = "취업자수", x = "연도") +
  scale_color_manual(values = c(
    "월간" = "red",
    "3개월 평균" = "blue"
  ))
```

    ## Warning: Removed 2 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
rollapply(employees.xts, width = 3, FUN = mean) |>
  head()
```

    ##               total employees.edu
    ## 2013-01-01       NA            NA
    ## 2013-02-01       NA            NA
    ## 2013-03-01 24412.67      1702.333
    ## 2013-04-01 24757.67      1714.000
    ## 2013-05-01 25222.67      1745.000
    ## 2013-06-01 25539.33      1768.333

## 4.7 지난 달 데이터는?: 필터링(subsetting)

``` r
covid19 |>
  filter(date >= as.Date("2020-10-01") & date <= as.Date("2020-10-10"))
```

    ## # A tibble: 9 × 11
    ##   date       status `0-9세` `10-19세` `20-29세` `30-39세` `40-49세` `50-59세` `60-69세` `70-79세` `80세 이상`
    ##   <date>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 2020-10-01 신규         6         4         7         9        14        13        13         7           4
    ## 2 2020-10-02 신규         3         6         6        10         6        11        13         4           4
    ## 3 2020-10-03 신규         1         1         6        13        11        11        16        13           3
    ## 4 2020-10-04 신규         0         4         4         9        13         8        12         8           6
    ## 5 2020-10-05 신규         0         3        17         9         8        12        14         6           4
    ## 6 2020-10-06 신규         5         8        26         8         2        11         9         4           2
    ## 7 2020-10-07 신규         1         2         8        13        10        28        31        16           5
    ## 8 2020-10-08 신규         6         5         6         7         9        12        11        10           3
    ## 9 2020-10-10 신규         0         0         0         0         0         0         0         0           0

``` r
covid19 |>
  filter(between(date, as.Date("2021-01-01"), as.Date("2021-01-15")))
```

    ## # A tibble: 15 × 11
    ##    date       status `0-9세` `10-19세` `20-29세` `30-39세` `40-49세` `50-59세` `60-69세` `70-79세` `80세 이상`
    ##    <date>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <dbl>
    ##  1 2021-01-01 신규        41        74       131       145       154       204       159        80          41
    ##  2 2021-01-02 신규        39        66       111       109       106       160       125        59          49
    ##  3 2021-01-03 신규        32        46        80        75        88       124       117        51          38
    ##  4 2021-01-04 신규        40        71       120       127       172       187       153        69          81
    ##  5 2021-01-05 신규        45        44       107       105       112       124       108        44          26
    ##  6 2021-01-06 신규        45        60       106       106       110       140       148        73          51
    ##  7 2021-01-07 신규        41        54       123       120       155       176       113        59          27
    ##  8 2021-01-08 신규        34        43        91        86        83       124       125        49          37
    ##  9 2021-01-09 신규        32        50       100        91        89       124        80        50          25
    ## 10 2021-01-10 신규        36        56        87        79        94       143        87        49          34
    ## 11 2021-01-11 신규        19        36        47        54        61        79        79        44          31
    ## 12 2021-01-12 신규        32        34        71        59        91       102        74        39          35
    ## 13 2021-01-13 신규        24        28        66        81        83       129        87        47          16
    ## 14 2021-01-14 신규        25        45        60        67        82        98        71        39          29
    ## 15 2021-01-15 신규        42        37        62        70        81        97        91        22          11

``` r
employees |>
  filter(year(time) == 2019 & month(time) == 5)
```

    ##         time total employees.edu
    ## 1 2019-05-01 27322          1884

``` r
covid19 |>
  filter(between(day(date), 3, 7))
```

    ## # A tibble: 50 × 11
    ##    date       status `0-9세` `10-19세` `20-29세` `30-39세` `40-49세` `50-59세` `60-69세` `70-79세` `80세 이상`
    ##    <date>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <dbl>
    ##  1 2020-05-03 신규         0         1         2         3         2         0         2         1           2
    ##  2 2020-05-04 신규         0         0         2         1         3         0         2         0           0
    ##  3 2020-05-05 신규         0         0         0         1         1         1         0         0           0
    ##  4 2020-05-06 신규         0         0         0         1         0         0         1         0           0
    ##  5 2020-05-07 신규         0         1         2         1         0         0         0         0           0
    ##  6 2020-06-03 신규         1         2         5         6         5         9        15         6           0
    ##  7 2020-06-04 신규         1         0         5         3         3         9         9         6           3
    ##  8 2020-06-05 신규         1         2         5         2         3         7        10         7           2
    ##  9 2020-06-06 신규         3         1         2         6        11         8         9         8           3
    ## 10 2020-06-07 신규         1         6         3         6         8        11        12         8           2
    ## # … with 40 more rows

``` r
covid19 |>
  filter_by_time(
    .date_var = date,
    .start = "2020-10-01",
    .end = "2020-10-05"
  )
```

    ## # A tibble: 5 × 11
    ##   date       status `0-9세` `10-19세` `20-29세` `30-39세` `40-49세` `50-59세` `60-69세` `70-79세` `80세 이상`
    ##   <date>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 2020-10-01 신규         6         4         7         9        14        13        13         7           4
    ## 2 2020-10-02 신규         3         6         6        10         6        11        13         4           4
    ## 3 2020-10-03 신규         1         1         6        13        11        11        16        13           3
    ## 4 2020-10-04 신규         0         4         4         9        13         8        12         8           6
    ## 5 2020-10-05 신규         0         3        17         9         8        12        14         6           4

``` r
covid19 |>
  filter(`0-9세` != 0) |>
  filter_period(
    .date_var = date,
    .period = "1 month",
    `0-9세` == max(`0-9세`)
  )
```

    ## # A tibble: 17 × 11
    ##    date       status `0-9세` `10-19세` `20-29세` `30-39세` `40-49세` `50-59세` `60-69세` `70-79세` `80세 이상`
    ##    <date>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <dbl>
    ##  1 2020-04-16 신규         3         3         5         1         2         3         2         1           2
    ##  2 2020-05-27 신규         3         2         8         6         3         6         8         3           1
    ##  3 2020-06-06 신규         3         1         2         6        11         8         9         8           3
    ##  4 2020-06-23 신규         3         0         4         9         9         9         7         4           1
    ##  5 2020-06-29 신규         3         1         9         5         6         5        12         1           0
    ##  6 2020-06-30 신규         3         4        10         6         8         6         3         3           0
    ##  7 2020-07-17 신규        10         4        26        37        32        23        19         6           3
    ##  8 2020-08-23 신규        16        35        34        40        62        82        87        27          14
    ##  9 2020-08-28 신규        16        17        32        48        53        73        64        48          20
    ## 10 2020-08-29 신규        16        18        31        36        47        54        67        36          18
    ## 11 2020-09-01 신규        14        17        25        24        25        45        45        30          10
    ## 12 2020-10-29 신규        11         8        13        23        14        17        25        10           4
    ## 13 2020-11-23 신규        23        21        41        41        49        50        20        17           9
    ## 14 2020-12-17 신규        56        54       107       122       141       214       173        85          59
    ## 15 2021-01-05 신규        45        44       107       105       112       124       108        44          26
    ## 16 2021-01-06 신규        45        60       106       106       110       140       148        73          51
    ## 17 2021-02-05 신규        30        28        30        60        42        65        75        25          14

``` r
covid19.xts["2020-10-02"]
```

    ##            0-9세 10-19세 20-29세 30-39세 40-49세 50-59세 60-69세 70-79세 80세 이상
    ## 2020-10-02     3       6       6      10       6      11      13       4         4

``` r
covid19.xts["2020-10-01/2020-10-10"]
```

    ##            0-9세 10-19세 20-29세 30-39세 40-49세 50-59세 60-69세 70-79세 80세 이상
    ## 2020-10-01     6       4       7       9      14      13      13       7         4
    ## 2020-10-02     3       6       6      10       6      11      13       4         4
    ## 2020-10-03     1       1       6      13      11      11      16      13         3
    ## 2020-10-04     0       4       4       9      13       8      12       8         6
    ## 2020-10-05     0       3      17       9       8      12      14       6         4
    ## 2020-10-06     5       8      26       8       2      11       9       4         2
    ## 2020-10-07     1       2       8      13      10      28      31      16         5
    ## 2020-10-08     6       5       6       7       9      12      11      10         3
    ## 2020-10-10     0       0       0       0       0       0       0       0         0

``` r
covid19.xts["2021-02-05/"]
```

    ##            0-9세 10-19세 20-29세 30-39세 40-49세 50-59세 60-69세 70-79세 80세 이상
    ## 2021-02-05    30      28      30      60      42      65      75      25        14
    ## 2021-02-06    25      31      53      41      60      77      63      30        13
    ## 2021-02-07    21      27      51      47      44      79      66      26        11
    ## 2021-02-08    19       9      35      31      53      52      59      22         9
    ## 2021-02-09    15      NA      33      41      52      59      38      28        11

``` r
covid19.xts["/2020-04-11"]
```

    ##            0-9세 10-19세 20-29세 30-39세 40-49세 50-59세 60-69세 70-79세 80세 이상
    ## 2020-04-09     2       4      12       7       7       2       2       0         3
    ## 2020-04-10     1       1       7       4       2       3       6       2         1
    ## 2020-04-11     1       5       5       2       3       6       7       0         1

## 4.8 월별, 분기별, 연별 증감량

``` r
cbind(
  연도 = students$연도,
  학생수계 = students$학생수계,
  전년 = students |>
    lag(1) |>
    select(학생수계) |>
    rename(전년 = 학생수계)
)
```

    ##          연도 학생수계    전년
    ## 1  1999-01-01  8658358      NA
    ## 2  2000-01-01  8535867 8658358
    ## 3  2001-01-01  8414423 8535867
    ## 4  2002-01-01  8361933 8414423
    ## 5  2003-01-01  8379775 8361933
    ## 6  2004-01-01  8371630 8379775
    ## 7  2005-01-01  8371421 8371630
    ## 8  2006-01-01  8354891 8371421
    ## 9  2007-01-01  8309932 8354891
    ## 10 2008-01-01  8187782 8309932
    ## 11 2009-01-01  8016924 8187782
    ## 12 2010-01-01  7807663 8016924
    ## 13 2011-01-01  7586266 7807663
    ## 14 2012-01-01  7370308 7586266
    ## 15 2013-01-01  7173904 7370308
    ## 16 2014-01-01  6973154 7173904
    ## 17 2015-01-01  6806411 6973154
    ## 18 2016-01-01  6621547 6806411
    ## 19 2017-01-01  6454281 6621547
    ## 20 2018-01-01  6295366 6454281
    ## 21 2019-01-01  6122198 6295366
    ## 22 2020-01-01  5995239 6122198

``` r
students_lag <-
  students |>
  select(연도, 학생수계) |>
  mutate(전년 = lag(학생수계, 1)) |>
  mutate(
    증감 = 학생수계 - 전년,
    증감율 = round((학생수계 / 전년) - 1, 3) * 100
  )
students_lag
```

    ## # A tibble: 22 × 5
    ##    연도       학생수계    전년    증감 증감율
    ##    <date>        <dbl>   <dbl>   <dbl>  <dbl>
    ##  1 1999-01-01  8658358      NA      NA   NA  
    ##  2 2000-01-01  8535867 8658358 -122491   -1.4
    ##  3 2001-01-01  8414423 8535867 -121444   -1.4
    ##  4 2002-01-01  8361933 8414423  -52490   -0.6
    ##  5 2003-01-01  8379775 8361933   17842    0.2
    ##  6 2004-01-01  8371630 8379775   -8145   -0.1
    ##  7 2005-01-01  8371421 8371630    -209    0  
    ##  8 2006-01-01  8354891 8371421  -16530   -0.2
    ##  9 2007-01-01  8309932 8354891  -44959   -0.5
    ## 10 2008-01-01  8187782 8309932 -122150   -1.5
    ## # … with 12 more rows

``` r
students_lag |>
  ggplot(aes(as.factor(year(연도)), 증감)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = scales::comma(증감)), vjust = 1, size = 3) +
  labs(
    title = "전년 대비 전체 학생수 증감 추이",
    x = "연도",
    y = "학생수 증감량"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

    ## Warning: Removed 1 rows containing missing values (geom_point).

    ## Warning: Removed 1 rows containing missing values (geom_text_repel).

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
students.tsibble |>
  select(1, 2) |>
  mutate(증감 = difference(학생수계, lag = 1)) |>
  mutate(증감율 = round(증감 / 학생수계, 3) * 100)
```

    ## # A tsibble: 22 x 4 [1D]
    ##    연도       학생수계    증감 증감율
    ##    <date>        <dbl>   <dbl>  <dbl>
    ##  1 1999-01-01  8658358      NA   NA  
    ##  2 2000-01-01  8535867 -122491   -1.4
    ##  3 2001-01-01  8414423 -121444   -1.4
    ##  4 2002-01-01  8361933  -52490   -0.6
    ##  5 2003-01-01  8379775   17842    0.2
    ##  6 2004-01-01  8371630   -8145   -0.1
    ##  7 2005-01-01  8371421    -209    0  
    ##  8 2006-01-01  8354891  -16530   -0.2
    ##  9 2007-01-01  8309932  -44959   -0.5
    ## 10 2008-01-01  8187782 -122150   -1.5
    ## # … with 12 more rows

``` r
employees |>
  mutate(증감 = difference(total, lag = 1)) |>
  mutate(증감율 = round((증감 / total), 3) * 100) |>
  select(1, 2, 4, 5)
```

    ##          time total 증감 증감율
    ## 1  2013-01-01 24287   NA     NA
    ## 2  2013-02-01 24215  -72   -0.3
    ## 3  2013-03-01 24736  521    2.1
    ## 4  2013-04-01 25322  586    2.3
    ## 5  2013-05-01 25610  288    1.1
    ## 6  2013-06-01 25686   76    0.3
    ## 7  2013-07-01 25681   -5    0.0
    ## 8  2013-08-01 25513 -168   -0.7
    ## 9  2013-09-01 25701  188    0.7
    ## 10 2013-10-01 25798   97    0.4
    ## 11 2013-11-01 25795   -3    0.0
    ## 12 2013-12-01 25248 -547   -2.2
    ## 13 2014-01-01 25050 -198   -0.8
    ## 14 2014-02-01 25116   66    0.3
    ## 15 2014-03-01 25463  347    1.4
    ## 16 2014-04-01 25985  522    2.0
    ## 17 2014-05-01 26112  127    0.5
    ## 18 2014-06-01 26178   66    0.3
    ## 19 2014-07-01 26280  102    0.4
    ## 20 2014-08-01 26183  -97   -0.4
    ## 21 2014-09-01 26213   30    0.1
    ## 22 2014-10-01 26247   34    0.1
    ## 23 2014-11-01 26260   13    0.0
    ## 24 2014-12-01 25679 -581   -2.3
    ## 25 2015-01-01 25392 -287   -1.1
    ## 26 2015-02-01 25471   79    0.3
    ## 27 2015-03-01 25766  295    1.1
    ## 28 2015-04-01 26153  387    1.5
    ## 29 2015-05-01 26431  278    1.1
    ## 30 2015-06-01 26440    9    0.0
    ## 31 2015-07-01 26538   98    0.4
    ## 32 2015-08-01 26369 -169   -0.6
    ## 33 2015-09-01 26487  118    0.4
    ## 34 2015-10-01 26519   32    0.1
    ## 35 2015-11-01 26469  -50   -0.2
    ## 36 2015-12-01 26098 -371   -1.4
    ## 37 2016-01-01 25646 -452   -1.8
    ## 38 2016-02-01 25615  -31   -0.1
    ## 39 2016-03-01 25980  365    1.4
    ## 40 2016-04-01 26325  345    1.3
    ## 41 2016-05-01 26613  288    1.1
    ## 42 2016-06-01 26718  105    0.4
    ## 43 2016-07-01 26765   47    0.2
    ## 44 2016-08-01 26696  -69   -0.3
    ## 45 2016-09-01 26697    1    0.0
    ## 46 2016-10-01 26746   49    0.2
    ## 47 2016-11-01 26762   16    0.1
    ## 48 2016-12-01 26347 -415   -1.6
    ## 49 2017-01-01 25878 -469   -1.8
    ## 50 2017-02-01 25979  101    0.4
    ## 51 2017-03-01 26443  464    1.8
    ## 52 2017-04-01 26744  301    1.1
    ## 53 2017-05-01 26992  248    0.9
    ## 54 2017-06-01 27020   28    0.1
    ## 55 2017-07-01 27078   58    0.2
    ## 56 2017-08-01 26904 -174   -0.6
    ## 57 2017-09-01 27011  107    0.4
    ## 58 2017-10-01 27026   15    0.1
    ## 59 2017-11-01 27019   -7    0.0
    ## 60 2017-12-01 26604 -415   -1.6
    ## 61 2018-01-01 26213 -391   -1.5
    ## 62 2018-02-01 26083 -130   -0.5
    ## 63 2018-03-01 26555  472    1.8
    ## 64 2018-04-01 26868  313    1.2
    ## 65 2018-05-01 27064  196    0.7
    ## 66 2018-06-01 27126   62    0.2
    ## 67 2018-07-01 27083  -43   -0.2
    ## 68 2018-08-01 26907 -176   -0.7
    ## 69 2018-09-01 27055  148    0.5
    ## 70 2018-10-01 27090   35    0.1
    ## 71 2018-11-01 27184   94    0.3
    ## 72 2018-12-01 26638 -546   -2.0
    ## 73 2019-01-01 26232 -406   -1.5
    ## 74 2019-02-01 26346  114    0.4
    ## 75 2019-03-01 26805  459    1.7
    ## 76 2019-04-01 27038  233    0.9
    ## 77 2019-05-01 27322  284    1.0
    ## 78 2019-06-01 27408   86    0.3
    ## 79 2019-07-01 27383  -25   -0.1
    ## 80 2019-08-01 27358  -25   -0.1
    ## 81 2019-09-01 27404   46    0.2
    ## 82 2019-10-01 27509  105    0.4
    ## 83 2019-11-01 27515    6    0.0
    ## 84 2019-12-01 27154 -361   -1.3
    ## 85 2020-01-01 26800 -354   -1.3
    ## 86 2020-02-01 26838   38    0.1
    ## 87 2020-03-01 26609 -229   -0.9
    ## 88 2020-04-01 26562  -47   -0.2
    ## 89 2020-05-01 26930  368    1.4
    ## 90 2020-06-01 27055  125    0.5
    ## 91 2020-07-01 27106   51    0.2
    ## 92 2020-08-01 27085  -21   -0.1
    ## 93 2020-09-01 27012  -73   -0.3
    ## 94 2020-10-01 27088   76    0.3
    ## 95 2020-11-01 27241  153    0.6
    ## 96 2020-12-01 26526 -715   -2.7

``` r
students.xts$증감 <- diff(students.xts[, 2])
students.xts$증감률 <- round((students.xts$증감 / students.xts$학생수계), 3) * 100
students.xts[, c("유치원", "증감", "증감률")]
```

    ##            유치원   증감 증감률
    ## 1999-01-01 534166     NA     NA
    ## 2000-01-01 545263  11097    0.1
    ## 2001-01-01 545142   -121    0.0
    ## 2002-01-01 550256   5114    0.1
    ## 2003-01-01 546531  -3725    0.0
    ## 2004-01-01 541713  -4818   -0.1
    ## 2005-01-01 541603   -110    0.0
    ## 2006-01-01 545812   4209    0.1
    ## 2007-01-01 541550  -4262   -0.1
    ## 2008-01-01 537822  -3728    0.0
    ## 2009-01-01 537361   -461    0.0
    ## 2010-01-01 538587   1226    0.0
    ## 2011-01-01 564834  26247    0.3
    ## 2012-01-01 613749  48915    0.7
    ## 2013-01-01 658188  44439    0.6
    ## 2014-01-01 652546  -5642   -0.1
    ## 2015-01-01 682553  30007    0.4
    ## 2016-01-01 704138  21585    0.3
    ## 2017-01-01 694631  -9507   -0.1
    ## 2018-01-01 675998 -18633   -0.3
    ## 2019-01-01 633913 -42085   -0.7
    ## 2020-01-01 612538 -21375   -0.4

``` r
par(family = "NanumGothic")
plot.xts(students.xts[, "증감률"],
  main = "전년 대비 유치원 학생수 증감률"
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
employees.xts$증감 <- diff(employees.xts$total)
employees.xts$증감률 <- round((employees.xts$증감 / employees.xts$total), 3) * 100
employees.xts[, c("total", "증감", "증감률")] |>
  head(10)
```

    ##            total 증감 증감률
    ## 2013-01-01 24287   NA     NA
    ## 2013-02-01 24215  -72   -0.3
    ## 2013-03-01 24736  521    2.1
    ## 2013-04-01 25322  586    2.3
    ## 2013-05-01 25610  288    1.1
    ## 2013-06-01 25686   76    0.3
    ## 2013-07-01 25681   -5    0.0
    ## 2013-08-01 25513 -168   -0.7
    ## 2013-09-01 25701  188    0.7
    ## 2013-10-01 25798   97    0.4

``` r
par(family = "NanumGothic")
plot.xts(employees.xts[, c("증감률")], main = "전월 대비 전체 취업자 증감률")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

## 4.9 월 비중 백분율, 연 비중 백분율

``` r
employees |>
  group_by(year(time)) |>
  mutate(sum.by.year = sum(total)) |>
  ungroup() |>
  mutate(rate.by.year = round(total / sum.by.year, 3) * 100)
```

    ## # A tibble: 96 × 6
    ##    time       total employees.edu `year(time)` sum.by.year rate.by.year
    ##    <date>     <int>         <int>        <dbl>       <int>        <dbl>
    ##  1 2013-01-01 24287          1710         2013      303592          8  
    ##  2 2013-02-01 24215          1681         2013      303592          8  
    ##  3 2013-03-01 24736          1716         2013      303592          8.1
    ##  4 2013-04-01 25322          1745         2013      303592          8.3
    ##  5 2013-05-01 25610          1774         2013      303592          8.4
    ##  6 2013-06-01 25686          1786         2013      303592          8.5
    ##  7 2013-07-01 25681          1813         2013      303592          8.5
    ##  8 2013-08-01 25513          1811         2013      303592          8.4
    ##  9 2013-09-01 25701          1794         2013      303592          8.5
    ## 10 2013-10-01 25798          1790         2013      303592          8.5
    ## # … with 86 more rows

``` r
covid19 |>
  group_by(yearmonth(date)) |>
  mutate(sum.by.month = sum(`0-9세`)) |>
  ungroup() |>
  mutate(rate.by.month = round(`0-9세` / sum.by.month, 3) * 100) |>
  select(date, `0-9세`, sum.by.month, rate.by.month)
```

    ## # A tibble: 302 × 4
    ##    date       `0-9세` sum.by.month rate.by.month
    ##    <date>       <dbl>        <dbl>         <dbl>
    ##  1 2020-04-09       2           14          14.3
    ##  2 2020-04-10       1           14           7.1
    ##  3 2020-04-11       1           14           7.1
    ##  4 2020-04-12       0           14           0  
    ##  5 2020-04-13       2           14          14.3
    ##  6 2020-04-14       0           14           0  
    ##  7 2020-04-15       0           14           0  
    ##  8 2020-04-16       3           14          21.4
    ##  9 2020-04-17       1           14           7.1
    ## 10 2020-04-18       2           14          14.3
    ## # … with 292 more rows

``` r
covid19 |>
  group_by(year(date), month(date), week(date)) |>
  mutate(sum.by.week = sum(`0-9세`)) |>
  ungroup() |>
  mutate(rate.by.week = round(`0-9세` / sum.by.week, 3) * 100) |>
  select(date, `0-9세`, sum.by.week, rate.by.week)
```

    ## # A tibble: 302 × 4
    ##    date       `0-9세` sum.by.week rate.by.week
    ##    <date>       <dbl>       <dbl>        <dbl>
    ##  1 2020-04-09       2           6         33.3
    ##  2 2020-04-10       1           6         16.7
    ##  3 2020-04-11       1           6         16.7
    ##  4 2020-04-12       0           6          0  
    ##  5 2020-04-13       2           6         33.3
    ##  6 2020-04-14       0           6          0  
    ##  7 2020-04-15       0           6          0  
    ##  8 2020-04-16       3           6         50  
    ##  9 2020-04-17       1           6         16.7
    ## 10 2020-04-18       2           6         33.3
    ## # … with 292 more rows

``` r
employees.tsibble |>
  index_by(yearqtr = ~ yearquarter(.)) |>
  mutate(sum.qtrly = sum(total)) |>
  mutate(rate.qtrly = total / sum.qtrly)
```

    ## # A tsibble: 96 x 6 [1D]
    ## # Groups:    @ yearqtr [32]
    ##    time       total employees.edu yearqtr sum.qtrly rate.qtrly
    ##    <date>     <int>         <int>   <qtr>     <int>      <dbl>
    ##  1 2013-01-01 24287          1710 2013 Q1     73238      0.332
    ##  2 2013-02-01 24215          1681 2013 Q1     73238      0.331
    ##  3 2013-03-01 24736          1716 2013 Q1     73238      0.338
    ##  4 2013-04-01 25322          1745 2013 Q2     76618      0.330
    ##  5 2013-05-01 25610          1774 2013 Q2     76618      0.334
    ##  6 2013-06-01 25686          1786 2013 Q2     76618      0.335
    ##  7 2013-07-01 25681          1813 2013 Q3     76895      0.334
    ##  8 2013-08-01 25513          1811 2013 Q3     76895      0.332
    ##  9 2013-09-01 25701          1794 2013 Q3     76895      0.334
    ## 10 2013-10-01 25798          1790 2013 Q4     76841      0.336
    ## # … with 86 more rows

``` r
employees.tsibble |>
  index_by(year = ~ year(.)) |>
  mutate(sum.year = sum(total)) |>
  mutate(rate.year = (total / sum.year) * 100)
```

    ## # A tsibble: 96 x 6 [1D]
    ## # Groups:    @ year [8]
    ##    time       total employees.edu  year sum.year rate.year
    ##    <date>     <int>         <int> <dbl>    <int>     <dbl>
    ##  1 2013-01-01 24287          1710  2013   303592      8.00
    ##  2 2013-02-01 24215          1681  2013   303592      7.98
    ##  3 2013-03-01 24736          1716  2013   303592      8.15
    ##  4 2013-04-01 25322          1745  2013   303592      8.34
    ##  5 2013-05-01 25610          1774  2013   303592      8.44
    ##  6 2013-06-01 25686          1786  2013   303592      8.46
    ##  7 2013-07-01 25681          1813  2013   303592      8.46
    ##  8 2013-08-01 25513          1811  2013   303592      8.40
    ##  9 2013-09-01 25701          1794  2013   303592      8.47
    ## 10 2013-10-01 25798          1790  2013   303592      8.50
    ## # … with 86 more rows

## 4.10 월별, 분기별, 연별 누적 합계

``` r
employees |>
  mutate(cumsum = cumsum(total)) |>
  select(time, total, cumsum)
```

    ##          time total  cumsum
    ## 1  2013-01-01 24287   24287
    ## 2  2013-02-01 24215   48502
    ## 3  2013-03-01 24736   73238
    ## 4  2013-04-01 25322   98560
    ## 5  2013-05-01 25610  124170
    ## 6  2013-06-01 25686  149856
    ## 7  2013-07-01 25681  175537
    ## 8  2013-08-01 25513  201050
    ## 9  2013-09-01 25701  226751
    ## 10 2013-10-01 25798  252549
    ## 11 2013-11-01 25795  278344
    ## 12 2013-12-01 25248  303592
    ## 13 2014-01-01 25050  328642
    ## 14 2014-02-01 25116  353758
    ## 15 2014-03-01 25463  379221
    ## 16 2014-04-01 25985  405206
    ## 17 2014-05-01 26112  431318
    ## 18 2014-06-01 26178  457496
    ## 19 2014-07-01 26280  483776
    ## 20 2014-08-01 26183  509959
    ## 21 2014-09-01 26213  536172
    ## 22 2014-10-01 26247  562419
    ## 23 2014-11-01 26260  588679
    ## 24 2014-12-01 25679  614358
    ## 25 2015-01-01 25392  639750
    ## 26 2015-02-01 25471  665221
    ## 27 2015-03-01 25766  690987
    ## 28 2015-04-01 26153  717140
    ## 29 2015-05-01 26431  743571
    ## 30 2015-06-01 26440  770011
    ## 31 2015-07-01 26538  796549
    ## 32 2015-08-01 26369  822918
    ## 33 2015-09-01 26487  849405
    ## 34 2015-10-01 26519  875924
    ## 35 2015-11-01 26469  902393
    ## 36 2015-12-01 26098  928491
    ## 37 2016-01-01 25646  954137
    ## 38 2016-02-01 25615  979752
    ## 39 2016-03-01 25980 1005732
    ## 40 2016-04-01 26325 1032057
    ## 41 2016-05-01 26613 1058670
    ## 42 2016-06-01 26718 1085388
    ## 43 2016-07-01 26765 1112153
    ## 44 2016-08-01 26696 1138849
    ## 45 2016-09-01 26697 1165546
    ## 46 2016-10-01 26746 1192292
    ## 47 2016-11-01 26762 1219054
    ## 48 2016-12-01 26347 1245401
    ## 49 2017-01-01 25878 1271279
    ## 50 2017-02-01 25979 1297258
    ## 51 2017-03-01 26443 1323701
    ## 52 2017-04-01 26744 1350445
    ## 53 2017-05-01 26992 1377437
    ## 54 2017-06-01 27020 1404457
    ## 55 2017-07-01 27078 1431535
    ## 56 2017-08-01 26904 1458439
    ## 57 2017-09-01 27011 1485450
    ## 58 2017-10-01 27026 1512476
    ## 59 2017-11-01 27019 1539495
    ## 60 2017-12-01 26604 1566099
    ## 61 2018-01-01 26213 1592312
    ## 62 2018-02-01 26083 1618395
    ## 63 2018-03-01 26555 1644950
    ## 64 2018-04-01 26868 1671818
    ## 65 2018-05-01 27064 1698882
    ## 66 2018-06-01 27126 1726008
    ## 67 2018-07-01 27083 1753091
    ## 68 2018-08-01 26907 1779998
    ## 69 2018-09-01 27055 1807053
    ## 70 2018-10-01 27090 1834143
    ## 71 2018-11-01 27184 1861327
    ## 72 2018-12-01 26638 1887965
    ## 73 2019-01-01 26232 1914197
    ## 74 2019-02-01 26346 1940543
    ## 75 2019-03-01 26805 1967348
    ## 76 2019-04-01 27038 1994386
    ## 77 2019-05-01 27322 2021708
    ## 78 2019-06-01 27408 2049116
    ## 79 2019-07-01 27383 2076499
    ## 80 2019-08-01 27358 2103857
    ## 81 2019-09-01 27404 2131261
    ## 82 2019-10-01 27509 2158770
    ## 83 2019-11-01 27515 2186285
    ## 84 2019-12-01 27154 2213439
    ## 85 2020-01-01 26800 2240239
    ## 86 2020-02-01 26838 2267077
    ## 87 2020-03-01 26609 2293686
    ## 88 2020-04-01 26562 2320248
    ## 89 2020-05-01 26930 2347178
    ## 90 2020-06-01 27055 2374233
    ## 91 2020-07-01 27106 2401339
    ## 92 2020-08-01 27085 2428424
    ## 93 2020-09-01 27012 2455436
    ## 94 2020-10-01 27088 2482524
    ## 95 2020-11-01 27241 2509765
    ## 96 2020-12-01 26526 2536291

``` r
covid19 |>
  mutate(cumsum = cumsum(`0-9세`)) |>
  select(date, `0-9세`, cumsum) |>
  ggplot(aes(date, cumsum)) +
  geom_line(aes(group = 1)) +
  labs(title = "코로나 확진자 누적 합계(0-9세)", x = "날짜", y = "누적합계") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y.%m") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

``` r
employees |>
  group_by(year(time)) |>
  mutate(
    cumsum.total = cumsum(total),
    cumsum.edu = cumsum(employees.edu)
  ) |>
  select(time, total, cumsum.total, employees.edu, cumsum.edu)
```

    ## Adding missing grouping variables: `year(time)`

    ## # A tibble: 96 × 6
    ## # Groups:   year(time) [8]
    ##    `year(time)` time       total cumsum.total employees.edu cumsum.edu
    ##           <dbl> <date>     <int>        <int>         <int>      <int>
    ##  1         2013 2013-01-01 24287        24287          1710       1710
    ##  2         2013 2013-02-01 24215        48502          1681       3391
    ##  3         2013 2013-03-01 24736        73238          1716       5107
    ##  4         2013 2013-04-01 25322        98560          1745       6852
    ##  5         2013 2013-05-01 25610       124170          1774       8626
    ##  6         2013 2013-06-01 25686       149856          1786      10412
    ##  7         2013 2013-07-01 25681       175537          1813      12225
    ##  8         2013 2013-08-01 25513       201050          1811      14036
    ##  9         2013 2013-09-01 25701       226751          1794      15830
    ## 10         2013 2013-10-01 25798       252549          1790      17620
    ## # … with 86 more rows

``` r
employees.tsibble |>
  index_by(yearqtr = ~ yearquarter(.)) |>
  mutate(cumsum.qtrly = cumsum(total)) |>
  select(yearqtr, cumsum.qtrly)
```

    ## # A tsibble: 96 x 3 [1D]
    ## # Groups:    @ yearqtr [32]
    ##    yearqtr cumsum.qtrly time      
    ##      <qtr>        <int> <date>    
    ##  1 2013 Q1        24287 2013-01-01
    ##  2 2013 Q1        48502 2013-02-01
    ##  3 2013 Q1        73238 2013-03-01
    ##  4 2013 Q2        25322 2013-04-01
    ##  5 2013 Q2        50932 2013-05-01
    ##  6 2013 Q2        76618 2013-06-01
    ##  7 2013 Q3        25681 2013-07-01
    ##  8 2013 Q3        51194 2013-08-01
    ##  9 2013 Q3        76895 2013-09-01
    ## 10 2013 Q4        25798 2013-10-01
    ## # … with 86 more rows

``` r
covid19.tsibble[, c(1, 3)] |>
  index_by(yearweek = ~ yearweek(.)) |>
  mutate(cumsum.weekly = cumsum(`0-9세`))
```

    ## # A tsibble: 302 x 4 [1D]
    ## # Groups:    @ yearweek [45]
    ##    date       `0-9세` yearweek cumsum.weekly
    ##    <date>       <dbl>   <week>         <dbl>
    ##  1 2020-04-09       2 2020 W15             2
    ##  2 2020-04-10       1 2020 W15             3
    ##  3 2020-04-11       1 2020 W15             4
    ##  4 2020-04-12       0 2020 W15             4
    ##  5 2020-04-13       2 2020 W16             2
    ##  6 2020-04-14       0 2020 W16             2
    ##  7 2020-04-15       0 2020 W16             2
    ##  8 2020-04-16       3 2020 W16             5
    ##  9 2020-04-17       1 2020 W16             6
    ## 10 2020-04-18       2 2020 W16             8
    ## # … with 292 more rows

``` r
do.call(rbind, lapply(split(employees.xts, f = "year"), cumsum)) |>
  head(15)
```

    ##             total employees.edu 증감 증감률
    ## 2013-01-01  24287          1710   NA     NA
    ## 2013-02-01  48502          3391   NA     NA
    ## 2013-03-01  73238          5107   NA     NA
    ## 2013-04-01  98560          6852   NA     NA
    ## 2013-05-01 124170          8626   NA     NA
    ## 2013-06-01 149856         10412   NA     NA
    ## 2013-07-01 175537         12225   NA     NA
    ## 2013-08-01 201050         14036   NA     NA
    ## 2013-09-01 226751         15830   NA     NA
    ## 2013-10-01 252549         17620   NA     NA
    ## 2013-11-01 278344         19413   NA     NA
    ## 2013-12-01 303592         21192   NA     NA
    ## 2014-01-01  25050          1748 -198   -0.8
    ## 2014-02-01  50166          3534 -132   -0.5
    ## 2014-03-01  75629          5348  215    0.9

## 4.11 동월별, 동 분기별, 동년별 플롯

``` r
employees |>
  mutate(year = year(time)) |>
  ggplot(aes(as.factor(year), total)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  labs(title = "동년별 취업자 분포", x = "연도", y = "취업자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-79-1.png)<!-- -->

``` r
employees |>
  mutate(month = month(time)) |>
  ggplot(aes(as.factor(month), total)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  labs(title = "동월별 취업자 분포", x = "연도", y = "취업자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-80-1.png)<!-- -->

``` r
employees |>
  mutate(quarter = quarter(time)) |>
  ggplot(aes(as.factor(quarter), total)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  labs(title = "동분기별 취업자 분포", x = "연도", y = "취업자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-81-1.png)<!-- -->

``` r
covid19 |>
  mutate(month = month(date)) |>
  ggplot(aes(as.factor(month), `0-9세`)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  labs(title = "동월별 확진자 분포", x = "월", y = "확진자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

``` r
covid19 |>
  mutate(wday = wday(date, label = TRUE)) |>
  ggplot(aes(wday, `50-59세`)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  labs(title = "동요일별 확진자 분포", x = "요일", y = "확진자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

``` r
employees |>
  timetk::plot_seasonal_diagnostics(
    .date_var = time,
    .value = total,
    .title = "전체 취업자의 주기별 플롯"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

``` r
covid19 |>
  timetk::plot_seasonal_diagnostics(
    .date_var = date,
    .value = `0-9세`,
    .title = "코로나  확진자(0-9)세의 주기별 플롯"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note04_files/figure-gfm/unnamed-chunk-85-1.png)<!-- -->
