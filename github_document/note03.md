note03
================

``` r
load("./data-tidied/dataset.rdata")
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 3 시계열 시각화

## 3.1 data.frame: ggplot2 패키지

``` r
library(ggplot2)
```

``` r
students |>
  ggplot(aes(연도, 학생수계)) +
  geom_line() +
  labs(title = "연도별 학생수 추이")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
students |>
  ggplot(aes(as.factor(lubridate::year(연도)), 학생수계)) +
  geom_line(aes(group = 1)) +
  labs(title = "연도별 학생수 추이", x = "연도") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(students.all, aes(x = 연도, y = 학생수계)) +
  geom_line(aes(group = 지역규모, linetype = 지역규모)) +
  labs(title = "연도별 학생수 추이", x = "연도") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(students.all, aes(x = 연도, y = 학생수계)) +
  geom_line(aes(group = 지역규모, color = 지역규모)) +
  geom_point(shape = "circle", size = 0.5) +
  labs(title = "연도별 학생수 추이", x = "연도") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
students |>
  ggplot(aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = "circle", size = 0.5) +
  geom_text(aes(label = scales::number(학생수계, big.mark = ",")), size = 2, vjust = 1.5) +
  labs(title = "연도별 학생수 추이", x = "연도") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
library(ggrepel)
students |>
  ggplot(aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = "circle", size = 0.5) +
  geom_text_repel(aes(label = scales::number(학생수계, big.mark = ",")), size = 2, vjust = 1.5) +
  labs(title = "연도별 학생수 추이", x = "연도") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
students |>
  ggplot(aes(x = as.factor(lubridate::year(연도)), y = 학생수계)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = "circle", size = 0.5) +
  geom_text_repel(aes(label = scales::number(학생수계, big.mark = ",")), size = 2, vjust = 1.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(title = "연도별 학생수 추이", x = "연도") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
employees |>
  ggplot(aes(time, total)) +
  geom_line() +
  geom_point() +
  labs(title = "월별 신규 취업자수", x = "기간", y = "취업자수") +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  scale_x_date(breaks = "6 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
covid19 |>
  ggplot(aes(date, `0-9세`)) +
  geom_line(aes(group = 1)) +
  geom_point(shape = "circle") +
  labs(title = "일별 코로나 확진자수(0-9세)", x = "일자", y = "확진자수") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_date(breaks = "15 days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## 3.2 xts: xts 패키지

``` r
library(xts)
par(family = "NanumGothic")
plot.xts(employees.xts$total,
  main = "월별 취업자수 추이",
  xlab = "월, 연",
  ylab = "취업자수"
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
par(family = "NanumGothic")
plot.xts(employees.xts,
  main = "월별 취업자수 추이",
  xlab = "월, 연",
  ylab = "취업자수",
  yaxis.left = FALSE
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
par(family = "NanumGothic")
plot.xts(employees.xts,
  main = "월별 취업자수 추이",
  xlab = "월, 연",
  ylab = "취업자수",
  yaxis.left = FALSE
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
addLegend("bottomleft",
  ncol = 1, bg = "white",
  lty = c(rep(1, 12)), lwd = c(rep(2, 12)),
  bty = "o"
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
par(family = "NanumGothic")
plot.xts(students.xts$초등학교,
  main = "연도별 학생수 추세", xlab = "연", ylab = "학생수",
  yaxis.right = FALSE, ylim = c(0, max(students.xts$초등학교)), col = "black"
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
lines(students.xts$유치원, lty = 2, col = "red")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
lines(students.xts$중학교, lty = 3, col = "blue")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
addLegend("topright",
  ncol = 1,
  legend.names = c("초등학교", "유치원", "중학교"),
  col = c("black", "red", "blue"),
  lty = c(1, 2, 3),
  bg = "white",
  bty = "o"
)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

``` r
par(family = "NanumGothic")
covid19.xts |>
  plot.xts(main = "일별 확진자수", xlab = "날짜", ylab = "확진자수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
addLegend("topleft", lty = 1, bg = "white", bty = "o", ncol = 2)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

## 3.3 ts: ts:forecast 패키지

``` r
library(forecast)
```

``` r
students.ts[, -1] |>
  autoplot(main = "연도별 학생수", xlab = "연도", ylab = "학생수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
students.ts[, 4] |>
  autoplot(
    series = "초등학교", main = "연도별 학생수",
    xlab = "연도", ylab = "학생수", lty = 1
  ) +
  autolayer(students.ts[, 3], series = "유치원", lty = 2) +
  autolayer(students.ts[, 5], series = "중학교", lty = 3) +
  labs(color = "학교급")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
students.ts[, 3:5] |>
  autoplot(main = "연도별 학생수", xlab = "연도", ylab = "학생수", facet = TRUE)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
students.ts[, 2] |>
  autoplot(
    series = "유치원", lty = 1, lwd = 1,
    main = "연도별 학생수", xlab = "연도", ylab = "학생수"
  ) +
  autolayer(students.ts[, 3], series = "초등학교", lty = 2, lwd = 1.2) +
  autolayer(students.ts[, 4], series = "중학교", lty = 3, lwd = 1.4) +
  autolayer(students.ts[, 5], series = "고등학교", lty = 4, lwd = 1.6) +
  scale_y_continuous(labels = scales::label_comma())
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
employees.ts[, 2] |>
  autoplot(
    series = "전체 취업자", lty = 1, lwd = 1,
    main = "월별 취업자수", xlab = "연도", ylab = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
covid19.ts[, 2] |>
  autoplot(
    series = "확진자", lty = 1, lwd = 1,
    main = "일별 확진자수(0-9세)", xlab = "날짜", ylab = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

## 3.4 tsibble: feasts 패키지

``` r
# install.packages("feasts")
library(feasts)
```

``` r
students.tsibble |>
  autoplot(학생수계) +
  labs(title = "연도별 학생수", x = "연도", y = "학생수")
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
students.tsibble |>
  dplyr::select(1, 3, 4, 5) |>
  tidyr::pivot_longer(2:4, names_to = "category") |>
  autoplot(value)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
students.tsibble |>
  ggplot(aes(x = 연도)) +
  geom_line(aes(y = 초등학교, group = 1, linetype = "초등학교")) +
  geom_line(aes(y = 유치원, group = 1, linetype = "유치원")) +
  geom_line(aes(y = 중학교, group = 1, linetype = "중학교")) +
  labs(title = "연도별 학생수", x = "연도", y = "학생수", color = "학교급") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_linetype_manual(values = c("초등학교" = 1, "유치원" = 2, "중학교" = 3))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
employees.tsibble |>
  mutate(time = tsibble::yearmonth(employees.tsibble$time)) |>
  gg_season(total)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# install.packages("tsibbledata")
tsibbledata::aus_retail %>%
  filter(
    State == "Victoria",
    Industry == "Cafes, restaurants and catering services"
  ) %>%
  gg_season(Turnover)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
employees.tsibble |>
  mutate(time = tsibble::yearmonth(time)) |>
  gg_subseries(total)
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

## 3.5 data.frame: timetk 패키지

``` r
# install.packages("timetk")
library(timetk)
```

``` r
students |>
  plot_time_series(
    .date_var = 연도,
    .value = 학생수계,
    .smooth = TRUE,
    .line_type = 2,
    .smooth_size = 0.5,
    .title = "timetk를 사용한 전체 학생수 플롯",
    .x_lab = "연도",
    .y_lab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
students.all |>
  plot_time_series(
    .date_var = 연도,
    .value = 학생수계,
    .color_var = 지역규모,
    .smooth = FALSE,
    .title = "timetk를 사용한 전체 학생수 다변량 플롯",
    .x_lab = "연도",
    .y_lab = "학생수",
    .interactive = FALSE
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
students |>
  dplyr::select(1, 3, 4, 5) |>
  tidyr::pivot_longer(2:4, names_to = "category") |>
  plot_time_series(
    .date_var = 연도,
    .value = value,
    .color_var = category,
    .smooth = FALSE,
    .title = "timetk를 사용한 전체 학생수 플롯",
    .x_lab = "연도",
    .y_lab = "학생수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
employees |>
  plot_time_series(
    .date_var = time,
    .value = total,
    .smooth = FALSE,
    .title = "월별 신규 취업자수",
    .x_lab = "연도",
    .y_lab = "취업자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
covid19 |>
  plot_time_series(
    .date_var = date,
    .value = `0-9세`,
    .smooth = FALSE,
    .title = "일별 코로나 확진자수(0-9세)",
    .x_lab = "연월",
    .y_lab = "확진자수"
  )
```

![](/Users/gglee/github/note/note-practical-time-series-data-processing-and-analysis-2021/github_document/note03_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->
