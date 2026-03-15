---
title: "Homework2"
output:
  pdf_document:
    latex_engine: xelatex
---



``` r
options(repos = c(CRAN = "https://cran.rstudio.com"))

install.packages(c(
  "tidyverse",
  "ggplot2",
  "earth",
  "mgcv",
  "pdp",
  "patchwork",
  "caret"
))
```

```
## 
## The downloaded binary packages are in
## 	/var/folders/1b/jh6d0q2j2cv00rrmxxnzy58m0000gn/T//RtmpWaDXn3/downloaded_packages
```



## Point 1. Read in the data and partition into training and test sets



``` r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

Libraries to be used in this:

``` r
library(tidyverse)
library(ggplot2)
library(earth)
library(mgcv)
library(pdp)
library(patchwork)
library(caret)
```

reading in data:

``` r
setwd("/Users/karolinawiesiolek/Desktop/Mailman Spring 2026/DSII/HW2")

college <- read.csv("College.csv", stringsAsFactors = FALSE)

dim(college)
```

```
## [1] 565  18
```

``` r
names(college)
```

```
##  [1] "College"     "Apps"        "Accept"      "Enroll"      "Top10perc"  
##  [6] "Top25perc"   "F.Undergrad" "P.Undergrad" "Outstate"    "Room.Board" 
## [11] "Books"       "Personal"    "PhD"         "Terminal"    "S.F.Ratio"  
## [16] "perc.alumni" "Expend"      "Grad.Rate"
```

``` r
head(college)
```

```
##                        College Apps Accept Enroll Top10perc Top25perc
## 1 Abilene Christian University 1660   1232    721        23        52
## 2           Adelphi University 2186   1924    512        16        29
## 3               Adrian College 1428   1097    336        22        50
## 4          Agnes Scott College  417    349    137        60        89
## 5    Alaska Pacific University  193    146     55        16        44
## 6            Albertson College  587    479    158        38        62
##   F.Undergrad P.Undergrad Outstate Room.Board Books Personal PhD Terminal
## 1        2885         537     7440       3300   450     2200  70       78
## 2        2683        1227    12280       6450   750     1500  29       30
## 3        1036          99    11250       3750   400     1165  53       66
## 4         510          63    12960       5450   450      875  92       97
## 5         249         869     7560       4120   800     1500  76       72
## 6         678          41    13500       3335   500      675  67       73
##   S.F.Ratio perc.alumni Expend Grad.Rate
## 1      18.1          12   7041        60
## 2      12.2          16  10527        56
## 3      12.9          30   8735        54
## 4       7.7          37  19016        59
## 5      11.9           2  10922        15
## 6       9.4          11   9727        55
```

``` r
summary(college)
```

```
##    College               Apps           Accept          Enroll      
##  Length:565         Min.   :   81   Min.   :   72   Min.   :  35.0  
##  Class :character   1st Qu.:  619   1st Qu.:  501   1st Qu.: 206.0  
##  Mode  :character   Median : 1133   Median :  859   Median : 328.0  
##                     Mean   : 1978   Mean   : 1306   Mean   : 456.9  
##                     3rd Qu.: 2186   3rd Qu.: 1580   3rd Qu.: 520.0  
##                     Max.   :20192   Max.   :13007   Max.   :4615.0  
##    Top10perc       Top25perc       F.Undergrad     P.Undergrad   
##  Min.   : 1.00   Min.   :  9.00   Min.   :  139   Min.   :    1  
##  1st Qu.:17.00   1st Qu.: 42.00   1st Qu.:  840   1st Qu.:   63  
##  Median :25.00   Median : 55.00   Median : 1274   Median :  207  
##  Mean   :29.33   Mean   : 56.96   Mean   : 1872   Mean   :  434  
##  3rd Qu.:36.00   3rd Qu.: 70.00   3rd Qu.: 2018   3rd Qu.:  541  
##  Max.   :96.00   Max.   :100.00   Max.   :27378   Max.   :10221  
##     Outstate       Room.Board       Books           Personal   
##  Min.   : 2340   Min.   :2370   Min.   : 250.0   Min.   : 250  
##  1st Qu.: 9100   1st Qu.:3736   1st Qu.: 450.0   1st Qu.: 800  
##  Median :11200   Median :4400   Median : 500.0   Median :1100  
##  Mean   :11802   Mean   :4586   Mean   : 547.5   Mean   :1214  
##  3rd Qu.:13970   3rd Qu.:5400   3rd Qu.: 600.0   3rd Qu.:1500  
##  Max.   :21700   Max.   :8124   Max.   :2340.0   Max.   :6800  
##       PhD            Terminal        S.F.Ratio      perc.alumni   
##  Min.   :  8.00   Min.   : 24.00   Min.   : 2.50   Min.   : 2.00  
##  1st Qu.: 60.00   1st Qu.: 68.00   1st Qu.:11.10   1st Qu.:16.00  
##  Median : 73.00   Median : 81.00   Median :12.70   Median :25.00  
##  Mean   : 71.09   Mean   : 78.53   Mean   :12.95   Mean   :25.89  
##  3rd Qu.: 85.00   3rd Qu.: 92.00   3rd Qu.:14.50   3rd Qu.:34.00  
##  Max.   :100.00   Max.   :100.00   Max.   :39.80   Max.   :64.00  
##      Expend        Grad.Rate  
##  Min.   : 3186   Min.   : 15  
##  1st Qu.: 7477   1st Qu.: 58  
##  Median : 8954   Median : 69  
##  Mean   :10486   Mean   : 69  
##  3rd Qu.:11625   3rd Qu.: 81  
##  Max.   :56233   Max.   :118
```

rename first column:

``` r
if (names(college)[1] != "College") {
  names(college)[1] <- "College"
}

str(college)
```

```
## 'data.frame':	565 obs. of  18 variables:
##  $ College    : chr  "Abilene Christian University" "Adelphi University" "Adrian College" "Agnes Scott College" ...
##  $ Apps       : int  1660 2186 1428 417 193 587 353 1899 1038 582 ...
##  $ Accept     : int  1232 1924 1097 349 146 479 340 1720 839 498 ...
##  $ Enroll     : int  721 512 336 137 55 158 103 489 227 172 ...
##  $ Top10perc  : int  23 16 22 60 16 38 17 37 30 21 ...
##  $ Top25perc  : int  52 29 50 89 44 62 45 68 63 44 ...
##  $ F.Undergrad: int  2885 2683 1036 510 249 678 416 1594 973 799 ...
##  $ P.Undergrad: int  537 1227 99 63 869 41 230 32 306 78 ...
##  $ Outstate   : int  7440 12280 11250 12960 7560 13500 13290 13868 15595 10468 ...
##  $ Room.Board : int  3300 6450 3750 5450 4120 3335 5720 4826 4400 3380 ...
##  $ Books      : int  450 750 400 450 800 500 500 450 300 660 ...
##  $ Personal   : int  2200 1500 1165 875 1500 675 1500 850 500 1800 ...
##  $ PhD        : int  70 29 53 92 76 67 90 89 79 40 ...
##  $ Terminal   : int  78 30 66 97 72 73 93 100 84 41 ...
##  $ S.F.Ratio  : num  18.1 12.2 12.9 7.7 11.9 9.4 11.5 13.7 11.3 11.5 ...
##  $ perc.alumni: int  12 16 30 37 2 11 26 37 23 15 ...
##  $ Expend     : int  7041 10527 8735 19016 10922 9727 8861 11487 11644 8991 ...
##  $ Grad.Rate  : int  60 56 54 59 15 55 63 73 80 52 ...
```

data partition:

``` r
set.seed(2026)

n <- nrow(college)
train_id <- sample(seq_len(n), size = round(0.8 * n))

train <- college[train_id, ]
test  <- college[-train_id, ]

dim(train)
```

```
## [1] 452  18
```

``` r
dim(test)
```

```
## [1] 113  18
```

Defining RMSE function:

``` r
rmse <- function(pred, truth) {
  sqrt(mean((pred - truth)^2))
}
```

## Point 2. Part (a): Smoothing spline using Top10perc only

#2.1 Fit smoothing splines across a range of degrees of freedom

``` r
df_grid <- 3:12

spline_fits <- lapply(df_grid, function(d) {
  smooth.spline(
    x = train$Top10perc,
    y = train$Outstate,
    df = d,
    cv = TRUE
  )
})

cv_table <- data.frame(
  df = df_grid,
  cv_score = sapply(spline_fits, function(fit) fit$cv.crit)
)

cv_table
```

```
##    df cv_score
## 1   3  8284786
## 2   4  8285696
## 3   5  8299019
## 4   6  8312836
## 5   7  8324052
## 6   8  8335022
## 7   9  8347964
## 8  10  8363402
## 9  11  8380858
## 10 12  8399461
```

# 2.2 Choosing the best degree of freedom

``` r
best_df <- cv_table$df[which.min(cv_table$cv_score)]
best_df
```

```
## [1] 3
```


#2.3 Creating data for plotting fitted curves


``` r
x_grid <- seq(min(train$Top10perc), max(train$Top10perc), length.out = 200)

plot_data_list <- lapply(seq_along(df_grid), function(i) {
  pred_i <- predict(spline_fits[[i]], x = x_grid)
  
  data.frame(
    Top10perc = pred_i$x,
    Outstate_hat = pred_i$y,
    df = paste("df =", df_grid[i])
  )
})

plot_data <- bind_rows(plot_data_list)
```

#2.4 Plot fitted curves for each degree of freedom

``` r
ggplot(train, aes(x = Top10perc, y = Outstate)) +
  geom_point(alpha = 0.5, color = "gray40") +
  geom_line(
    data = plot_data,
    aes(x = Top10perc, y = Outstate_hat),
    color = "blue",
    linewidth = 1
  ) +
  facet_wrap(~ df, ncol = 2) +
  labs(
    title = "Smoothing spline fits across degrees of freedom",
    x = "Top10perc",
    y = "Outstate"
  ) +
  theme_minimal()
```

![](homework2_files/figure-latex/unnamed-chunk-11-1.pdf)<!-- --> 

#2.5 Plot the optimal smoothing spline fit

``` r
best_spline <- smooth.spline(
  x = train$Top10perc,
  y = train$Outstate,
  df = best_df,
  cv = TRUE
)

best_pred <- predict(best_spline, x = x_grid)

best_plot_df <- data.frame(
  Top10perc = best_pred$x,
  Outstate_hat = best_pred$y
)
```


``` r
ggplot(train, aes(x = Top10perc, y = Outstate)) +
  geom_point(alpha = 0.5, color = "gray40") +
  geom_line(
    data = best_plot_df,
    aes(x = Top10perc, y = Outstate_hat),
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = paste("Optimal smoothing spline fit (df =", best_df, ")"),
    subtitle = "Selected using the smallest CV score",
    x = "Top10perc",
    y = "Outstate"
  ) +
  theme_minimal()
```

![](homework2_files/figure-latex/unnamed-chunk-13-1.pdf)<!-- --> 

#2.6 Compute test error for the selected spline model

``` r
spline_test_pred <- predict(best_spline, x = test$Top10perc)$y
spline_test_rmse <- rmse(spline_test_pred, test$Outstate)

spline_test_rmse
```

```
## [1] 3018.485
```

#2.7 answer for Point 2a
At lower degrees of freedom, the smoothing spline is very smooth and may underfit the relationship between Top10perc and Outstate. As the degrees of freedom increase, the spline becomes more flexible and can capture more curvature in the data. When the degrees of freedom become too large, the fitted curve may become too wiggly and begin fitting noise rather than the underlying trend.

I selected the optimal degree of freedom using the cross-validation criterion returned by smooth.spline(..., cv = TRUE). Specifically, I chose the degree of freedom with the smallest CV score. The test RMSE for the selected smoothing spline model is reported above.

## Point 3. Part (b): Multivariate Adaptive Regression Splines (MARS)
#3.1 data prepp for MARS 

``` r
train_mars <- train %>% select(-College)
test_mars  <- test %>% select(-College)

str(train_mars)
```

```
## 'data.frame':	452 obs. of  17 variables:
##  $ Apps       : int  695 1239 1432 3151 553 560 824 1310 1160 2324 ...
##  $ Accept     : int  535 1017 888 2584 452 392 670 983 991 1319 ...
##  $ Enroll     : int  239 383 317 958 228 270 337 316 352 370 ...
##  $ Top10perc  : int  21 10 29 14 22 11 15 5 19 52 ...
##  $ Top25perc  : int  30 34 58 40 49 31 41 35 55 81 ...
##  $ F.Undergrad: int  988 1207 1121 4772 1301 743 1160 1057 1357 1686 ...
##  $ P.Undergrad: int  785 157 493 856 242 118 653 175 737 35 ...
##  $ Outstate   : int  10200 7820 10860 7800 9650 8734 9400 8550 12200 16560 ...
##  $ Room.Board : int  7000 3400 5760 3750 4400 3362 3400 5050 3880 5140 ...
##  $ Books      : int  350 550 550 570 600 600 500 400 480 558 ...
##  $ Personal   : int  1100 1550 900 3020 1000 625 1100 900 930 1152 ...
##  $ PhD        : int  63 69 56 37 57 56 37 35 74 91 ...
##  $ Terminal   : int  76 81 62 43 69 78 37 67 81 93 ...
##  $ S.F.Ratio  : num  11.7 13.9 12.9 16.5 14.9 11.3 8.4 17.4 17.8 10.5 ...
##  $ perc.alumni: int  20 8 23 4 8 27 21 16 25 30 ...
##  $ Expend     : int  10622 7264 8604 12878 6336 6422 5352 4333 7666 16196 ...
##  $ Grad.Rate  : int  68 91 96 44 83 68 59 27 79 79 ...
```

#3.2 Fit the MARS model

``` r
set.seed(2026)

mars_fit <- earth(
  Outstate ~ .,
  data = train_mars,
  degree = 2,
  trace = 0
)

summary(mars_fit)
```

```
## Call: earth(formula=Outstate~., data=train_mars, trace=0, degree=2)
## 
##                                        coefficients
## (Intercept)                               8565.1365
## h(Accept-1598)                               0.3503
## h(1379-F.Undergrad)                         -4.3955
## h(F.Undergrad-1379)                         -0.2787
## h(4130-Room.Board)                          -1.7235
## h(Room.Board-4130)                           0.4516
## h(1400-Personal)                             1.8202
## h(PhD-81)                                   64.6390
## h(Expend-4933)                               0.7745
## h(Expend-15494)                             -1.2547
## h(83-Grad.Rate)                            -59.4114
## h(56-Top25perc) * h(Expend-15494)            0.0444
## h(Top25perc-56) * h(Expend-15494)            0.0124
## h(1379-F.Undergrad) * h(12011-Expend)        0.0006
## h(2943-F.Undergrad) * h(83-Grad.Rate)        0.0237
## h(4130-Room.Board) * h(600-Books)            0.0084
## h(4130-Room.Board) * h(22-perc.alumni)      -0.0939
## h(1400-Personal) * h(32-perc.alumni)        -0.0832
## h(80-Terminal) * h(Expend-15494)             0.4293
## 
## Selected 19 of 32 terms, and 11 of 16 predictors
## Termination condition: Reached nk 33
## Importance: Expend, Grad.Rate, Room.Board, Personal, F.Undergrad, Books, ...
## Number of terms at each degree of interaction: 1 10 8
## GCV 2687587    RSS 980113136    GRSq 0.8095451    RSq 0.8456555
```

#3.3 Report the regression function

``` r
formula(mars_fit)
```

```
## Outstate ~ Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + 
##     P.Undergrad + Room.Board + Books + Personal + PhD + Terminal + 
##     S.F.Ratio + perc.alumni + Expend + Grad.Rate
## attr(,"variables")
## list(Outstate, Apps, Accept, Enroll, Top10perc, Top25perc, F.Undergrad, 
##     P.Undergrad, Room.Board, Books, Personal, PhD, Terminal, 
##     S.F.Ratio, perc.alumni, Expend, Grad.Rate)
## attr(,"factors")
##             Apps Accept Enroll Top10perc Top25perc F.Undergrad P.Undergrad
## Outstate       0      0      0         0         0           0           0
## Apps           1      0      0         0         0           0           0
## Accept         0      1      0         0         0           0           0
## Enroll         0      0      1         0         0           0           0
## Top10perc      0      0      0         1         0           0           0
## Top25perc      0      0      0         0         1           0           0
## F.Undergrad    0      0      0         0         0           1           0
## P.Undergrad    0      0      0         0         0           0           1
## Room.Board     0      0      0         0         0           0           0
## Books          0      0      0         0         0           0           0
## Personal       0      0      0         0         0           0           0
## PhD            0      0      0         0         0           0           0
## Terminal       0      0      0         0         0           0           0
## S.F.Ratio      0      0      0         0         0           0           0
## perc.alumni    0      0      0         0         0           0           0
## Expend         0      0      0         0         0           0           0
## Grad.Rate      0      0      0         0         0           0           0
##             Room.Board Books Personal PhD Terminal S.F.Ratio perc.alumni Expend
## Outstate             0     0        0   0        0         0           0      0
## Apps                 0     0        0   0        0         0           0      0
## Accept               0     0        0   0        0         0           0      0
## Enroll               0     0        0   0        0         0           0      0
## Top10perc            0     0        0   0        0         0           0      0
## Top25perc            0     0        0   0        0         0           0      0
## F.Undergrad          0     0        0   0        0         0           0      0
## P.Undergrad          0     0        0   0        0         0           0      0
## Room.Board           1     0        0   0        0         0           0      0
## Books                0     1        0   0        0         0           0      0
## Personal             0     0        1   0        0         0           0      0
## PhD                  0     0        0   1        0         0           0      0
## Terminal             0     0        0   0        1         0           0      0
## S.F.Ratio            0     0        0   0        0         1           0      0
## perc.alumni          0     0        0   0        0         0           1      0
## Expend               0     0        0   0        0         0           0      1
## Grad.Rate            0     0        0   0        0         0           0      0
##             Grad.Rate
## Outstate            0
## Apps                0
## Accept              0
## Enroll              0
## Top10perc           0
## Top25perc           0
## F.Undergrad         0
## P.Undergrad         0
## Room.Board          0
## Books               0
## Personal            0
## PhD                 0
## Terminal            0
## S.F.Ratio           0
## perc.alumni         0
## Expend              0
## Grad.Rate           1
## attr(,"term.labels")
##  [1] "Apps"        "Accept"      "Enroll"      "Top10perc"   "Top25perc"  
##  [6] "F.Undergrad" "P.Undergrad" "Room.Board"  "Books"       "Personal"   
## [11] "PhD"         "Terminal"    "S.F.Ratio"   "perc.alumni" "Expend"     
## [16] "Grad.Rate"  
## attr(,"order")
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## attr(,"intercept")
## [1] 1
## attr(,"response")
## [1] 1
## attr(,"predvars")
## list(Outstate, Apps, Accept, Enroll, Top10perc, Top25perc, F.Undergrad, 
##     P.Undergrad, Room.Board, Books, Personal, PhD, Terminal, 
##     S.F.Ratio, perc.alumni, Expend, Grad.Rate)
## attr(,"dataClasses")
##    Outstate        Apps      Accept      Enroll   Top10perc   Top25perc 
##   "numeric"   "numeric"   "numeric"   "numeric"   "numeric"   "numeric" 
## F.Undergrad P.Undergrad  Room.Board       Books    Personal         PhD 
##   "numeric"   "numeric"   "numeric"   "numeric"   "numeric"   "numeric" 
##    Terminal   S.F.Ratio perc.alumni      Expend   Grad.Rate 
##   "numeric"   "numeric"   "numeric"   "numeric"   "numeric"
```

``` r
coef(mars_fit)
```

```
##                            (Intercept)                        h(Expend-15494) 
##                           8.565137e+03                          -1.254667e+00 
##                        h(83-Grad.Rate)                     h(Room.Board-4130) 
##                          -5.941142e+01                           4.516159e-01 
##                     h(4130-Room.Board)                    h(F.Undergrad-1379) 
##                          -1.723508e+00                          -2.786987e-01 
##                    h(1379-F.Undergrad)                       h(1400-Personal) 
##                          -4.395458e+00                           1.820193e+00 
##   h(1400-Personal) * h(32-perc.alumni)  h(1379-F.Undergrad) * h(12011-Expend) 
##                          -8.317456e-02                           6.351720e-04 
##      h(4130-Room.Board) * h(600-Books)       h(80-Terminal) * h(Expend-15494) 
##                           8.357174e-03                           4.292716e-01 
##      h(Top25perc-56) * h(Expend-15494)      h(56-Top25perc) * h(Expend-15494) 
##                           1.235142e-02                           4.440995e-02 
##                         h(Expend-4933)  h(2943-F.Undergrad) * h(83-Grad.Rate) 
##                           7.745425e-01                           2.372376e-02 
## h(4130-Room.Board) * h(22-perc.alumni)                         h(Accept-1598) 
##                          -9.388876e-02                           3.502941e-01 
##                              h(PhD-81) 
##                           6.463901e+01
```

#3.4 Display variable importance

``` r
evimp(mars_fit)
```

```
##             nsubsets   gcv    rss
## Expend            18 100.0  100.0
## Grad.Rate         16  42.7   45.7
## Room.Board        15  34.0   37.7
## Personal          14  29.3   33.4
## F.Undergrad       13  27.4   31.4
## Books             10  19.2   23.6
## perc.alumni        9  17.6   21.8
## Terminal           8  15.3   19.6
## Top25perc          6  10.1   14.9
## PhD                2   6.3    8.8
## Accept             1   4.5    6.2
```

#3.5 Partial dependence plot for an arbitrary predictor

``` r
mars_pdp <- partial(
  object = mars_fit,
  pred.var = "Expend",
  train = train_mars
)

head(mars_pdp)
```

```
##   Expend      yhat
## 1   3186  9064.619
## 2   4246  8877.646
## 3   5307  8980.175
## 4   6368  9614.816
## 5   7429 10249.456
## 6   8490 10884.096
```


``` r
autoplot(mars_pdp, contour = FALSE) +
  labs(
    title = "Partial dependence plot for Expend in the MARS model",
    x = "Expend",
    y = "Predicted Outstate"
  ) +
  theme_minimal()
```

![](homework2_files/figure-latex/unnamed-chunk-20-1.pdf)<!-- --> 


#3.6 Compute test error for the MARS model

``` r
mars_test_pred <- predict(mars_fit, newdata = test_mars)
mars_test_rmse <- rmse(mars_test_pred, test_mars$Outstate)

mars_test_rmse
```

```
## [1] 2478.376
```

#3.7 Written answer for part 3b

The MARS model fits a flexible regression function using piecewise linear basis functions. This allows the model to capture nonlinear relationships and interactions among predictors without requiring these terms to be specified in advance. The regression function is reported above through the fitted basis functions and coefficients. The partial dependence plot for Expend shows how predicted out-of-state tuition changes as instructional expenditure changes, averaging over the other predictors. The test RMSE reported above summarizes the predictive performance of the MARS model on the test data.


## Point 4. Part (c): Generalized Additive Model (GAM)

#4.1 Prepare data for GAM

``` r
train_gam <- train %>% select(-College)
test_gam  <- test %>% select(-College)
```

#4.2 Fit the GAM model

``` r
set.seed(2026)

gam_fit <- gam(
  Outstate ~ 
    s(Apps, k = 10) +
    s(Accept, k = 10) +
    s(Enroll, k = 10) +
    s(Top10perc, k = 10) +
    s(Top25perc, k = 10) +
    s(F.Undergrad, k = 10) +
    s(P.Undergrad, k = 10) +
    s(Room.Board, k = 10) +
    s(Books, k = 10) +
    s(Personal, k = 10) +
    s(PhD, k = 10) +
    s(Terminal, k = 10) +
    s(S.F.Ratio, k = 10) +
    s(perc.alumni, k = 10) +
    s(Expend, k = 10) +
    s(Grad.Rate, k = 10),
  data = train_gam,
  method = "REML",
  select = TRUE
)

summary(gam_fit)
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## Outstate ~ s(Apps, k = 10) + s(Accept, k = 10) + s(Enroll, k = 10) + 
##     s(Top10perc, k = 10) + s(Top25perc, k = 10) + s(F.Undergrad, 
##     k = 10) + s(P.Undergrad, k = 10) + s(Room.Board, k = 10) + 
##     s(Books, k = 10) + s(Personal, k = 10) + s(PhD, k = 10) + 
##     s(Terminal, k = 10) + s(S.F.Ratio, k = 10) + s(perc.alumni, 
##     k = 10) + s(Expend, k = 10) + s(Grad.Rate, k = 10)
## 
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11779.54      77.14   152.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                      edf Ref.df      F  p-value    
## s(Apps)        0.8524465      9  0.629 0.004895 ** 
## s(Accept)      2.2814373      9  1.347 0.000161 ***
## s(Enroll)      2.1283279      9  1.178 0.000538 ***
## s(Top10perc)   0.7561369      9  0.344 0.036433 *  
## s(Top25perc)   0.0010806      9  0.000 0.672458    
## s(F.Undergrad) 4.6430104      9  1.847 0.000163 ***
## s(P.Undergrad) 0.0006087      9  0.000 0.877091    
## s(Room.Board)  2.6746192      9  5.673  < 2e-16 ***
## s(Books)       0.4817507      9  0.103 0.157315    
## s(Personal)    1.7049708      9  1.489 0.000274 ***
## s(PhD)         1.9331130      9  0.759 0.013560 *  
## s(Terminal)    0.0385656      9  0.004 0.280749    
## s(S.F.Ratio)   3.9439876      9  1.389 0.006380 ** 
## s(perc.alumni) 1.8049426      9  1.422 0.000371 ***
## s(Expend)      5.4928313      9 15.569  < 2e-16 ***
## s(Grad.Rate)   1.7189443      9  1.000 0.002548 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.809   Deviance explained = 82.2%
## -REML = 4023.4  Scale est. = 2.6895e+06  n = 452
```



#4.3 Plot nonlinear terms

``` r
plot(gam_fit, pages = 1, shade = TRUE, residuals = TRUE)
```

![](homework2_files/figure-latex/unnamed-chunk-24-1.pdf)<!-- --> 

#4.4 Compute test error for the GAM model

``` r
gam_test_pred <- predict(gam_fit, newdata = test_gam)
gam_test_rmse <- rmse(gam_test_pred, test_gam$Outstate)

gam_test_rmse
```

```
## [1] 1627.169
```

#4.5 Written answer for part (c)

The GAM allows each predictor to have its own smooth nonlinear relationship with Outstate. In the model summary, terms with estimated degrees of freedom greater than 1 suggest nonlinear effects. The plots above visualize these smooth terms. Some predictors may show approximately linear trends, while others may display more curvature, thresholds, or diminishing returns. The test RMSE reported above summarizes the predictive performance of the GAM on the test set.


Point 5. Part (d): Would I favor a MARS model over a linear model in this dataset?

#5.1 Fit a linear model for comparison

``` r
train_lm <- train %>% select(-College)
test_lm  <- test %>% select(-College)

lm_fit <- lm(Outstate ~ ., data = train_lm)

summary(lm_fit)
```

```
## 
## Call:
## lm(formula = Outstate ~ ., data = train_lm)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6688.9 -1235.5    58.7  1285.7  9709.9 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1463.80297  928.03563   1.577 0.115450    
## Apps          -0.09268    0.11480  -0.807 0.419935    
## Accept         1.37465    0.20714   6.636 9.60e-11 ***
## Enroll        -3.61879    0.90814  -3.985 7.92e-05 ***
## Top10perc     43.40542   15.72363   2.761 0.006015 ** 
## Top25perc     -5.13726   12.44128  -0.413 0.679868    
## F.Undergrad    0.03130    0.14021   0.223 0.823443    
## P.Undergrad   -0.19819    0.14589  -1.358 0.175017    
## Room.Board     0.81999    0.11189   7.329 1.14e-12 ***
## Books          0.10996    0.58015   0.190 0.849760    
## Personal      -0.50331    0.14694  -3.425 0.000672 ***
## PhD            6.81964   10.79454   0.632 0.527871    
## Terminal      28.07301   11.97714   2.344 0.019533 *  
## S.F.Ratio    -33.91785   31.33997  -1.082 0.279739    
## perc.alumni   38.11307    9.40868   4.051 6.04e-05 ***
## Expend         0.16738    0.02816   5.944 5.71e-09 ***
## Grad.Rate     17.91217    7.06486   2.535 0.011581 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1967 on 435 degrees of freedom
## Multiple R-squared:  0.7349,	Adjusted R-squared:  0.7251 
## F-statistic: 75.36 on 16 and 435 DF,  p-value: < 2.2e-16
```

#5.2 Compute test error for the linear model

``` r
lm_test_pred <- predict(lm_fit, newdata = test_lm)
lm_test_rmse <- rmse(lm_test_pred, test_lm$Outstate)

lm_test_rmse
```

```
## [1] 1924.72
```

#5.3 Compare predictive performance across models

``` r
results <- data.frame(
  Model = c(
    "Smoothing spline (Top10perc only)",
    "MARS",
    "GAM",
    "Linear model"
  ),
  Test_RMSE = c(
    spline_test_rmse,
    mars_test_rmse,
    gam_test_rmse,
    lm_test_rmse
  )
)

results[order(results$Test_RMSE), ]
```

```
##                               Model Test_RMSE
## 3                               GAM  1627.169
## 4                      Linear model  1924.720
## 2                              MARS  2478.376
## 1 Smoothing spline (Top10perc only)  3018.485
```


#5.4 Written answer for part (d)

In this dataset, I would favor the MARS model over a linear model if the MARS model achieves a clearly lower test RMSE. A lower test RMSE would suggest that nonlinear relationships and interactions are important for predicting out-of-state tuition, and that a simple linear model may be too restrictive. Because tuition is likely influenced by multiple institutional features in complex ways, MARS can be useful since it automatically captures nonlinearities and interactions. However, if the MARS and linear model have very similar test errors, I would prefer the linear model because it is simpler and easier to interpret.


## Point 6. Part (e): In practical applications, is MARS superior to a linear model?
6.1 Written answer for part (e)

In practical applications, I would not say that a MARS model is always superior to a linear model. The better choice depends on the purpose of the analysis and the structure of the data.

A MARS model is often preferable when the main goal is prediction and the data contain nonlinear relationships or interactions that are difficult to specify ahead of time. Because it is more flexible, it can produce better predictive accuracy when the true relationships are not linear.

A linear model is often preferable when interpretability, simplicity, and ease of communication are more important. Linear models are easier to explain, easier to report, and less complex. If the linear model performs nearly as well as the MARS model, the linear model may be the better practical choice.

Therefore, in practice, I would compare both models on test-set performance and then decide whether the predictive gain from MARS is large enough to justify the additional complexity.


##Point 7. Final comparison table

``` r
results
```

```
##                               Model Test_RMSE
## 1 Smoothing spline (Top10perc only)  3018.485
## 2                              MARS  2478.376
## 3                               GAM  1627.169
## 4                      Linear model  1924.720
```

















