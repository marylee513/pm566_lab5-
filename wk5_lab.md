pm566_wk5_lab
================
Yiping Li
2022-09-22

``` r
library(data.table)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(dplyr)
library(dtplyr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
if (!file.exists("met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("met_all.gz")

met <- met[temp > -17][elev == 9999.0, elev:=NA]
```

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

``` r
met <- merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  )
```

\#Question 1: Representative station for the US

``` r
station_averages <- 
  met[, .(
    temp = mean(temp, na.rm=T), 
    wind.sp = mean(wind.sp, na.rm=T), 
    atm.press = mean(atm.press, na.rm=T)
  ), by = USAFID]

length(unique(met[,USAFID]))
```

    ## [1] 1588

\#the above computes the mean by weather station. Now let’s compute the
median value for each variable

``` r
statemedians <- 
  met[, .(
    temp50 = median(temp, na.rm=T), 
    wind.sp50 = median(wind.sp, na.rm=T), 
    atm.press50 = median(atm.press, na.rm=T)
  )]

#summary(statmedians[, temp]) not working
```

\#A helpful function we might want to use which.min()”

``` r
station_averages[, temp_dist50 := abs(temp - statemedians$temp50)][order(temp_dist50)]
```

    ##       USAFID      temp   wind.sp atm.press  temp_dist50
    ##    1: 725830 23.498627  2.966084  1012.675  0.001372998
    ##    2: 720549 23.496276  1.953681       NaN  0.003724395
    ##    3: 724769 23.489852  3.214057  1013.090  0.010148233
    ##    4: 723114 23.520054  1.846268       NaN  0.020053957
    ##    5: 726813 23.478088  2.435372  1012.315  0.021912351
    ##   ---                                                  
    ## 1584: 722788 36.852459  3.393852       NaN 13.352459016
    ## 1585: 722787 37.258907  2.847381       NaN 13.758907363
    ## 1586: 723805 37.625391  3.532935  1005.207 14.125390625
    ## 1587: 726130  9.189602 12.239908       NaN 14.310397554
    ## 1588: 720385  8.044959  7.298963       NaN 15.455040872

\#Lets use which.min

``` r
station_averages[which.min(temp_dist50)]
```

    ##    USAFID     temp  wind.sp atm.press temp_dist50
    ## 1: 725830 23.49863 2.966084  1012.675 0.001372998

it matches the above results

\#Question 2: Representative station per state

``` r
station_averages <- 
  met[, .(
    temp = mean(temp, na.rm=T), 
    wind.sp = mean(wind.sp, na.rm=T), 
    atm.press = mean(atm.press, na.rm=T)
  ), by = .(USAFID,STATE)]
```

``` r
state_medians <- station_averages[,.(temp50=median(temp,na.rm=T),
                   wind.sp50=median(wind.sp,na.rm=T)),by=STATE]
head(state_medians)
```

    ##    STATE   temp50 wind.sp50
    ## 1:    CA 22.66268  2.561738
    ## 2:    TX 29.75188  3.413810
    ## 3:    MI 20.51970  2.273423
    ## 4:    SC 25.80545  1.696119
    ## 5:    IL 22.43194  2.237652
    ## 6:    MO 23.95109  2.453547

``` r
station_averages <- merge(
  x = station_averages, 
  y = state_medians, 
  by.x = "STATE", 
  by.y = "STATE", 
  all.x = TRUE, 
  all.y = FALSE
)



#find the smallest distance iwth "which.min()"
station_averages[,temp_dist_state50 := temp - temp50]
station_averages[,wind.sp_dist50 := wind.sp - wind.sp50]


station_averages[,eucdist :=temp_dist_state50^2 +wind.sp_dist50^2 ]

repstation <- station_averages[,.(eucdist=min(eucdist,na.rm=TRUE)),by=STATE]

# merge the repstation with the station_average
test <- merge(x=station_averages,
  y=repstation,
  by.x = c('eucdist','STATE'),
  by.y= c('eucdist','STATE'),
  all.x=FALSE,
  all.y=TRUE
)

dim(test)
```

    ## [1] 49 10

``` r
head(test)
```

    ##         eucdist STATE USAFID     temp  wind.sp atm.press   temp50 wind.sp50
    ## 1: 0.0000000000    DE 724180 24.56026 2.753082  1015.046 24.56026  2.753082
    ## 2: 0.0000000000    MD 722218 24.89883 1.883499       NaN 24.89883  1.883499
    ## 3: 0.0000000000    NJ 724090 23.47238 2.148058  1015.095 23.47238  2.148058
    ## 4: 0.0000000000    WA 720254 19.24684 1.268571       NaN 19.24684  1.268571
    ## 5: 0.0003044727    WV 720328 21.94820 1.615064       NaN 21.94446  1.632107
    ## 6: 0.0003044727    WV 724176 21.94072 1.649151  1015.982 21.94446  1.632107
    ##    temp_dist_state50 wind.sp_dist50
    ## 1:       0.000000000     0.00000000
    ## 2:       0.000000000     0.00000000
    ## 3:       0.000000000     0.00000000
    ## 4:       0.000000000     0.00000000
    ## 5:       0.003739818    -0.01704366
    ## 6:      -0.003739818     0.01704366
