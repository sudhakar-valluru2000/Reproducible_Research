 Course Reproducible Research by Sudhakar Raju Valluru
=======================================================
#  Peer Assessment 1 

##  1. Introduction 
This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. 
The data consists of two months of data from an anonymous individual 
collected during the months of October and November, 2012 and include 
the number of steps taken in 5 minute intervals each day.

##  2.   Read The data "Activity Monitoring Data", the file is "activity.csv" 

###    Loading and preprocessing the data 



```r
opts_chunk$set (echo=TRUE)

library (knitr)
library (dplyr)
library (lubridate)
library(ggplot2)


activity_data <- read.csv ("./activity.csv", header = TRUE, sep = ',', 
				colClasses = c("numeric", "character", "integer"))

str (activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity_data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

### Draw a pairs plot to show the data between the "steps" and the "interval"


```r
pairs ( ~ steps + interval, data = activity_data )
```

![plot of chunk Draw pairs plot](figure/Draw pairs plot-1.png) 


##  3. What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

If you do not understand the difference between a histogram and a barplot, 
research the difference between them. Make a histogram of the total number of steps taken each day


Calculate and report the mean and median of the total number of steps taken per day


#### From the data frame "activity_data", then group by "date" and then total the steps, then print the output

there is a problem with this, the NA's are showing up, we need to filter the NA' using the filter function



```r
total_steps <-  activity_data %>% filter (!is.na(steps)) %>% group_by (date) %>% summarize (steps = sum(steps)) %>%
print
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

#### Lets draw a plot using ggplot2 library function ggplot
#### There is a warning saying use binwidth , says to use binwidth  = x, Lets use binwidth = 10000


```r
ggplot () + 
geom_histogram(data=total_steps, aes(x = steps), fill="yellow", color  = "red", binwidth = 10000) +
labs(title = "Histogram of Steps/day", x = "Steps", y = "Frequency")
```

![plot of chunk Draw ggplot histogram](figure/Draw ggplot histogram-1.png) 

#### Lets calculate the mean, meadian of steps as well


```r
steps_mean <- total_steps %>% summarize (mean = mean(steps)) %>%
print
```

```
## Source: local data frame [1 x 1]
## 
##       mean
## 1 10766.19
```

```r
steps_median <- total_steps %>% summarize (median = median(steps)) %>%
print
```

```
## Source: local data frame [1 x 1]
## 
##   median
## 1  10765
```


## 4. What is the average daily activity pattern?

 a) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
	averaged across all days (y-axis)


```r
fivemininterval <-  activity_data %>% filter (!is.na(steps)) %>% group_by (interval) %>% summarize (mean_steps = mean(steps)) %>%
print
```

```
## Source: local data frame [288 x 2]
## 
##    interval mean_steps
## 1         0  1.7169811
## 2         5  0.3396226
## 3        10  0.1320755
## 4        15  0.1509434
## 5        20  0.0754717
## 6        25  2.0943396
## 7        30  0.5283019
## 8        35  0.8679245
## 9        40  0.0000000
## 10       45  1.4716981
## ..      ...        ...
```

#### Plot a graph
 

 


```r
ggplot () + 
geom_line(data=fivemininterval , aes(x = interval, y = mean_steps), fill="yellow", color  = "red", type = "l") +
labs(title = "Time Series Plot of 5 min interval and Average of steps", x = "Time Interval", y = "Average Steps")
```

![plot of chunk Draw timeseries ggplot histogram](figure/Draw timeseries ggplot histogram-1.png) 

b) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Use which.max function to find the max



```r
fivemininterval[which.max(fivemininterval$mean_steps), ]
```

```
## Source: local data frame [1 x 2]
## 
##   interval mean_steps
## 1      835   206.1698
```

#### 5. Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). 
The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Copy the original dataset into a new one first



```r
NA_activity_data <- activity_data
sum(is.na(NA_activity_data$steps))
```

```
## [1] 2304
```


Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#full_activity_data <- NA_activity_data %>% group_by (interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps )) %>%
#print

full_activity_data <- NA_activity_data %>% group_by (interval) %>% mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm=TRUE) )) %>%
print
```

```
## Source: local data frame [17,568 x 3]
## Groups: interval
## 
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
## ..       ...        ...      ...
```


```r
sum(is.na(full_activity_data$steps))
```

```
## [1] 0
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps 
taken per day. Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

Total Steps per day

```r
full_total_steps <-  full_activity_data %>% filter (!is.na(steps)) %>% group_by (date) %>% summarize (steps = sum(steps)) %>%
print
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```

Plot the graph


```r
ggplot () + 
geom_histogram(data=full_total_steps, aes(x = steps), fill="yellow", color  = "red", binwidth = 10000) +
labs(title = "Histogram of Steps/day without NAs ", x = "Steps", y = "Frequency")
```

![plot of chunk Plot ggplot histogram without NAs](figure/Plot ggplot histogram without NAs-1.png) 

Lets calculate the mean, meadian of steps as well


```r
full_steps_mean <- full_total_steps %>% summarize (mean = mean(steps)) %>%
print
```

```
## Source: local data frame [1 x 1]
## 
##       mean
## 1 10766.19
```

```r
full_steps_median <- full_total_steps %>% summarize (median = median(steps)) %>%
print
```

```
## Source: local data frame [1 x 1]
## 
##     median
## 1 10766.19
```

# Both the values of  mean and meadian are same for dataset with/without missing values respectivily.


#### 6. Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Add a field to the dataset depending on the date if its a weekday or a weekend, by using function weekdays


```r
weekfulldata  <- full_activity_data 
weekfulldata <- 
        weekfulldata %>%
        mutate(week = ifelse(weekdays(ymd(date)) %in% 
                                c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekday", "weekend" )) %>%
	print
```

```
## Source: local data frame [17,568 x 4]
## Groups: interval
## 
##        steps       date interval    week
## 1  1.7169811 2012-10-01        0 weekday
## 2  0.3396226 2012-10-01        5 weekday
## 3  0.1320755 2012-10-01       10 weekday
## 4  0.1509434 2012-10-01       15 weekday
## 5  0.0754717 2012-10-01       20 weekday
## 6  2.0943396 2012-10-01       25 weekday
## 7  0.5283019 2012-10-01       30 weekday
## 8  0.8679245 2012-10-01       35 weekday
## 9  0.0000000 2012-10-01       40 weekday
## 10 1.4716981 2012-10-01       45 weekday
## ..       ...        ...      ...     ...
```

```r
#print

str(weekfulldata)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ week    : chr  "weekday" "weekday" "weekday" "weekday" ...
##  - attr(*, "vars")=List of 1
##   ..$ : symbol interval
##  - attr(*, "labels")='data.frame':	288 obs. of  1 variable:
##   ..$ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##   ..- attr(*, "vars")=List of 1
##   .. ..$ : symbol interval
##  - attr(*, "indices")=List of 288
##   ..$ : int  0 288 576 864 1152 1440 1728 2016 2304 2592 ...
##   ..$ : int  1 289 577 865 1153 1441 1729 2017 2305 2593 ...
##   ..$ : int  2 290 578 866 1154 1442 1730 2018 2306 2594 ...
##   ..$ : int  3 291 579 867 1155 1443 1731 2019 2307 2595 ...
##   ..$ : int  4 292 580 868 1156 1444 1732 2020 2308 2596 ...
##   ..$ : int  5 293 581 869 1157 1445 1733 2021 2309 2597 ...
##   ..$ : int  6 294 582 870 1158 1446 1734 2022 2310 2598 ...
##   ..$ : int  7 295 583 871 1159 1447 1735 2023 2311 2599 ...
##   ..$ : int  8 296 584 872 1160 1448 1736 2024 2312 2600 ...
##   ..$ : int  9 297 585 873 1161 1449 1737 2025 2313 2601 ...
##   ..$ : int  10 298 586 874 1162 1450 1738 2026 2314 2602 ...
##   ..$ : int  11 299 587 875 1163 1451 1739 2027 2315 2603 ...
##   ..$ : int  12 300 588 876 1164 1452 1740 2028 2316 2604 ...
##   ..$ : int  13 301 589 877 1165 1453 1741 2029 2317 2605 ...
##   ..$ : int  14 302 590 878 1166 1454 1742 2030 2318 2606 ...
##   ..$ : int  15 303 591 879 1167 1455 1743 2031 2319 2607 ...
##   ..$ : int  16 304 592 880 1168 1456 1744 2032 2320 2608 ...
##   ..$ : int  17 305 593 881 1169 1457 1745 2033 2321 2609 ...
##   ..$ : int  18 306 594 882 1170 1458 1746 2034 2322 2610 ...
##   ..$ : int  19 307 595 883 1171 1459 1747 2035 2323 2611 ...
##   ..$ : int  20 308 596 884 1172 1460 1748 2036 2324 2612 ...
##   ..$ : int  21 309 597 885 1173 1461 1749 2037 2325 2613 ...
##   ..$ : int  22 310 598 886 1174 1462 1750 2038 2326 2614 ...
##   ..$ : int  23 311 599 887 1175 1463 1751 2039 2327 2615 ...
##   ..$ : int  24 312 600 888 1176 1464 1752 2040 2328 2616 ...
##   ..$ : int  25 313 601 889 1177 1465 1753 2041 2329 2617 ...
##   ..$ : int  26 314 602 890 1178 1466 1754 2042 2330 2618 ...
##   ..$ : int  27 315 603 891 1179 1467 1755 2043 2331 2619 ...
##   ..$ : int  28 316 604 892 1180 1468 1756 2044 2332 2620 ...
##   ..$ : int  29 317 605 893 1181 1469 1757 2045 2333 2621 ...
##   ..$ : int  30 318 606 894 1182 1470 1758 2046 2334 2622 ...
##   ..$ : int  31 319 607 895 1183 1471 1759 2047 2335 2623 ...
##   ..$ : int  32 320 608 896 1184 1472 1760 2048 2336 2624 ...
##   ..$ : int  33 321 609 897 1185 1473 1761 2049 2337 2625 ...
##   ..$ : int  34 322 610 898 1186 1474 1762 2050 2338 2626 ...
##   ..$ : int  35 323 611 899 1187 1475 1763 2051 2339 2627 ...
##   ..$ : int  36 324 612 900 1188 1476 1764 2052 2340 2628 ...
##   ..$ : int  37 325 613 901 1189 1477 1765 2053 2341 2629 ...
##   ..$ : int  38 326 614 902 1190 1478 1766 2054 2342 2630 ...
##   ..$ : int  39 327 615 903 1191 1479 1767 2055 2343 2631 ...
##   ..$ : int  40 328 616 904 1192 1480 1768 2056 2344 2632 ...
##   ..$ : int  41 329 617 905 1193 1481 1769 2057 2345 2633 ...
##   ..$ : int  42 330 618 906 1194 1482 1770 2058 2346 2634 ...
##   ..$ : int  43 331 619 907 1195 1483 1771 2059 2347 2635 ...
##   ..$ : int  44 332 620 908 1196 1484 1772 2060 2348 2636 ...
##   ..$ : int  45 333 621 909 1197 1485 1773 2061 2349 2637 ...
##   ..$ : int  46 334 622 910 1198 1486 1774 2062 2350 2638 ...
##   ..$ : int  47 335 623 911 1199 1487 1775 2063 2351 2639 ...
##   ..$ : int  48 336 624 912 1200 1488 1776 2064 2352 2640 ...
##   ..$ : int  49 337 625 913 1201 1489 1777 2065 2353 2641 ...
##   ..$ : int  50 338 626 914 1202 1490 1778 2066 2354 2642 ...
##   ..$ : int  51 339 627 915 1203 1491 1779 2067 2355 2643 ...
##   ..$ : int  52 340 628 916 1204 1492 1780 2068 2356 2644 ...
##   ..$ : int  53 341 629 917 1205 1493 1781 2069 2357 2645 ...
##   ..$ : int  54 342 630 918 1206 1494 1782 2070 2358 2646 ...
##   ..$ : int  55 343 631 919 1207 1495 1783 2071 2359 2647 ...
##   ..$ : int  56 344 632 920 1208 1496 1784 2072 2360 2648 ...
##   ..$ : int  57 345 633 921 1209 1497 1785 2073 2361 2649 ...
##   ..$ : int  58 346 634 922 1210 1498 1786 2074 2362 2650 ...
##   ..$ : int  59 347 635 923 1211 1499 1787 2075 2363 2651 ...
##   ..$ : int  60 348 636 924 1212 1500 1788 2076 2364 2652 ...
##   ..$ : int  61 349 637 925 1213 1501 1789 2077 2365 2653 ...
##   ..$ : int  62 350 638 926 1214 1502 1790 2078 2366 2654 ...
##   ..$ : int  63 351 639 927 1215 1503 1791 2079 2367 2655 ...
##   ..$ : int  64 352 640 928 1216 1504 1792 2080 2368 2656 ...
##   ..$ : int  65 353 641 929 1217 1505 1793 2081 2369 2657 ...
##   ..$ : int  66 354 642 930 1218 1506 1794 2082 2370 2658 ...
##   ..$ : int  67 355 643 931 1219 1507 1795 2083 2371 2659 ...
##   ..$ : int  68 356 644 932 1220 1508 1796 2084 2372 2660 ...
##   ..$ : int  69 357 645 933 1221 1509 1797 2085 2373 2661 ...
##   ..$ : int  70 358 646 934 1222 1510 1798 2086 2374 2662 ...
##   ..$ : int  71 359 647 935 1223 1511 1799 2087 2375 2663 ...
##   ..$ : int  72 360 648 936 1224 1512 1800 2088 2376 2664 ...
##   ..$ : int  73 361 649 937 1225 1513 1801 2089 2377 2665 ...
##   ..$ : int  74 362 650 938 1226 1514 1802 2090 2378 2666 ...
##   ..$ : int  75 363 651 939 1227 1515 1803 2091 2379 2667 ...
##   ..$ : int  76 364 652 940 1228 1516 1804 2092 2380 2668 ...
##   ..$ : int  77 365 653 941 1229 1517 1805 2093 2381 2669 ...
##   ..$ : int  78 366 654 942 1230 1518 1806 2094 2382 2670 ...
##   ..$ : int  79 367 655 943 1231 1519 1807 2095 2383 2671 ...
##   ..$ : int  80 368 656 944 1232 1520 1808 2096 2384 2672 ...
##   ..$ : int  81 369 657 945 1233 1521 1809 2097 2385 2673 ...
##   ..$ : int  82 370 658 946 1234 1522 1810 2098 2386 2674 ...
##   ..$ : int  83 371 659 947 1235 1523 1811 2099 2387 2675 ...
##   ..$ : int  84 372 660 948 1236 1524 1812 2100 2388 2676 ...
##   ..$ : int  85 373 661 949 1237 1525 1813 2101 2389 2677 ...
##   ..$ : int  86 374 662 950 1238 1526 1814 2102 2390 2678 ...
##   ..$ : int  87 375 663 951 1239 1527 1815 2103 2391 2679 ...
##   ..$ : int  88 376 664 952 1240 1528 1816 2104 2392 2680 ...
##   ..$ : int  89 377 665 953 1241 1529 1817 2105 2393 2681 ...
##   ..$ : int  90 378 666 954 1242 1530 1818 2106 2394 2682 ...
##   ..$ : int  91 379 667 955 1243 1531 1819 2107 2395 2683 ...
##   ..$ : int  92 380 668 956 1244 1532 1820 2108 2396 2684 ...
##   ..$ : int  93 381 669 957 1245 1533 1821 2109 2397 2685 ...
##   ..$ : int  94 382 670 958 1246 1534 1822 2110 2398 2686 ...
##   ..$ : int  95 383 671 959 1247 1535 1823 2111 2399 2687 ...
##   ..$ : int  96 384 672 960 1248 1536 1824 2112 2400 2688 ...
##   ..$ : int  97 385 673 961 1249 1537 1825 2113 2401 2689 ...
##   ..$ : int  98 386 674 962 1250 1538 1826 2114 2402 2690 ...
##   .. [list output truncated]
```

```r
head(weekfulldata)
```

```
## Source: local data frame [6 x 4]
## Groups: interval
## 
##       steps       date interval    week
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

 Calculate mean


```r
weekfulldata_mean <-
weekfulldata %>%
group_by(interval, week) %>%
        summarise(steps= mean(steps)) %>%
print
```

```
## Source: local data frame [576 x 3]
## Groups: interval
## 
##    interval    week       steps
## 1         0 weekday 2.251153040
## 2         0 weekend 0.214622642
## 3         5 weekday 0.445283019
## 4         5 weekend 0.042452830
## 5        10 weekday 0.173165618
## 6        10 weekend 0.016509434
## 7        15 weekday 0.197903564
## 8        15 weekend 0.018867925
## 9        20 weekday 0.098951782
## 10       20 weekend 0.009433962
## ..      ...     ...         ...
```


Draw a graph comparison


```r
ggplot(data = weekfulldata_mean, aes(y=steps, x=interval)) +
facet_grid(week ~ .) + 
geom_line(color="red")  +      
labs(y = "Average number of Steps") + 
labs(x = "Interval") + 
labs(title = "Weekend Vs. Weekday (Average Steps taken per Interval)")
```

![plot of chunk plot final graph](figure/plot final graph-1.png) 


