---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---
Eric Byrnes


## Synopsis
This analysis is of personal activity data (steps) gathered from an activity monitoring device such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). The exact device used to collect the data was not specified. Data is collected in 5 minute increments (more precisely, the number of steps during each increment) between Oct. 1, 2012 and Nov. 30, 2012, inclusive. Unavailable data is removed or imputed, then plotted and weekday/weekend means and medians calculated.


## Environment
The locations of source and download files are detailed here and may be changed if necessary. This analysis will use the values below to download a zip file with a single CSV file containing the data. The zip will be stored to the value given below and unzipped to start the analysis.

#### Analysis Configuration

```r
### Configuration variables set here
# set remote zip filename here - you may comment this line out in order to use
# data that has already been downloaded to data.filepath
download.filepath <- "https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip"
# set local zip filename here - this zip is assumed to have one file which will
# be loaded as a CSV
data.filepath <- "./activity.zip"
```

In the following section, required packages are loaded and the execution environment described for reproducibility. The analysis depends on the library *mice* for data imputation and *lattice* for plotting; these are installed and loaded as needed.

```r
# store run date
run.date <- date()

packages.required <- c("mice", "lattice")
packages.needed <- packages.required[!(packages.required %in% installed.packages()[ ,"Package"])]
if (length(packages.needed))
   install.packages(packages.needed, repos="http://cran.rstudio.com/")
library(mice, quietly = TRUE)
library(lattice, quietly = TRUE)

sessionInfo()
```

```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] mice_2.22       Rcpp_0.11.3     knitr_1.8       lattice_0.20-29
## 
## loaded via a namespace (and not attached):
##  [1] digest_0.6.4        evaluate_0.5.5      formatR_1.0        
##  [4] grid_3.1.2          htmltools_0.2.6     markdown_0.7.4     
##  [7] MASS_7.3-35         mime_0.2            nnet_7.3-8         
## [10] randomForest_4.6-10 rmarkdown_0.3.11    rpart_4.1-8        
## [13] stringr_0.6.2       tools_3.1.2         yaml_2.1.13
```

The analysis was ron on Sat Jan 17 19:21:04 2015. 


## Loading and preprocessing the data
This analysis is of personal activity data (steps) gathered from an activity monitoring device such as a
[Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). The exact device used to extract the measurements used here was not specified.

The data for this assignment is available from the Reproducible Research course web site; it is also downloaded automatically to generate this document (these locations may be configured in the first code block of this document):

* Source Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip)
* Stored locally at: ./activity.zip

The downloaded data is summarized below:

```r
# download zip file from source location and unzip; not normally required but
# included for full reproducibility
if (exists("download.filepath")) {
   download.file(download.filepath, data.filepath, "curl", quiet = TRUE)
   data.filepaths <- unzip(data.filepath, junkpaths = TRUE)
   data.filepath <- data.filepaths[1]
} else {
   data.filepaths <- unzip(data.filepath, list = TRUE, junkpaths = TRUE)
   if (! file.exists(data.filepaths[1]$Name)) {
      data.filepaths <- unzip(data.filepath, junkpaths = TRUE)
      data.filepath <- data.filepaths[1]
   } else {
      data.filepath <- data.filepaths[1]$Name
   }
}
```

```
## Warning: running command 'curl -s -S
## "https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip" -o
## "./activity.zip"' had status 127
```

```
## Warning in download.file(download.filepath, data.filepath, "curl", quiet =
## TRUE): download had nonzero exit status
```

```r
# load CSV data
this.data.raw <- read.csv(data.filepath,
                          colClasses = c("integer", "Date", "integer"),
                          stringsAsFactors = FALSE)
this.data.raw$interval <- as.factor(this.data.raw$interval)

# summarize data
summary(this.data.raw)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                          (Other):17202
```

Unzipped CSV data is available at ./activity.csv. The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17568 observations in this dataset.

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken


#### Data Cleaning
THe data cleanup strategy is in two parts:

* Remove days for which all values are NA. The code checks each date for at least one valid (non-NA) step data value; if none exists, the entire day's data is removed from the data set.
* Impute data. The library *mice* is used to impute data.

#####Step 1: Remove days for which all values are NA

```r
this.data <- this.data.raw
# remove days with all NA values for the whole day
for (this.date in unique(this.data$date)) {
   this.date.data <- this.data[this.data$date == this.date, ]
   if (all(is.na(this.date.data$steps)))
      this.data <- this.data[!(this.data$date == this.date), ]
}

summary(this.data)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-02   0      :   53  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   53  
##  Median :  0.00   Median :2012-10-29   10     :   53  
##  Mean   : 37.38   Mean   :2012-10-30   15     :   53  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   20     :   53  
##  Max.   :806.00   Max.   :2012-11-29   25     :   53  
##                                        (Other):14946
```

`this.data` contains only days that have at least one non-NA value for *steps* - that is, all days wihch have all NAs for this value have been removed.

#####Step 2: Impute missing values within data

```r
# impute missing values
this.data.imputed <- this.data
if (any(is.na(this.data.imputed$steps))) {
   this.data.imputed.predictors <- data.frame(this.data.imputed$steps,
                                              lag(this.data.imputed$steps, shift = 1),
                                              lag(this.data.imputed$steps, shift = -1),
                                              as.numeric(this.data.imputed$date),
                                              this.data.imputed$interval)
   this.data.imputed.temp <- complete(mice(this.data.imputed.predictors, seed = 1, method = "norm.nob"))
   this.data.imputed$steps <- as.integer(round(this.data.imputed.temp[, 1]))
}
```

`this.data.imputed` refines `this.data` by imputing an further misssing values using *mice*, if any. Exttraneous output from *mice* is not displayed here. A predictive matrix was constructed using the steps led and lagged by one (in order to bias the imputed value toward values just before and after the missing value, if available), and the date and interval (to bias the selected value toward values with this value). **This is an open-ended anb subjective area of the analysis as different predictors and methods are available!** For this analysis, the *norm.nob* (Linear regression ignoring model error) method was selected. See *?mice* for details of available methods.


## Analysis
### What is mean total number of steps taken per day?
The following uses the raw data (prior to cleaning and data imputation) so that all days appear in the data, even if they have all NA values. The first plot shows a bar chart of total number of steps per day and the second shows a histogram of the same.

```r
# create data
this.data.stepsonly <- this.data[this.data$steps > 0, ]
this.data.bydate <- aggregate(steps ~ date, data = this.data.stepsonly, sum)
names(this.data.bydate)[2] <- "steps"

# total mean and median by date
this.data.bydate$mean <- cbind(aggregate(steps ~ date, data = this.data.stepsonly, mean)$steps)
this.data.bydate$median <- cbind(aggregate(steps ~ date, data = this.data.stepsonly, median)$steps)
# total mean and median across data set
this.data.bydate.mean <- mean(this.data.bydate$steps, na.rm = TRUE)
this.data.bydate.median <- median(this.data.bydate$steps, na.rm = TRUE)

# barchart plot by date
date.labels <- seq(1, nrow(this.data.bydate), by = 15)
barchart(steps ~ date, data = this.data.bydate,
         xlab = "Date", ylab = "Steps",
         horizontal = FALSE,
         ylim = c(0, max(this.data.bydate$steps) * 1.05),
         scales = list(x = list(labels = format(this.data.bydate$date, "%b %d")[date.labels],
                                at = date.labels)),
         panel = function(...) {
            panel.grid(h = -1, v = 3)
            panel.barchart(...)
         })
```

![plot of chunk analysis1](figure/analysis1-1.png) 


```r
# histogram plot of total steps
histogram(this.data.bydate$steps,
          xlab = "Steps",
          ylab = "Frequency",
          breaks = 8,
          xlim = c(0, max(this.data.bydate$steps) * 1.05),
          ylim = c(0, 35))
```

![plot of chunk analysis1b](figure/analysis1b-1.png) 

* Mean value: 10766.19 steps
* Median value: 10765 steps

#### Mean/Median steps by day

```r
this.data.bydate
```

```
##          date steps      mean median
## 1  2012-10-02   126  63.00000   63.0
## 2  2012-10-03 11352 140.14815   61.0
## 3  2012-10-04 12116 121.16000   56.5
## 4  2012-10-05 13294 154.58140   66.0
## 5  2012-10-06 15420 145.47170   67.0
## 6  2012-10-07 11015 101.99074   52.5
## 7  2012-10-09 12811 134.85263   48.0
## 8  2012-10-10  9900  95.19231   56.5
## 9  2012-10-11 10304 137.38667   35.0
## 10 2012-10-12 17382 156.59459   46.0
## 11 2012-10-13 12426 119.48077   45.5
## 12 2012-10-14 15098 160.61702   60.5
## 13 2012-10-15 10139 131.67532   54.0
## 14 2012-10-16 15084 157.12500   64.0
## 15 2012-10-17 13452 152.86364   61.5
## 16 2012-10-18 10056 152.36364   52.5
## 17 2012-10-19 11829 127.19355   74.0
## 18 2012-10-20 10395 125.24096   49.0
## 19 2012-10-21  8821  96.93407   48.0
## 20 2012-10-22 13460 154.71264   52.0
## 21 2012-10-23  8918 101.34091   56.0
## 22 2012-10-24  8355 104.43750   51.5
## 23 2012-10-25  2492  56.63636   35.0
## 24 2012-10-26  6778  77.02273   36.5
## 25 2012-10-27 10119 134.92000   72.0
## 26 2012-10-28 11458 110.17308   61.0
## 27 2012-10-29  5018  80.93548   54.5
## 28 2012-10-30  9819 110.32584   40.0
## 29 2012-10-31 15414 179.23256   83.5
## 30 2012-11-02 10600 143.24324   55.5
## 31 2012-11-03 10571 117.45556   59.0
## 32 2012-11-05 10439 141.06757   66.0
## 33 2012-11-06  8334 100.40964   52.0
## 34 2012-11-07 12883 135.61053   58.0
## 35 2012-11-08  3219  61.90385   42.5
## 36 2012-11-11 12608 132.71579   55.0
## 37 2012-11-12 10765 156.01449   42.0
## 38 2012-11-13  7336  90.56790   57.0
## 39 2012-11-15    41  20.50000   20.5
## 40 2012-11-16  5441  89.19672   43.0
## 41 2012-11-17 14339 183.83333   65.5
## 42 2012-11-18 15110 162.47312   80.0
## 43 2012-11-19  8841 117.88000   34.0
## 44 2012-11-20  4472  95.14894   58.0
## 45 2012-11-21 12787 188.04412   55.0
## 46 2012-11-22 20427 177.62609   65.0
## 47 2012-11-23 21194 252.30952  113.0
## 48 2012-11-24 14478 176.56098   65.5
## 49 2012-11-25 11834 140.88095   84.0
## 50 2012-11-26 11162 128.29885   53.0
## 51 2012-11-27 13646 158.67442   57.0
## 52 2012-11-28 10183 212.14583   70.0
## 53 2012-11-29  7047 110.10938   44.5
```


### What is the average daily activity pattern?
The following uses the raw data (prior to cleaning and data imputation).

```r
this.data.byint <- aggregate(this.data$steps,
                             list(interval = this.data$interval),
                             sum, na.rm = TRUE)
names(this.data.byint) <- c("interval", "steps")
this.data.byint <- this.data.byint[
   order(as.integer(as.character(this.data.byint[, 1]))), ]
row.names(this.data.byint) <- 1:nrow(this.data.byint)

this.data.byint.mean <- mean(this.data.byint$steps, na.rm = TRUE)
this.data.byint.max_item <- this.data.byint[which.max(this.data.byint$steps), ]
this.data.byint.max <- this.data.byint.max_item$steps
this.data.byint.maxint <- as.character(this.data.byint.max_item$interval)

# line plot - average steps per day
int.labels <- seq(1, nrow(this.data.byint), by = 48)
xyplot(steps ~ interval, data = this.data.byint, type = "l",
       xlab = "Interval", ylab = "Mean Steps",
       ylim = c(0, this.data.byint.max * 1.05),
       scales = list(x = list(labels = as.character(this.data.byint$interval)[int.labels],
                              at = int.labels)),
       panel = function(...) {
          panel.grid(h = -1, v = 5)
          panel.xyplot(...)
       })
```

![plot of chunk analysis2](figure/analysis2-1.png) 

* Mean value: 1981.278 steps
* Maximum value: 10927 steps
* Interval with maximum value: 835



### Imputing missing values
Missing values for the *steps* dimension were handled using the data cleaning strategy outlined above. Missing values were calculated as follows:

```r
# number of NAs in original set
this.data.raw.na <- sum(is.na(this.data.raw$steps))
# number of NAs after removing days with all NA
this.data.na <- sum(is.na(this.data$steps))
# number of NAs after removing NAs within days that have data
this.data.imputed.na <- sum(is.na(this.data.imputed$steps))
```

##### Number of missing *steps* values
* In original data set: 2304
* After removing days with all NA steps: 0
* After removing other NAs (should be 0): 0


```r
# create data
this.data.bydate <- aggregate(steps ~ date, data = this.data.imputed, sum)
names(this.data.bydate)[2] <- "steps"

this.data.bydate.mean <- mean(this.data.imputed$steps, na.rm = TRUE)
this.data.bydate.median <- median(this.data.imputed$steps, na.rm = TRUE)

# histogram plot by date
barchart(steps ~ date, data = this.data.bydate,
         xlab = "Date", ylab = "Steps",
         horizontal = FALSE,
         ylim = c(0, max(this.data.bydate$steps) * 1.05),
         scales = list(x = list(labels = format(this.data.bydate$date, "%b %d")[date.labels],
                                at = date.labels)),
         panel = function(...) {
            panel.grid(h = -1, v = 3)
            panel.barchart(...)
         })
```

![plot of chunk analysis4](figure/analysis4-1.png) 


```r
# histogram plot of total steps
histogram(this.data.bydate$steps,
          xlab = "Steps",
          ylab = "Frequency",
          breaks = 8,
          xlim = c(0, max(this.data.bydate$steps) * 1.05),
          ylim = c(0, 35))
```

![plot of chunk analysis4b](figure/analysis4b-1.png) 

* Mean value: 37.3826 steps
* Median value: 0 steps

In this case, there were no mean/median differences in values because no NA values needed to be imputed within a given day that already had data.


### Are there differences in activity patterns between weekdays and weekends?
The following analysis classifies data as *weekday* or *weekend* and augments the data frame with this information. The mean number of steps across these types of days is then calculated.

```r
# augment data
this.data.bydaytype <- this.data.imputed
this.data.bydaytype$day_type <- as.factor(
   ifelse(weekdays(this.data.bydaytype$date) %in% c("Saturday","Sunday"),
          "Weekend", "Weekday"))
this.data.bydaytype <- aggregate(steps ~ interval + day_type,
                                 this.data.bydaytype,
                                 mean)
names(this.data.bydaytype)[3] <- "steps"
this.data.bydaytype$interval <- as.factor(this.data.bydaytype$interval)

# plot by day type
xyplot(steps ~ interval | day_type, data = this.data.bydaytype,
       type = "l", layout = c(1, 2),
       xlab = "Interval", ylab = "Mean Steps",
       ylim = c(0, max(this.data.bydaytype$steps) * 1.05),
       scales = list(x = list(labels = as.character(this.data.bydaytype$interval)[int.labels],
                              at = int.labels)),
       panel = function(...) {
          panel.grid(h = -1, v = 5)
          panel.xyplot(...)
       })
```

![plot of chunk analysis5](figure/analysis5-1.png) 

The weekend and weekday patterns show significant differences:

* Weekday activity is much higher in the early morning hours (between about 6AM and 8AM). This may represent concerted activity in preparation for work (versus a more relaxed morning pace on weekends).
* Weekday activity shows a high peak between 8AM and 9AM which may represent concentrated travel to a workplace.
* Weekend activity shows both higher activity and higher variability, which suggests free-form or unstructured activity vs. weekday "office" or more structured work.
* Weekend activity persists until at least 9PM, whereas weekday activity tapers off by 8PM.
