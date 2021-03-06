# Reproducible Research:


## Loading and preprocessing the data
Data has been provided for this assignment as a zip file in the directory 
created by the assignment fork. 
Two pakages are used in this assignment: **dplyr** to support data wrngling and 
carpentry, and **ggplot2** for graphical output.  The choice was because I
need more practice with dplyr and ggplot2 is the graphics package I use the most
and am most familiar with. 


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```
The following code checks for the existance of the unzipped,
csv format file and only runs *unzip* if the file is not present.  
Once the data is read, the date field, which originally is a character
variable is converted to Date type by nterpreting it as YYYY-MM-DD.  
The data frame *activity* is then converted into a data frame tbl for
convenient use with plyr.  The *interval* variable received in the data uses
an integer with starting interval time formatted like a digital clock with
the first two digits as hours (24 hour format) and the second two digits as
mintes.  Consequently, 0155 is followed by 0200.  A new integer variable 
*minint* is created with the time as minutes in the day.  A factor version of 
*minint* is created as *finterval*.  (I should confess that I am still not sure 
when it is better to create factor variables in R rather than using *as.factor*
on the fly.  Also, please note that due to the descriptive text surrounding each
code chunk less comments are embedded with the R code.  Normal commenting of the
R code seems redundant.)


```r
if (!file.exists("activity.csv")) {
    print("unzipping...")
    unzip("activity.zip")
}

# now that we an unzipped data file we can make some adjustments
# prior to starting analysis.

activity <- read.csv(file="./activity.csv")

activity$date <- as.Date(activity$date, format='%Y-%m-%d')

activity_df <- tbl_df(activity)
activity_df$minint <- activity$interval %/% 100 * 60 + 
    activity$interval %% 100
activity_df$finterval <- as.factor(activity_df$minint)
```
Ready to start analysis.

## What is mean total number of steps taken per day?

For this part of the assignment we were instructed to "ignore the 
missing values". Default options were left alone so that
routines stripped missing data before analysis or graphical processing.  
Using *dplyr*, the *activity_df* was grouped by date and then the total number 
of steps per day was calculated.  The consolidated results were saved in 
*steps_by_day*.


```r
steps_by_day <- activity_df %>%
    group_by(date) %>%
    summarize(total = sum(steps))
```

A histogram is shown below to give a rough idea of the distribution of total 
steps per day.  Histograms are somewhat sentisitive to the choice of bin size. 
There are only a total of 61 days in the data. Since the count of days for any 
bin never exceeds 10 (and the count is integer), the result is a little blocky 
looking.  


```r
sprintf("number of days = %d", nrow(steps_by_day))
```

```
## [1] "number of days = 61"
```

```r
ggplot(steps_by_day, aes(x=total)) +
    geom_histogram(binwidth=1000, colour="black", fill="white") +
    xlab("Total Steps per Day") + ylab("count (days)") +
    scale_y_continuous(breaks=c(2, 4, 6, 8, 10))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

A quick look at the median and mean using *summary()* reveals that mean steps is
10770 and median steps is 10760.  This is remarkably close!


```r
summary(steps_by_day$total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```
## What is the average daily activity pattern?

Now we move from grouping by date to grouping by interval.  This gives us the 
opportunity to look for patterns in daily activity cycles.
Initially we want to look at activity variation through the day averaging all 
dates together.
The methods for doing this is very similar to that used above with data now
grouped by *finterval* and the summarized parameter is now *mean* rather than
*total*.  In addition, the plot used is a time series view of average steps
per 5 minute interval.  (Note there are 288 time intervals in a day.)

```r
#activity_df$finterval <- as.factor(activity_df$interval)

steps_by_interval <- activity_df %>%
    group_by(finterval) %>%
    summarize(intavg = mean(steps, na.rm=TRUE))

steps_by_interval$ival <- as.integer(steps_by_interval$finterval)

ggplot(steps_by_interval, aes(ival, intavg)) +
    geom_line() + xlab("5 minute time interval") + 
    ylab("average steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

The peak (or interval with maximum steps) in the above graph is at interval 104
with approximately 206 steps.

```r
max(steps_by_interval$intavg)
```

```
## [1] 206.1698
```

```r
which.max(steps_by_interval$intavg)
```

```
## [1] 104
```

```r
hour <- (which.max(steps_by_interval$intavg) * 5) %/% 60
minute <- (which.max(steps_by_interval$intavg) * 5) %% 60
print(sprintf("%d:%d", hour, minute), quote=FALSE)
```

```
## [1] 8:40
```
That peak interval is the 5 minutes starting at 08:40
## Imputing missing values

How many missing values do we have in the data?

```r
sum(is.na(activity_df$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity_df$steps))/nrow(activity_df)*100
```

```
## [1] 13.11475
```
This is a significant amount of the data - approximately 1 in 8 measurements is
an NA.  
The method chosen for imputing missing values is to use the median value for
the interval of the missing value.  First the medians for each interval are
calculated using the equivalent method to that used in calculating
*steps_by_day* and *steps_by_interval*.  In this case we generate *medsteps*
which contains the median steps for each interval.  Since we would like to keep this as a new set of data we copy *activity_df* to *activity_df2* and then loop through filling
*NAs* with the appropriate median value from *medsteps*.

```r
medsteps <- activity_df %>%
    group_by(finterval) %>%
    summarize(intmed = median(steps, na.rm=TRUE))

activity_df2 <- activity_df

for (i in 1:nrow(activity_df2)) {
    if (is.na(activity_df2$steps[i])) {
        activity_df2$steps[i] <- medsteps$intmed[activity_df2$minint[i]/5+1]
    }
}

sum(is.na(activity_df2$steps))
```

```
## [1] 0
```
An inspection of *medsteps* shows that quite a few medians are 0, which might 
suggest that other methods for imputation might be better.  However, for this
assignment median is suffcient. Having completed that step we can now take a 
look at histogram of total steps per day and compare that with the first 
histogram that was generated before the *NAs* were processed.

```r
steps_by_day2 <- activity_df2 %>%
    group_by(date) %>%
    summarize(total = sum(steps))

ggplot(steps_by_day2, aes(x=total)) +
    geom_histogram(binwidth=1000, colour="black", fill="white") +
    xlab("Total Steps per Day") + ylab("count (days)") +
    scale_y_continuous(breaks=c(2, 4, 6, 8, 10))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
Some differences can been seen between the two histograms, but the location of 
the peak has not moved significantly.  A second peak in the 1000 to 2000 steps
per day has emerged that might need some consideration to determine if it is
actually an artifact of the imputation method.  The effect of this can be seen 
in the summary below where the mean has been dragged down much more than the 
median.


```r
summary(steps_by_day2$total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10400    9504   12810   21190
```
## Are there differences in activity patterns between weekdays and weekends?
To answer the final question we need to determine the day of the week.  To do 
this we use the *weekdays()* function to generate a new variable in 
*activity_df2* contaiing the name of the day.  We then create another variable
that will contain the classification of that day as *weekday* or *weekend*.
All days are initially classified as *weekday* prior to looping through 
*activity_df2* to identfy which *day*s contain "Saturday" or "Sunday", which
are then classifed as "weekend".  

```r
activity_df2$day <- weekdays(activity_df2$date)
activity_df2$daytype <- "weekday"
activity_df2$daytype[activity_df2$day=="Saturday" | 
                         activity_df2$day=="Sunday"] <- "weekend"
activity_df2$daytype <- as.factor(activity_df2$daytype)

steps_by_weekday <- activity_df2 %>%
    group_by(daytype, finterval) %>%
    summarize(intavg = mean(steps, na.rm=TRUE))
steps_by_weekday$ival <- as.integer(steps_by_weekday$finterval)

ggplot(steps_by_weekday, aes(ival, intavg)) +
    geom_line() + facet_grid(daytype ~ .) +
    xlab("5 minute time interval") + ylab("average steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
The superficial answer to is there a difference between weekends and weekdays
is "yes".  This may be less than expected and there may only be a few 
significant differences.  For example, the shape of the start of the day shows
a shoulder is missing on weekends that may represent less people getting up to 
go to work.  
Clearly further work is required! ;-)
