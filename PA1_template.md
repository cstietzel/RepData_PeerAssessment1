# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Throughout the project, functions will be used from the "tidyverse" collection of packages for data science <a href= http://tidyverse.tidyverse.org>http://tidyverse.tidyverse.org</a>. 


```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```
To aid in the displpay of tabular data, the xtable package will be used. A fix to the xtable function is also applied to allow for better date value display.
library(xtable)


```r
xtable <- function(x, ...) {
    for (i in which(sapply(x, function(y)   !all(is.na(match(c("POSIXt","Date"),class(y))))))) x[[i]] <- as.character(x[[i]])
    xtable::xtable(x, ...)
}
```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
