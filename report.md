# Analyisis to determine economic impact of severe weather events using NOAA storm data
Telvis Calhoun  
December 26, 2015  

## Introduction

Title: Analyisis to determine economic impact of severe weather events using NOAA storm data

Synopsis: In this document, we analyze the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database to determine the economic impact of different weather events. The goal of the analysis is to rank the storm events based on their economic impact. Our hope is that this ranking will help policy-makers to allocate emergency management funds to the events with the greatest economic impact.  

## Data Processing


First, lets load libraries used in the analysis


```r
library(lubridate)
library(dplyr)
```

Now we will fetch the data and load CSV file. The file in bzip'd so we'll use bzfile()

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              "repdata-data-StormData.csv.bz2", method = "curl")
df <- read.csv(bzfile("repdata-data-StormData.csv.bz2"), na.strings="", stringsAsFactors = FALSE)
```

### EVTYPE Cleanup
There are several issues with the raw NOAA data. The values in the `EVTYPE` column are not consistent for the same event type. One example is "THUNDERSTORM WIND" : It appears in the data as variations of "TSTM WIND", "THUNDERSTORM WINDS" and many other values that include arbitrary string. Let's attempt to normalize those variations.


```r
  # fix thunderstorm wind labels. Being careful not to change 'MARINE THUNDERSTORM WIND'.
  df <- mutate(df, EVTYPE=ifelse(grepl("^(?!MARINE)?\\s*TSTM", EVTYPE, perl=TRUE), "THUNDERSTORM WIND", EVTYPE))
  df <- mutate(df, EVTYPE=ifelse(grepl("^(?!MARINE)?\\s*THUNDERSTORM", EVTYPE, perl=TRUE), "THUNDERSTORM WIND", EVTYPE))
  
  # fix marine thunderstorm winds
  df <- mutate(df, EVTYPE=ifelse(grepl("MARINE\\s*TSTM", EVTYPE, perl=TRUE), "MARINE THUNDERSTORM WIND", EVTYPE))
  df <- mutate(df, EVTYPE=ifelse(grepl("MARINE\\s*THUNDERSTORM", EVTYPE, perl=TRUE), "MARINE THUNDERSTORM WIND", EVTYPE))
```

NOTE: There are probably other event types that coud be cleaned up. "THUNDERSTORM WIND" was the only value that impacted the "Top N" analysis. 

Finally, let's convert `EVTYPE` to a factor variable


```r
df <- transform(df, EVTYPE=factor(EVTYPE))
```

### PROPDMGEXP Cleanup

The `PROPDMGEXP` column defines the power of 10 to multiply by `PROPDMG` to calculate the Property Damage in Dollars. Unfornately, the values in this column were string integers (e.g. "1", "2") or humanized abbreviations (e.g. K = thousands of dollars). There were also strange values like `+` and `?`. Let's clean this up.


```r
# Fix the prop dmg exponent. 
table(df$PROPDMGEXP)
```

```
## 
##      -      ?      +      0      1      2      3      4      5      6 
##      1      8      5    216     25     13      4      4     28      4 
##      7      8      B      h      H      K      m      M 
##      5      1     40      1      6 424665      7  11330
```

```r
# function to perform the tranformation from the various input values
map_propdmgexp <- function(x){
  x <- x[[1]]
  
  if (is.na(x)){
    ret = NA
  } else if ( x %in% c("-","?","+")){
    ret = NA 
  } else if (toupper(x) %in% c("B", "K", "M", "H")){
    exp_mappings <- c("B"=9, "H"=2, "K"=3, "M"=6)
    ret = exp_mappings[toupper(x)]
  } else {
    # assume its a number string
    ret = as.numeric(x)
    if (is.na(ret)){
      stop(sprintf("Unexpected 'NA' %s", x))
    }
  }
  ret
}

# use mapply and map_propdmgexp() to convert 'exp' values
df$PROPDMGEXP_mapped <- mapply(map_propdmgexp, df$PROPDMGEXP)

# check the values now. should all be integer strings
table(df$PROPDMGEXP_mapped)
```

```
## 
##      0      1      2      3      4      5      6      7      8      9 
##    216     25     20 424669      4     28  11341      5      1     40
```

```r
# Now lets calculate the property damage
df$TOTALCOST <- df$PROPDMG * 10 ** df$PROPDMGEXP_mapped
```


## Results
TODO....
- At most 3 plots

