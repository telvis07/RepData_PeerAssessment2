library(dplyr)
library(lubridate)


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

clean_data <- function(df = NULL) {
  
  if(is.null(df)){
    df <- read.csv(bzfile("repdata-data-StormData.csv.bz2"), na.strings="", stringsAsFactors = FALSE)
  }

  # fix thunderstorm wind labels
  df <- mutate(df, EVTYPE=ifelse(grepl("^(?!MARINE)?\\s*TSTM", EVTYPE, perl=TRUE), "THUNDERSTORM WIND", EVTYPE))
  df <- mutate(df, EVTYPE=ifelse(grepl("^(?!MARINE)?\\s*THUNDERSTORM", EVTYPE, perl=TRUE), "THUNDERSTORM WIND", EVTYPE))
  
  # fix marine thunderstorm winds
  df <- mutate(df, EVTYPE=ifelse(grepl("MARINE\\s*TSTM", EVTYPE, perl=TRUE), "MARINE THUNDERSTORM WIND", EVTYPE))
  df <- mutate(df, EVTYPE=ifelse(grepl("MARINE\\s*THUNDERSTORM", EVTYPE, perl=TRUE), "MARINE THUNDERSTORM WIND", EVTYPE))
  
  # Fix the prop dmg exponent. 
  table(df$PROPDMGEXP)
  
  # use mapply and map_propdmgexp() to convert 'exp' values
  df$PROPDMGEXP_mapped <- mapply(map_propdmgexp, df$PROPDMGEXP)
  df$TOTALCOST <- df$PROPDMG * 10 ** df$PROPDMGEXP_mapped
  
  # event type to factor
  df <- transform(df, EVTYPE=factor(EVTYPE))
  
  # begin date to Date
  df <- transform(df, BGN_DATE=mdy_hms(BGN_DATE))
  df
}

do_the_plots <- function(top_lists){
  top_injuries_by_evtype <- top_lists$top_injuries_by_evtype
  top_prop_dmg_by_evtype <- top_lists$top_prop_dmg_by_evtype
  top_fatalities_by_evtype <- top_lists$top_fatalities_by_evtype

  barplot(height=top_fatalities_by_evtype$FATALITIES, 
          names.arg = top_fatalities_by_evtype$EVTYPE, 
          cex.names=0.75, 
          col = "wheat",
          ylab = "Number of Fatalities", 
          xlab = "Storm Data Event Type", 
          main="Top 5 Storm Event Types by Fatalities from 1950 - November 2011")

  barplot(height=top_injuries_by_evtype$INJURIES, 
          names.arg=top_injuries_by_evtype$EVTYPE, 
          cex.names = 0.75, 
          col = "wheat",
          ylab = "Number of Injuries", 
          xlab = "Storm Data Event Type", 
          main="Top 5 Storm Event Types by Injuries from 1950 - November 2011")

  barplot(height=top_prop_dmg_by_evtype$TOTALCOST, 
          names.arg = top_prop_dmg_by_evtype$EVTYPE, 
          cex.names=0.75,
          col = "wheat",
          ylab = "Total Property Damage (in dollars)", 
          xlab = "Storm Data Event Type", 
          main="Top 5 Storm Event Types by Property Damage (in dollars) from 1950 - November 2011")
}

summarize_data <- function(df=NULL) {
  if (is.null(df)){
    df <- clean_data()
  }
  
  by_evtype <- group_by(df, EVTYPE)
  
  # property damage by event type
  prop_dmg_by_evtype <- summarize(by_evtype, TOTALCOST=sum(TOTALCOST, na.rm=TRUE)) %>% arrange(desc(TOTALCOST))
  print("Top 10 Event Types by Property Damage")
  print(head(prop_dmg_by_evtype, n=10))
  
  # Injuries by event type 
  injuries_by_evtype <- summarize(by_evtype, INJURIES=sum(INJURIES, na.rm=TRUE)) %>% arrange(desc(INJURIES))
  print("Top 10 Event Types by Injuries")
  print(head(injuries_by_evtype, n=10))
  
  # Fatalities by event type
  fatalities_by_evtype <- summarize(by_evtype, FATALITIES=sum(FATALITIES, na.rm=TRUE)) %>% arrange(desc(FATALITIES))
  print("Top 10 Event Types by Fatalities")
  print(head(fatalities_by_evtype, n=10))
  
  # return a list with data summaries
  list(top_injuries_by_evtype = head(injuries_by_evtype, n=5),
       top_prop_dmg_by_evtype = head(prop_dmg_by_evtype, n=5),
       top_fatalities_by_evtype = head(fatalities_by_evtype, n=5))
}

