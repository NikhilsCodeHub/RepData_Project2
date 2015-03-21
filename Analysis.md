# Analysis Of US Severe Weather Data and its Impact on Population and Property

### Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.
The Below Analysis is conducted to identify the events which are most damaging to Population and Property.

####1. Load Libraries

```r
library(knitr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
```

####2. Load the datafile.


```r
##  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","stormData.csv.bz2", method="curl")
## unzip("stormData.csv.bz2", "stormData2.csv")
## data<-read.csv("stormData.csv", header=TRUE, stringsAsFactors=FALSE)
data<-tbl_df(data)
```


####3.Cleaning Data
Taking a look at the current range of EVTYPE, we see that there are quite some similar categories which can be renamed and regrouped for accurancy.
Below we try to identify and rename most of those cases.


```r
data[grep("THUNDER",data$EVTYPE),"EVTYPE"]<-"THUNDERSTORM"
data[grep("TORNADO",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"TORNADO"
data[grep("WIND",data$EVTYPE, ignore.case=TRUE), "EVTYPE"]<-"WIND"

data[grep("FUNNEL",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"TORNADO"
data[grep("HURRICANE",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"HURRICANE"
data[grep("SNOW",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"SNOW"
data[grep("BLIZZ",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"SNOW"
data[grep("ICE",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"ICE"
data[grep("SLEET",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"ICE"
data[grep("ICY",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"ICE"
data[grep("FREEZ",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"ICE"
data[grep("FLOOD",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"FLOODING"
data[grep("LIGHTN",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"LIGHTNING"
data[grep("HEAT|HOT|WARM",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"HEAT WAVE"
data[grep("COLD|LOW",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"COLD WAVE"
data[grep("WINTER",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"SNOW"
data[grep("WATERSPOUT",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"TORNADO"
data[grep("(TROPICAL|COASTAL)(.|)STORM",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"HURRICANE"
data[grep("RAIN",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"RAINFALL"
data[grep("HAIL",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"HAIL"
data[grep("FIRE",data$EVTYPE, ignore.case=TRUE),"EVTYPE"]<-"FOREST FIRE"
```


####4. Impact on Population (Injuries and Fatalities)

Here we'll analyse which events have caused the most damage to Population causing Injuries and Fatalities. 
 - First select on those columns required for analysis.
 -


```r
PopImpactDF<- select(data, STATE, contains("DMG"), FATALITIES, INJURIES, EVTYPE)
PopImpactDF <- PopImpactDF %>% filter(!(FATALITIES<1 & INJURIES <1))


PopImpactSummaryDF<-group_by(PopImpactDF, EVTYPE) %>% summarise(Total_Fatalities=sum(FATALITIES), Total_Injuries=sum(INJURIES), Total_Events=n()) %>% mutate(Total_Affected=Total_Injuries+Total_Fatalities
) %>% arrange(desc(Total_Affected))



head(PopImpactSummaryDF,10)
```

```
## Source: local data frame [10 x 5]
## 
##          EVTYPE Total_Fatalities Total_Injuries Total_Events
## 1       TORNADO             5664          91439         7942
## 2     HEAT WAVE             3178           9243          946
## 3          WIND             1216           9059         4094
## 4      FLOODING             1525           8602         1410
## 5     LIGHTNING              817           5231         3307
## 6          SNOW              541           3803          613
## 7  THUNDERSTORM              212           2480         1067
## 8           ICE              115           2223          139
## 9     HURRICANE              203           1713          116
## 10  FOREST FIRE               90           1608          333
## Variables not shown: Total_Affected (dbl)
```

```r
ggplot(PopImpactSummaryDF[1:10,])+geom_bar(aes(EVTYPE, Total_Fatalities), stat="identity", fill="navy")+geom_bar(aes(EVTYPE, Total_Injuries), stat="identity", fill="brown")
```

![](Analysis_files/figure-html/PopulationData-1.png) 



Looking at the Economic Impact


```r
econImpactDF<- select(data, STATE, contains("DMG"), FATALITIES, INJURIES, EVTYPE)
econImpactDF <- econImpactDF %>% filter(!(PROPDMG<1 & CROPDMG <1)) %>% select(-FATALITIES, -INJURIES)

unique(econImpactDF$PROPDMGEXP)
```

```
##  [1] "K" "M" "B" "m" ""  "+" "0" "5" "6" "4" "h" "2" "7" "3" "H" "-"
```

```r
unique(econImpactDF$CROPDMGEXP)
```

```
## [1] ""  "M" "K" "m" "0" "k" "B" "?"
```

```r
for(val in unique(econImpactDF$PROPDMGEXP)) 
    econImpactDF[econImpactDF["PROPDMGEXP"]==val,"PROPEXP"]<-10^switch(EXPR=val, K =3, M = 6,B=9, H=2, 0)

for(val in unique(econImpactDF$CROPDMGEXP)) 
    econImpactDF[econImpactDF["CROPDMGEXP"]==val,"CROPEXP"]<-10^switch(EXPR=val, K =3, M = 6,B=9, H=2, 0)

econImpactDF<-econImpactDF %>% mutate(PROPDMG=PROPDMG*PROPEXP, CROPDMG=CROPDMG*CROPEXP) %>% select(-CROPEXP, -PROPEXP)

econImpactSummaryDF<-econImpactDF%>% mutate(Damages=PROPDMG+CROPDMG) %>% group_by(EVTYPE) %>% summarise(Total_Damages=sum(Damages), Total_events=n()) %>% arrange(desc(Total_Damages))
```

### Results 

Here we'll tabulate the top 10 events that were most damaging from human Population perspective.
<Table of top 10 Events>

Here we'll tabulate the top 10 events that were most damaging from Economic perspective.
<Table of top 10 Events>
