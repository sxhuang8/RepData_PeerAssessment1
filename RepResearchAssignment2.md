# Impact of severe weather events on population health and economic damages



Updated on Sun Jan 25 16:32:29 2015 by Sarah Huang

## Synopsis

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. The original data is processed so it is tidy for analysis. Two questions are investigated and answered: First, across the United States, which types of events are most harmful with respect to population health? Second, across the United States, which types of events have the greatest economic consequences? For the first question, we address the population health by examining the data on fatalities and injuries. For the second question, we address the economic consequences by using the data on property damage and crop damage. Summary data and figures are presented at the end of this document.

## Data Processing 

Load the data


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormData.csv.bz2")
```


```r
stormData <- read.csv("stormData.csv.bz2")
```

Here are the variables in the dataset.


```r
names(stormData)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

Subset the data. Select only the variables necessary to conduct the analysis. The necessary variables are: EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP.


```r
stormData <- subset(stormData, select = c(8, 23:28))
head(stormData) #a rough look at the subsetted data
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0          K       0           
## 2 TORNADO          0        0     2.5          K       0           
## 3 TORNADO          0        2    25.0          K       0           
## 4 TORNADO          0        2     2.5          K       0           
## 5 TORNADO          0        2     2.5          K       0           
## 6 TORNADO          0        6     2.5          K       0
```

```r
str(stormData) #a look at the data structure
```

```
## 'data.frame':	902297 obs. of  7 variables:
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Remove rows coded as "Summary", "?", "month" or "year" in variable EVTYPE.


```r
stormData <- stormData[!grepl("summary|year|month|county|southeast|other|none|^high$|\\?", stormData$EVTYPE, ignore.case = TRUE),]
```

Replace values of PROPDMGEXP with appropriate exponent. Rules: replace K with 3, M and m with 6, H and h with 2, B with 9, + and - with 0, ? with NA. There are also empty cells that correspond to zero damage so it is appropriate to replace these empty cells with 0. Change the variable from factor to numeric.


```r
stormData$PROPDMGEXP <- gsub("K", 3, stormData$PROPDMGEXP)
stormData$PROPDMGEXP <- gsub("M", 6, stormData$PROPDMGEXP, ignore.case = TRUE)
stormData$PROPDMGEXP <- gsub("H", 2, stormData$PROPDMGEXP, ignore.case = TRUE)
stormData$PROPDMGEXP <- gsub("B", 9, stormData$PROPDMGEXP)
stormData$PROPDMGEXP <- gsub("+", 0, stormData$PROPDMGEXP, fixed = TRUE)
stormData$PROPDMGEXP <- gsub("-", 0, stormData$PROPDMGEXP, fixed = TRUE)
stormData$PROPDMGEXP <- gsub("?", NA, stormData$PROPDMGEXP, fixed = TRUE)
stormData$PROPDMGEXP[stormData$PROPDMGEXP==""] <- 0 
stormData$PROPDMGEXP <- as.numeric(stormData$PROPDMGEXP)
```

Replace values of CROPDMGEXP with appropriate exponent. Rules: replace K and k with 3, M and m with 6, B with 9, ? with NA, all empty cells with 0. Change the variable from factor to numeric.


```r
stormData$CROPDMGEXP <- gsub("K", 3, stormData$CROPDMGEXP, ignore.case = TRUE)
stormData$CROPDMGEXP <- gsub("M", 6, stormData$CROPDMGEXP, ignore.case = TRUE)
stormData$CROPDMGEXP <- gsub("B", 9, stormData$CROPDMGEXP)
stormData$CROPDMGEXP <- gsub("?", NA, stormData$CROPDMGEXP, fixed = TRUE)
stormData$CROPDMGEXP[stormData$CROPDMGEXP==""] <- 0
stormData$CROPDMGEXP <- as.numeric(stormData$CROPDMGEXP)
```

Next, we need to tidy variable EVTYPE. This variable contains the names of the weather events and in many cases, there are entries of the same event that are worded differently. You can see this by calling the unique() function on EVTYPE. We will rename these entries of the same event with the same descriptive name. 

First, creat a new dataset with variables EVTYPE and EVENT. The values of EVTYPE are the unique values of the same variable in stormData dataset. The variable EVENT stores modified descriptive names of EVTYPE.


```r
EVTYPE <- sort(unique(stormData$EVTYPE))
EVENT <- EVTYPE #initial value, to be modified later
type <- data.frame(EVTYPE, EVENT)
head(type)
```

```
##                  EVTYPE                 EVENT
## 1    HIGH SURF ADVISORY    HIGH SURF ADVISORY
## 2         COASTAL FLOOD         COASTAL FLOOD
## 3           FLASH FLOOD           FLASH FLOOD
## 4             LIGHTNING             LIGHTNING
## 5             TSTM WIND             TSTM WIND
## 6       TSTM WIND (G45)       TSTM WIND (G45)
```


```r
type$EVENT <- gsub("[[:punct:]]$", "", type$EVENT) #remove tailing punctuation
type$EVENT <- gsub("/|\\\\|,|&|-|[[:punct:]]", " ", type$EVENT)
type$EVENT <- tolower(type$EVENT)
type$EVENT <- gsub("advisor[a-z]*|damage|pattern|spell|conditions|very|patchy|normal|prolong|prolonged|early|bitter|effects|gradient", "", type$EVENT)
type$EVENT <- gsub("winds|wins|w inds|wnd|windss|wind gust[a-z]*", "wind", type$EVENT)
type$EVENT <- gsub("storms|tsorm|strom", "storm", type$EVENT)
type$EVENT <- gsub("fldg|fld|flooding|floooding|floods|floodin", "flood", type$EVENT)
type$EVENT <- gsub("torndao|tornado[a-z].*", "tornado", type$EVENT)
type$EVENT <- gsub("tstm|thu[a-z]+torm", "thunderstorm", type$EVENT)
type$EVENT <- gsub("tunder", "thunder", type$EVENT)
type$EVENT <- gsub("snowfall", "snow", type$EVENT)
type$EVENT <- gsub("rainfall|rains|shower|showers", "rain", type$EVENT)
type$EVENT <- gsub("currents", "current", type$EVENT)
type$EVENT <- gsub("slides", "slide", type$EVENT)
type$EVENT <- gsub("mishap", "accident", type$EVENT)
type$EVENT <- gsub(" and ", " ", type$EVENT)
type$EVENT <- gsub("avalance", "avalanche ", type$EVENT)
type$EVENT <- gsub("dryness", "dry", type$EVENT)
type$EVENT <- gsub("cool|low temp|hyp[a-z]+thermia.*|^cold.*", "cold", type$EVENT)
type$EVENT <- gsub("abnormal|abnormally|unseasonably|unseasonal|unusual|unusually|unseasonable|ably|^ab", "", type$EVENT)
type$EVENT <- gsub("excessive heat|extreme heat|heat wave$|heat waves$|heatburst|high temperature|hot|warm|warmth|heats", "heat", type$EVENT)
type$EVENT <- gsub("wa.*spout|waterspouts|waterspout funnel cloud", "waterspout", type$EVENT)
type$EVENT <- gsub("tides", "tide", type$EVENT)
type$EVENT <- gsub("[a-z][0-9]+.*|[0-9]+.*", "", type$EVENT)
type$EVENT <- gsub("^ *|(?<= ) | *$", "", type$EVENT, perl=TRUE) #remove excess spaces
```


```r
type[grep("thunderstorm|thundersnow", type$EVENT), 2] <- "thunderstorm"
type[grep("lightning|lighting|ligntning", type$EVENT), 2] <- "lightning"
type[grep("hurricane|typhoon|floyd", type$EVENT), 2] <- "hurricane/typhoon"
type[grep("volcanic|vog", type$EVENT), 2] <- "volcanic ash"
type[grep("tornado|waterspout|landspout", type$EVENT), 2] <- "tornado/waterspout"
type[grep("flood|urban small|small stream|coastal surge", type$EVENT), 2] <- "flood/coastal flood"
type[grep("cloud|funnels|funnel", type$EVENT), 2] <- "funnel cloud"
type[grep("hail", type$EVENT), 2] <- "hail"
type[grep("fire|fires", type$EVENT), 2] <- "wirefire"
type[grep("tropical storm", type$EVENT), 2] <- "tropical storm"
type[grep("wint.*mix", type$EVENT), 2] <- "winter weather"
type[grep("win.*storm", type$EVENT), 2] <- "winter storm"
type[grep("microburst|micoburst|mircoburst|downburst", type$EVENT), 2] <- "microburst/downburst"
type[grep("sleet", type$EVENT), 2] <- "sleet"
type[grep("snow", type$EVENT), 2] <- "snow/heavy snow"
type[grep("coastal.*storm|storm surge|rough seas", type$EVENT), 2] <- "coastal storm"
type[grep("slide|slump", type$EVENT), 2] <- "landslide"
type[grep("blizzard", type$EVENT), 2] <- "blizzard"
type[grep("frost|freez", type$EVENT), 2] <- "frost/freeze"
type[grep("surf|high tides|high tide|rip current|high seas|high waves|high water|rogue wave|rising water|heavy seas|high swells|heavy swells", type$EVENT), 2] <- "high surf/high wave/rip current"
type[grep("eros", type$EVENT), 2] <- "coastal erosion"
type[grep("dust devel|dust devil|gustnado", type$EVENT), 2] <- "whirlwind"
type[grep("dust", type$EVENT), 2] <- "dust/duststorm"
type[grep("smoke", type$EVENT), 2] <- "dense smoke"
type[grep("fog", type$EVENT), 2] <- "dense fog"
type[grep("ice|icy", type$EVENT), 2] <- "ice/icestorm"
type[grep("drought|record low rain|below precipitation|dry weather|mild dry|record dry|excessively dry|^dry$", type$EVENT), 2] <- "drought/dry weather"
type[grep("rain|wet|record precipitation", type$EVENT), 2] <- "rain/heavy rain/wet weather"
type[grep("wind.*chill|record cold|severe cold|record low|colderature|extended cold|excessive cold|extreme cold|^cold", type$EVENT), 2] <- "cold/windchill"
type[grep("heat|temperature record|record temperature|record high", type$EVENT), 2] <- "heat"
type[grep("high wind|gusty.*wind|^wind|strong wind|wake low wind|force wind|metro storm", type$EVENT), 2] <- "strong wind/high wind"
type[grep("precip", type$EVENT), 2] <- "mix precipitation"
type[grep("southeast|red flag criteria|northern lights|none|no severe|non severe|mild|glaze|heavy mix|^dam|excessive$|^high$|drowning|marine accident", type$EVENT), 2] <- "other"
```

Merge the new events description file "type" with the storm dataset "stormData".


```r
stormData <- merge(stormData, type, by="EVTYPE", all=TRUE)
stormData$EVTYPE <- as.factor(stormData$EVENT)
stormData$EVENT <- NULL
head(stormData)
```

```
##                            EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP
## 1 high surf/high wave/rip current          0        0     200          3
## 2             flood/coastal flood          0        0       0          0
## 3             flood/coastal flood          0        0      50          3
## 4                       lightning          0        0       0          0
## 5                    thunderstorm          0        0       0          0
## 6                    thunderstorm          0        0     100          3
##   CROPDMG CROPDMGEXP
## 1       0          0
## 2       0          0
## 3       0          0
## 4       0          0
## 5       0          0
## 6       0          0
```

Add a variable "TOTALCASUALTY", total of "FATALITIES" and "INJURIES".


```r
stormData$TOTALCASUALTY <- stormData$FATALITIES + stormData$INJURIES
```

Add a variable "TOTALDAMAGE", total amount of property damage and crop damage.


```r
stormData$TOTALDAMAGE <- stormData$PROPDMG * (10 ^ stormData$PROPDMGEXP) + stormData$CROPDMG * (10 ^ stormData$CROPDMGEXP)
head(stormData)
```

```
##                            EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP
## 1 high surf/high wave/rip current          0        0     200          3
## 2             flood/coastal flood          0        0       0          0
## 3             flood/coastal flood          0        0      50          3
## 4                       lightning          0        0       0          0
## 5                    thunderstorm          0        0       0          0
## 6                    thunderstorm          0        0     100          3
##   CROPDMG CROPDMGEXP TOTALCASUALTY TOTALDAMAGE
## 1       0          0             0       2e+05
## 2       0          0             0       0e+00
## 3       0          0             0       5e+04
## 4       0          0             0       0e+00
## 5       0          0             0       0e+00
## 6       0          0             0       1e+05
```

## Results

Calculate 
1. Total number of fatalities and injuries per weather event. 
2. Average total number of fatalities and injuries per occurance of each event.
3. Total economic damage of properties and crops per weather event.
4. Average total economic damage of properties and crops per occurance of each event.


```r
totalCasualty <- aggregate(TOTALCASUALTY ~ EVTYPE, data = stormData, FUN= "sum")
averageCasualty <- aggregate(TOTALCASUALTY ~ EVTYPE, data = stormData, FUN= "mean")
totalDamage <- aggregate(TOTALDAMAGE ~ EVTYPE, data = stormData, FUN= "sum")
averageDamage <- aggregate(TOTALDAMAGE ~ EVTYPE, data = stormData, FUN= "mean")
```

Next, we will plot the above caculations. 

For the ease of plotting, create an event label file "eventLabel" in which each event is assigned a number. Only the event number will be shown in plots and the actual event name can be found in "eventLabel".


```r
EVENTLABEL <- paste("E", 1:length(totalCasualty$EVTYPE), sep="")
EVTYPE <- totalCasualty$EVTYPE
eventLabel <- data.frame(EVTYPE, EVENTLABEL)
eventLabel
```

```
##                             EVTYPE EVENTLABEL
## 1            astronomical low tide         E1
## 2                        avalanche         E2
## 3                         blizzard         E3
## 4                    blow out tide         E4
## 5                  coastal erosion         E5
## 6                    coastal storm         E6
## 7                   cold/windchill         E7
## 8                        dense fog         E8
## 9                      dense smoke         E9
## 10             drought/dry weather        E10
## 11                  dust/duststorm        E11
## 12             flood/coastal flood        E12
## 13                    frost/freeze        E13
## 14                    funnel cloud        E14
## 15                            hail        E15
## 16                            heat        E16
## 17 high surf/high wave/rip current        E17
## 18               hurricane/typhoon        E18
## 19                    ice/icestorm        E19
## 20                       landslide        E20
## 21                       lightning        E21
## 22            microburst/downburst        E22
## 23               mix precipitation        E23
## 24                           other        E24
## 25     rain/heavy rain/wet weather        E25
## 26                          seiche        E26
## 27               severe turbulence        E27
## 28                           sleet        E28
## 29                 snow/heavy snow        E29
## 30           strong wind/high wind        E30
## 31                    thunderstorm        E31
## 32              tornado/waterspout        E32
## 33             tropical depression        E33
## 34                  tropical storm        E34
## 35                         tsunami        E35
## 36                    volcanic ash        E36
## 37                       whirlwind        E37
## 38                    winter storm        E38
## 39                  winter weather        E39
## 40                        wirefire        E40
```

Add the event label to the plotting data.


```r
totalCasualty$EVENTLABEL <- eventLabel$EVENTLABEL
averageCasualty$EVENTLABEL <- eventLabel$EVENTLABEL
totalDamage$EVENTLABEL <- eventLabel$EVENTLABEL
averageDamage$EVENTLABEL <- eventLabel$EVENTLABEL
```

Figure 1: Total casualties across all years recorded vs. average total casualties per occurance.


```r
par(mfrow = c(1, 2), cex = 0.8, bg = "transparent")
with(totalCasualty, {
  barplot(TOTALCASUALTY,
          names.arg = EVENTLABEL, 
          space = 1,
          ylab = "Total Casualties", 
          xlab = "Weather Event",
          ylim = c(0, 1e+05), 
          main = "",
          col = "red")
  box()
})

with(averageCasualty, {
  barplot(TOTALCASUALTY,
          names.arg = EVENTLABEL, 
          space = 1,
          ylab = "Average Casualties", 
          xlab = "Weather Event",
          ylim = c(0, 10), 
          main = "",
          col = "red")
  box()
})
```

![plot of chunk unnamed-chunk-16](./RepResearchAssignment2_files/figure-html/unnamed-chunk-16.png) 

Figure 2: Total damage across all years recorded vs. average total damage per occurance.


```r
par(mfrow = c(1, 2), cex = 0.8, bg = "transparent")
with(totalDamage, {
  barplot(TOTALDAMAGE,
          names.arg = EVENTLABEL, 
          space = 1,
          ylab = "Total Damage", 
          xlab = "Weather Event",
          ylim = c(0, 2e+11), 
          main = "",
          col = "red")
  box()
})

with(averageDamage, {
  barplot(TOTALDAMAGE,
          names.arg = EVENTLABEL, 
          space = 1,
          ylab = "Average Damage", 
          xlab = "Weather Event",
          ylim = c(0, 3.2e+08), 
          main = "",
          col = "red")
  box()
})
```

![plot of chunk unnamed-chunk-17](./RepResearchAssignment2_files/figure-html/unnamed-chunk-17.png) 

Besides the figures above, let's look at the top 3 events that have the greatest number of total casualties and the top 3 events that have the greatest average total casualties per occurance.


```r
head(totalCasualty[with(totalCasualty, order(-TOTALCASUALTY)), ], 3)
```

```
##                EVTYPE TOTALCASUALTY EVENTLABEL
## 32 tornado/waterspout         97100        E32
## 16               heat         12400        E16
## 31       thunderstorm         10276        E31
```

```r
head(averageCasualty[with(totalCasualty, order(-TOTALCASUALTY)), ], 3)
```

```
##                EVTYPE TOTALCASUALTY EVENTLABEL
## 32 tornado/waterspout       1.50424        E32
## 16               heat       4.06691        E16
## 31       thunderstorm       0.03051        E31
```

Answer to the first question: Undoubtly, the most harmful type of event to population health is tornado/waterspout either you look at the total casualties or the average total casualties per occurance.

Next, we look at the top 3 events which incur the greatest total damage and the top 3 events which incur the greatest average total damage per occurance.


```r
head(totalDamage[with(totalDamage, order(-TOTALDAMAGE)), ], 3)
```

```
##                 EVTYPE TOTALDAMAGE EVENTLABEL
## 12 flood/coastal flood   1.807e+11        E12
## 18   hurricane/typhoon   9.087e+10        E18
## 32  tornado/waterspout   5.903e+10        E32
```

```r
head(averageDamage[with(totalDamage, order(-TOTALDAMAGE)), ], 3)
```

```
##                 EVTYPE TOTALDAMAGE EVENTLABEL
## 12 flood/coastal flood     2097311        E12
## 18   hurricane/typhoon   301902086        E18
## 32  tornado/waterspout      914476        E32
```

Answer to the second question: We might draw different conclusion depending on how we look at this. If we mostly care about the total damage over time, then the type of event causing the greatest economic consequence is flood/coastal flood, all sorts of flood related events. However, if we mostly care about the damage per occurance, then hurricane/typhoon is the most damaging to the economic aspect. 
