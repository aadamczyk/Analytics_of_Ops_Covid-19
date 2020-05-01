######################
#Title: Create Google Trends Database
#Author: Akshay Duda
#Last Update: 4/30/20 10:48 PM
#######################



#0. Clear Environment & Load Packages
rm(list = ls())

library(gtrendsR)
library(Stack)
library(stats)


#1. Download Base Datasets

#1.1. Download and Clean List of Geos

data("countries")
USDMA <- countries[c(122636:122845),]
USDMAcode <- USDMA$sub_code


#2. Download Queries for Every DMA

OverTimeDMA <- as.data.frame(NULL)
for (i in 1:length(USDMAcode))
  {

query1 <- gtrends(keyword = "covid symptoms", time = "2020-01-01 2020-04-30",
              gprop = "web", geo= USDMAcode[i],
              category = 0, hl = "en-US", low_search_volume = TRUE,
              tz = 0, onlyInterest = FALSE)

  extract <- query1$interest_over_time
  extract$geo_code <- USDMAcode[i]

OverTimeDMA <- rbind(OverTimeDMA, extract)

}

#3. Download Comparison Across DMAs

query2 <- gtrends(keyword = "covid symptoms", time = "2020-01-01 2020-04-30",
                  gprop = "web", geo= "US",
                  category = 0, hl = "en-US", low_search_volume = TRUE,
                  tz = 0, onlyInterest = FALSE)

AcrossDMA <- query2$interest_by_dma

#3.2. Add in Location Code Into Across DMA Dataset

USDMA$location.clean <- gsub(",", "", USDMA$name)
USDMA[192, c("location.clean")] = "Norfolk-Portsmouth-Newport News VA"
USDMA[52, c("location.clean")] = "Davenport IA-Rock Island-Moline IL"   
USDMA[50, c("location.clean")] = "Champaign & Springfield-Decatur IL"

AcrossDMA$location.clean <- gsub(",", "", AcrossDMA$location)
AcrossDMA[191, c("location.clean")] <- gsub("[.]", "", AcrossDMA[191,c("location.clean")])

AcrossDMA <- merge(AcrossDMA, USDMA, by.x = "location.clean", by.y = "location.clean", all.x = TRUE)

#4. ReScale Data

#4.1 Create Scale Factor Across DMAs 
AcrossDMA$Factor <- AcrossDMA$hits/100

#4.2 Create Aggregate Sums of Daily Counts

Countacrossdays <- aggregate(x = OverTimeDMA$hits, by=list(OverTimeDMA$geo_code), FUN=sum)

#4.3. Create an Inflation Factor that matches aggregate sum of fractional searches with normalized daily fractions 
CountFactor <- merge(Countacrossdays, AcrossDMA[, c("sub_code", "Factor")], by.x = "Group.1", by.y = "sub_code", all.x = TRUE)

CountFactor$Numerator <- CountFactor$Factor*CountFactor[which.max(CountFactor$Factor),"x"]
CountFactor$Inflation.Factor <- CountFactor$Numerator/CountFactor$x

#4.4 Merge Inflation Factor Into Dataset and Adjust
OverTimeClean <- merge(OverTimeDMA, CountFactor[,c("Group.1","Inflation.Factor")], by.x = "geo_code", by.y = 'Group.1', all.x = TRUE)

OverTimeClean$hits.adj <- OverTimeClean$hits* OverTimeClean$Inflation.Factor

write.csv(OverTimeClean,"TrendsData.csv", row.names = FALSE)