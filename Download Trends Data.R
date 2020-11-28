######################
#Title: Create Google Trends Database
#Author: Akshay Duda
#Last Update: 4/30/20 10:48 PM
#######################



#0. Clear Environment & Load Packages
rm(list = ls())

library(Stack)
library(stats)
library(readr)
library(dplyr)
library(gtrendsR)

#1. Download Base Datasets

#1.1. Download and Clean List of Geos

dmas <- read_csv("raw_data/dmaList.csv")

#2. Download Queries for Every DMA

source("gtrendsR.R")

# If there are more than 6 months in the API pull then Google returns data for
# each week instead of each day. So it's necessary to split this into 2 different
# pulls. We'll have them overlap by a month so we can try to scale them back
# together.

OverTimeDMA1 <- as.data.frame(NULL)
for (i in 1:nrow(dmas)){
    query1 <- gtrends(keyword = "covid symptoms", time = "2020-01-21 2020-6-20",
                  gprop = "web", geo= dmas[[1]][i],
                  category = 0, low_search_volume = TRUE,
                  tz = 0, onlyInterest = FALSE)

    extract <- query1$interest_over_time
    extract$geo_code <- dmas[[1]][i]

    OverTimeDMA1 <- rbind(OverTimeDMA1, extract)
    print(i)
}

OverTimeDMA2 <- as.data.frame(NULL)
for (i in 1:nrow(dmas)){
    query1 <- gtrends(keyword = "covid symptoms", time = "2020-05-20 2020-10-09",
                      gprop = "web", geo= dmas[[1]][i],
                      category = 0, low_search_volume = TRUE,
                      tz = 0, onlyInterest = FALSE)

    extract <- query1$interest_over_time
    extract$geo_code <- dmas[[1]][i]

    OverTimeDMA2 <- rbind(OverTimeDMA2, extract)
    print(i)
}


# We'll take the overlapping days and estimate the relationship between them.
# Google introduces noise into each observation, so the ratio can't be perfectly
# estimated, but the average relationship should be good enough.
OverTimeDMA1 %>%
    filter(date >= "2020-05-20" & date <= "2020-06-20") %>%
    select(date, geo, hits) -> overlap_days_1

OverTimeDMA2 %>%
    filter(date >= "2020-05-20" & date <= "2020-06-20") %>%
    select(date, geo, hits) -> overlap_days_2

overlap_days <- left_join(overlap_days_1, overlap_days_2,
                          by = c("date" = "date", "geo" = "geo"))

overlap_days <- as_tibble(overlap_days)

overlap_days %>%
    mutate(ratio = hits.y / hits.x) %>%
    filter(!(ratio == Inf)) %>%
    group_by(geo) %>%
    summarise(avg_ratio = mean(ratio, na.rm = T)) -> days_scalar

# Now we'll multiply the first set of days by their respective scalar to
# transform it to be on the same scale as the second set of dates
OverTimeDMA1 <- left_join(OverTimeDMA1, days_scalar)

OverTimeDMA1 %>%
    filter(date <= "2020-05-19") %>%
    mutate(hits = hits * avg_ratio) %>%
    select(date, geo, hits) -> OverTimeDMA1

# Trim to the same columns
OverTimeDMA2 %>%
    select(date, geo, hits) -> OverTimeDMA2

OverTimeDMA <- rbind(OverTimeDMA1, OverTimeDMA2)

#3. Download Comparison Across DMAs

query2 <- gtrends(keyword = "covid symptoms", time = "2020-01-01 2020-10-09",
                  gprop = "web", geo= "US",
                  category = 0, hl = "en-US", low_search_volume = TRUE,
                  tz = 0, onlyInterest = FALSE)

AcrossDMA <- query2$interest_by_dma

#3.2. Add in Location Code Into Across DMA Dataset

USDMA <- read_csv("raw_data/dmaLocationCrosswalk.csv")

AcrossDMA <- left_join(AcrossDMA, USDMA)

#4. ReScale Data

#4.1 Create Scale Factor Across DMAs
AcrossDMA$Factor <- AcrossDMA$hits/100

#4.2 Create Aggregate Sums of Daily Counts

Countacrossdays <- aggregate(x = OverTimeDMA$hits, by=list(OverTimeDMA$geo), FUN=sum)

#4.3. Create an Inflation Factor that matches aggregate sum of fractional
# searches with normalized daily fractions
CountFactor <- left_join(Countacrossdays, AcrossDMA[, c("geo_code", "Factor")],
                     by = c("Group.1" = "geo_code"))

CountFactor$Numerator <- CountFactor$Factor*CountFactor[which.max(CountFactor$Factor),"x"]
CountFactor$Inflation.Factor <- CountFactor$Numerator/CountFactor$x

#4.4 Merge Inflation Factor Into Dataset and Adjust
OverTimeClean <- left_join(OverTimeDMA,
                           CountFactor[,c("Group.1","Inflation.Factor")],
                           by = c("geo" = 'Group.1'))

OverTimeClean$hits.adj <- OverTimeClean$hits* OverTimeClean$Inflation.Factor

OverTimeClean %>%
    select(date, geo, hits.adj) -> OverTimeClean

write_csv(OverTimeClean, "Data/TrendsData.csv")
