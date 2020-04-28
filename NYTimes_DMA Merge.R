####NYTimes <> DMA Merging
library(base)
library(lubridate)
library(tidyr)
library(dplyr)

nytimes <- read.csv("nytimes_clean.csv")
head(nytimes, 1)
nytimes_county_state <- unite(nytimes, 
                       County_State, c(county,state), sep = ",", remove=TRUE)
nytimes_county_state$Identifier <- NULL
nytimes_county_state$X <- NULL
head(nytimes_county_state)
str(nytimes_county_state)
##Clean DMA
dma <- read.csv("county_dma.csv")
dma$CNTYFP <- NULL
dma$CNTYTVHH <- NULL
dma$DMAINDEX <- NULL
dma$STATEFP <- NULL
dma$County <- trimws(dma$COUNTY, which = c("both"))
dma$COUNTY <-NULL
dma$DMA <- trimws(dma$DMA, which = c("both"))
head(dma, 1)
dma_v1 <- unite(dma, County_State, c(County,STATE), sep = ",", remove=TRUE)
head(dma_v1)
str(dma_v1)
merged <- left_join(nytimes_county_state, dma_v1, by = c("County_State" = "County_State"))
merged
