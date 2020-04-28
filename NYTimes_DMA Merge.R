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
##Clean DMA
dma <- read.csv("county_dma.csv")
dma$CNTYFP <- NULL
dma$CNTYTVHH <- NULL
dma$DMAINDEX <- NULL
dma$STATEFP <- NULL
dma$County <- trimws(dma$COUNTY, which = c("both"))
dma$COUNTY <-NULL
###Remove spaces from DMA **May be different from Aarons data
dma$DMA <- trimws(dma$DMA, which = c("both"))
head(dma, 1)
dma_1 <- unite(dma, County_State, c(County,STATE), sep = ",", remove=TRUE)
head(dma_1)
##To fix error message with merge
dma_1$County_State<- trimws(dma_1$County_State, which =c("both"))
head(dma_1, 1)
merged <- left_join(nytimes_county_state, dma_1, by = c("County_State" = "County_State"))
head(merged)
##add new column Identifier (date_DMA)
nytimes_dma<- unite(merged, date_DMA, c(date,DMA), sep = "_", remove=FALSE)
head(nytimes_dma, 5)
##save file
write.csv(nytimes_dma,'nytimes_dma.csv')
