####NYTimes <> DMA Merging
library(base)
library(lubridate)
library(tidyr)
library(dplyr)
library(tm)
setwd("~/GitHub/Analytics_Edge_Covid-19")
nytimes <- read.csv("nytimes_clean.csv")
head(nytimes, 1)
nytimes_county_state <- unite(nytimes, 
                       County_State, c(county,state), sep = ",", remove=TRUE)
nytimes_county_state$Identifier <- NULL
nytimes_county_state$X <- NULL
head(nytimes_county_state)
##Fix "New York City"
nytimes_county_state$County_State <- gsub("New York City,NY", "New York,NY", 
                                         nytimes_county_state$County_State)
##Fix "Unknownw, RI"
nytimes_county_state$County_State <- gsub("Unknown,RI", "Providence,RI",
                                          nytimes_county_state$County_State)
##Fix "District of Columbia
nytimes_county_state$County_State <- gsub("District of Columbia,NA", "District Of Columbia,DC",
                                          nytimes_county_state$County_State)
#Fix Miami-Dade
nytimes_county_state$County_State <- 
  gsub( "Miami-Dade,FL", "Dade,FL",
        nytimes_county_state$County_State)
#Fix Anchorage
nytimes_county_state$County_State <- 
  gsub("Anchorage,AK","Anchorage Borough,AK",
       nytimes_county_state$County_State)
#Fix Laporte
nytimes_county_state$County_State <- 
  gsub("LaPorte,IN","La Porte,IN",
       nytimes_county_state$County_State)
###IGNORE CASE
nytimes_county_state$County_State <- tolower(nytimes_county_state$County_State)




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
##Remove word Parish
dma_1$County_State <- gsub(" Parish", "", dma_1$County_State)
dma_1$County_State <- gsub(" ,la", ",la",
                           dma_1$County_State)
head(dma_1,10)
##To fix space error message with merge
dma_1$County_State<- trimws(dma_1$County_State, which =c("both"))
head(dma_1, 1)
##? fix all spaces
dma_1$County_State<- trimws(dma_1$County_State)
head(dma_1, 1)
##Make all lower case
dma_1$County_State <- tolower(dma_1$County_State)

##MERGE
merged <- left_join(nytimes_county_state, dma_1, by = c("County_State" = "County_State"))
head(merged)

##add new column Identifier (date_DMA)
nytimes_dma<- unite(merged, date_DMA, c(date,DMA), sep = "_", remove=FALSE)
head(nytimes_dma, 5)



####END


##save file
setwd("C:/Users/poay1/Downloads")
write.csv(nytimes_dma,'nytimes_dma_2.csv')


##APPENDIX
#Fix Caps
##Fix "Prince George's"
nytimes_county_state$County_State <- 
  gsub("Prince George's,MD", "Prince George'S,MD",
       nytimes_county_state$County_State)
#Fix "city"
nytimes_county_state$County_State <- 
  gsub("city", "City",
       nytimes_county_state$County_State)

##Fix Fond du Lac
nytimes_county_state$County_State <- 
  gsub("Fond du Lac,WI","Fond Du Lac,WI",
       nytimes_county_state$County_State)
#Fix McCook,SD
nytimes_county_state$County_State <- 
  gsub("McCook,SD","Mccook,SD",
       nytimes_county_state$County_State)
#Fix Dekalb
nytimes_county_state$County_State <- 
  gsub("DeKalb,GA","Dekalb,GA",
       nytimes_county_state$County_State)
