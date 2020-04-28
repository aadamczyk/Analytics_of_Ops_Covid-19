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
##Fix "Unknown, RI"
nytimes_county_state$County_State <- gsub("Unknown,RI", "Providence,RI",
                                          nytimes_county_state$County_State)
##Fix "District of Columbia
nytimes_county_state$County_State <- gsub("District of Columbia,NA", "District Of Columbia,DC",
                                          nytimes_county_state$County_State)
#Fix Miami-Dade
nytimes_county_state$County_State <- 
  gsub( "Miami-Dade,FL", "Dade,FL",
        nytimes_county_state$County_State)
#Fix Alaska
nytimes_county_state$County_State <- 
  gsub("Anchorage,AK","Anchorage Borough,AK",
       nytimes_county_state$County_State)
nytimes_county_state$County_State <- 
  gsub("Ketchikan Gateway Borough,AK","Juneau Borough,AK",
       nytimes_county_state$County_State)            
nytimes_county_state$County_State <- 
  gsub("Juneau City and Borough,AK","Juneau Borough,AK",
       nytimes_county_state$County_State)
nytimes_county_state$County_State <- 
  gsub("Petersburg Borough,AK","Juneau Borough,AK",
       nytimes_county_state$County_State)
nytimes_county_state$County_State <- 
  gsub("Prince of Wales-Hyder Census Area,AK","Juneau Borough,AK",
       nytimes_county_state$County_State)
nytimes_county_state$County_State <- 
  gsub("Southeast Fairbanks Census Area,AK","Fairbanks North Star Borough,AK",
       nytimes_county_state$County_State)

#Fix Laporte,IN
nytimes_county_state$County_State <- 
  gsub("LaPorte,IN","La Porte,IN",
       nytimes_county_state$County_State)
#Fix Lasalle,IL
nytimes_county_state$County_State <- 
  gsub("LaSalle,IL","La Salle,IL",
       nytimes_county_state$County_State)
#Fix Broomfield
nytimes_county_state$County_State <- 
  gsub("Broomfield,CO","Boulder,CO",
       nytimes_county_state$County_State)
#Fix Kansas City
nytimes_county_state$County_State <- 
  gsub("Kansas City,MO","Jackson,MO",
       nytimes_county_state$County_State)
#Fix New Mexico
nytimes_county_state$County_State <- 
  gsub("Doña Ana,NM","Dona Ana,NM",
       nytimes_county_state$County_State)


###IGNORE CASE
nytimes_county_state$County_State <- tolower(nytimes_county_state$County_State)
#More state fixing
#Fix lasalle,il
nytimes_county_state$County_State <- 
  gsub("lasalle,il","la salle,il",
       nytimes_county_state$County_State)
#Fix lasalle,la
nytimes_county_state$County_State <- 
  gsub("lasalle,la","la salle,la",
       nytimes_county_state$County_State)
#Fix Dekalb, in
nytimes_county_state$County_State <- 
  gsub("dekalb,in","de kalb,in",
       nytimes_county_state$County_State)
#Fix McKean, PA
nytimes_county_state$County_State <- 
  gsub("mckean,pa","mc kean,pa",
       nytimes_county_state$County_State)
#Fix Ogala County
nytimes_county_state$County_State <- 
  gsub("oglala lakota,sd","shannon,sd",
       nytimes_county_state$County_State)

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

##Aggregate data and sum cases/deaths

nytimes_dma_Totalcases<- aggregate(nytimes_dma$cases, 
                          by=list(Category=nytimes_dma$date_DMA), FUN=sum)
colnames(nytimes_dma_Totalcases)[2] <- "Total Cases"
                           
nytimes_dma_Totaldeaths<-aggregate(nytimes_dma$deaths, 
                  by=list(Category=nytimes_dma$date_DMA), FUN=sum)
colnames(nytimes_dma_Totaldeaths)[2] <- "Total Deaths"

merged2 <- left_join(nytimes_dma_Totalcases, nytimes_dma_Totaldeaths, by = c("Category" = "Category"))


####END


##save file
setwd("C:/Users/poay1/Downloads")
write.csv(merged2,'nytimes_dma_3.csv')


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
