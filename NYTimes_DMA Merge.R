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

##Aggregate data and sum cases
nytimes_dma_Totalcases<- aggregate(nytimes_dma$cases, 
                                   by=list(nytimes_dma$date_DMA, nytimes_dma$date, nytimes_dma$DMA), FUN=sum)
colnames(nytimes_dma_Totalcases)[3] <- "DMA"
colnames(nytimes_dma_Totalcases)[2]<- "Date"
colnames(nytimes_dma_Totalcases)[4]<- "Total_Cases"
head(nytimes_dma_Totalcases,5)

##sum death
nytimes_dma_Totaldeaths<- aggregate(nytimes_dma$deaths, 
                                      by=list(nytimes_dma$date_DMA), FUN=sum)
colnames(nytimes_dma_Totaldeaths)[2] <- "Total_Deaths"
#Merge NYTimes/DMA
nytimes_dma2 <- left_join(nytimes_dma_Totalcases, 
                             nytimes_dma_Totaldeaths, by = c("Group.1" = "Group.1"))
##Change to New Cases per day
head(nytimes_dma2) 
nytimes_dma3<- slide(nytimes_dma2, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                     NewVar = "CaseLagDay-1", slideBy = -1)
nytimes_dma3$NewCasesPerDay = nytimes_dma3$Total_Cases - nytimes_dma3$'CaseLagDay-1'

##Change to New Deaths per day
nytimes_dma4<- slide(nytimes_dma3, Var = "Total_Deaths", GroupVar = "DMA", TimeVar = "Date", 
                      NewVar = "DeathLagDay-1", slideBy = -1)
nytimes_dma4$NewDeathsPerDay = nytimes_dma4$Total_Deaths - nytimes_dma4$'DeathLagDay-1'


##Adding Case Lags -14 to 14
nytimes_dma_lag<- slide(nytimes_dma4, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                     NewVar = "CaseLagDay-2", slideBy = -2)
nytimes_dma_lag3<- slide(nytimes_dma_lag, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                        NewVar = "CaseLagDay-3", slideBy = -3)
nytimes_dma_lag4<- slide(nytimes_dma_lag3, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                        NewVar = "CaseLagDay-4", slideBy = -4)
nytimes_dma_lag5<- slide(nytimes_dma_lag4, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                    NewVar = "CaseLagDay-5", slideBy = -5)
nytimes_dma_lag6<- slide(nytimes_dma_lag5, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                    NewVar = "CaseLagDay-6", slideBy = -6)
nytimes_dma_lag7<- slide(nytimes_dma_lag6, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-7", slideBy = -7)
nytimes_dma_lag8<- slide(nytimes_dma_lag7, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-8", slideBy = -8)
nytimes_dma_lag9<- slide(nytimes_dma_lag8, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-9", slideBy = -9)
nytimes_dma_lag10<- slide(nytimes_dma_lag9, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-10", slideBy = -10)
nytimes_dma_lag11<- slide(nytimes_dma_lag10, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-11", slideBy = -11)
nytimes_dma_lag12<- slide(nytimes_dma_lag11, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-12", slideBy = -12)
nytimes_dma_lag13<- slide(nytimes_dma_lag12, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-13", slideBy = -13)
nytimes_dma_lag14<- slide(nytimes_dma_lag13, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay-14", slideBy = -14)
##Leading +1 to + 14
nytimes_dma_lag15<- slide(nytimes_dma_lag14, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                     NewVar = "CaseLagDay+1", slideBy = +1)
nytimes_dma_lag16<- slide(nytimes_dma_lag15, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+2", slideBy = +2)
nytimes_dma_lag17<- slide(nytimes_dma_lag16, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+3", slideBy = +3)
nytimes_dma_lag18<- slide(nytimes_dma_lag17, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+4", slideBy = +4)
nytimes_dma_lag19<- slide(nytimes_dma_lag18, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+5", slideBy = +5)
nytimes_dma_lag20<- slide(nytimes_dma_lag19, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+6", slideBy = +6)
nytimes_dma_lag21<- slide(nytimes_dma_lag20, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+7", slideBy = +7)
nytimes_dma_lag22<- slide(nytimes_dma_lag21, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+8", slideBy = +8)
nytimes_dma_lag23<- slide(nytimes_dma_lag22, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                      NewVar = "CaseLagDay+9", slideBy = +9)
nytimes_dma_lag24<- slide(nytimes_dma_lag23, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                       NewVar = "CaseLagDay+10", slideBy = +10)
nytimes_dma_lag25<- slide(nytimes_dma_lag24, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                       NewVar = "CaseLagDay+11", slideBy = +11)
nytimes_dma_lag26<- slide(nytimes_dma_lag25, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                       NewVar = "CaseLagDay+12", slideBy = +12)

nytimes_dma_lag27<- slide(nytimes_dma_lag26, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                       NewVar = "CaseLagDay+13", slideBy = +13)
nytimes_dma_lag28<- slide(nytimes_dma_lag27, Var = "Total_Cases", GroupVar = "DMA", TimeVar = "Date", 
                                                       NewVar = "CaseLagDay+14", slideBy = +14)
                            
####END


##save file
setwd("C:/Users/poay1/Downloads")
write.csv(nytimes_dma_lag28,'nytimes_dma_4.csv')



