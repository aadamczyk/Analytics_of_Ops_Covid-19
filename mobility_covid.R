library(dplyr)
library(tidyr)
library(lubridate)

global_mobility <- read.csv("Global_Mobility_Report.csv")
county_mobility <- filter(global_mobility, country_region_code == "US" & sub_region_2 != "")
glimpse(county_mobility)

#remove "County"
county_mobility$sub_region_2 <- mapply(gsub," County","",county_mobility$sub_region_2)
#remove "Borough"
county_mobility$sub_region_2 <- mapply(gsub," Borough","",county_mobility$sub_region_2)
#remove "Parish"
county_mobility$sub_region_2 <- mapply(gsub," Parish","",county_mobility$sub_region_2)
#replace state name with abbreviation
county_mobility$sub_region_1 <- state.abb[match(county_mobility$sub_region_1,state.name)]

#only captitalize first letter of each word in County
Caps <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#fix capitalization discrepancies
county_mobility$sub_region_2 <- sapply(county_mobility$sub_region_2, tolower)
county_mobility$sub_region_2 <- sapply(county_mobility$sub_region_2, Caps)

#unique changes for each state
county_mobility$sub_region_2[which(county_mobility$sub_region_1 == "AK")]<- paste(county_mobility$sub_region_2[which(county_mobility$sub_region_1 == "AK")],"Borough", sep=" ")
#VA has many cities that have "City" included and many without, it will probably be easiest to get rid of City
#county_mobility$sub_region_2[which(county_mobility$sub_region_1 == "VA")]<- paste(county_mobility$sub_region_2[which(county_mobility$sub_region_1 == "VA")],"City", sep=" ")
county_mobility$sub_region_2[which(county_mobility$sub_region_1 == "LA")]<- paste(county_mobility$sub_region_2[which(county_mobility$sub_region_1 == "LA")],"Parish", sep=" ")
#Change "Miami-dade" to "Dade"
county_mobility$sub_region_2 <- mapply(gsub,"Miami-dade","Dade",county_mobility$sub_region_2)
#Change "Lasalle" to "La Salle"
county_mobility$sub_region_2 <- mapply(gsub,"Lasalle","La Salle",county_mobility$sub_region_2)
#Change "Laporte" to "La Porte"
county_mobility$sub_region_2 <- mapply(gsub,"Laporte","La Porte",county_mobility$sub_region_2)
#Change "O'brien" to "O'Brien"
county_mobility$sub_region_2 <- mapply(gsub,"O'brien","O'Brien",county_mobility$sub_region_2)
#Change "'s" to "'S"
county_mobility$sub_region_2 <- mapply(gsub,"'s","'S",county_mobility$sub_region_2)
#Change "Doã±a" to "Dona"
county_mobility$sub_region_2 <- mapply(gsub,"Doã±a","Dona",county_mobility$sub_region_2)
#Change "Mckean" to "Mc Kean"
county_mobility$sub_region_2 <- mapply(gsub,"Mckean","Mc Kean",county_mobility$sub_region_2)


#Changes to VA
county_mobility$sub_region_2 <- mapply(gsub,"Alexandria","Alexandria City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Charlottesville","Charlottesville City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Chesapeake","Chesapeake City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Colonial Heights","Colonial Heights City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Danville","Danville City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Emporia","Emporia City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Falls Church","Falls Church City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Fredericksburg","Fredericksburg City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Galax","Galax City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Harrisonburg","Harrisonburg City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Hopewell","Hopewell City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Lynchburg","Lynchburg City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Manassas","Manassas City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Manassas Park","Manassas Park City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Martinsville","Martinsville City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Newport News","Newport News City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Norton","Norton City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Petersburg","Petersburg City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Poquoson","Poquoson City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Portsmouth","Portsmouth City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Radford","Radford City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Staunton","Staunton City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Virgina Beach","Virginia Beach City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Waynesboro","Waynesboro City",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Winchester","Winchester City",county_mobility$sub_region_2)


mobility_county_state <- unite(county_mobility, 
                              County_State, c(sub_region_2,sub_region_1), sep = ",", remove=TRUE)

mobility_county_state$County_State<-trimws(mobility_county_state$County_State, which = c("both"))
str(mobility_county_state$County_State)

#Change "Dekalb,IN" to "De Kalb,IN"
mobility_county_state$County_State <- mapply(gsub,"Dekalb,IN","De Kalb,IN",mobility_county_state$County_State)

#Final changes to VA
mobility_county_state$County_State <- mapply(gsub,"Buena Vista,VA","Buena Vista City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Norfolk,VA","Norfolk City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Suffolk,VA","Suffolk City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Hampton,VA","Hampton City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Salem,VA","Salem City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Covington,VA","Covington City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Lexington,VA","Lexington City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Williamsburg,VA","Williamsburg City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Bristol,VA","Bristol City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Manassas City Park,VA","Manassas Park City,VA",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Virginia Beach,VA","Virginia Beach City,VA",mobility_county_state$County_State)


#County to DMA
dma <- read.csv("county_dma.csv")
dma$CNTYFP <- NULL
dma$CNTYTVHH <- NULL
dma$DMAINDEX <- NULL
dma$STATEFP <- NULL
dma$County <- trimws(dma$COUNTY, which = c("both"))
dma$COUNTY <-NULL
dma$DMA <- trimws(dma$DMA, which = c("both"))
dma$STATE <-trimws(dma$STATE,which = c("both"))
head(dma, 1)
dma_v1 <- unite(dma, County_State, c(County,STATE), sep = ",", remove=TRUE)
head(dma_v1)
str(dma_v1)
merged <- left_join(mobility_county_state, dma_v1, by = c("County_State" = "County_State"))
merged


#County_States not mapped to a DMA
unique(merged[is.na(merged$DMA), "County_State"])


#remove dashes from date
merged$date <- mapply(gsub, "-","",merged$date)

merged$id <- paste(merged$date, merged$DMA, sep = "_")

write.csv(merged, "C:\\Users\\aaron\\Documents\\MIT\\Analytics Edge\\mobility_w_dma.csv")
