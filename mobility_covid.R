library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)


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
#Captialize susitna
county_mobility$sub_region_2 <- mapply(gsub,"Matanuska-susitna","Matanuska-Susitna",county_mobility$sub_region_2)


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
#merged$date <- mapply(gsub, "-","",merged$date)

merged$id <- paste(merged$date, merged$DMA, sep = "_")

write.csv(merged[,-c(4:6)], "mobility_w_dma.csv")

#Removing all NA
clean_merged <- filter(merged, retail_and_recreation_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, grocery_and_pharmacy_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, parks_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, transit_stations_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, workplaces_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, residential_percent_change_from_baseline != "NA")

#interpolate between missing days for one county
autauga <- filter(merged, County_State == "Autauga,AL")
autauga
autauga$date <- as.Date(autauga$date)
glimpse(autauga)
#linear interpolation
autauga$residential_percent_change_from_baseline <- na.approx(autauga$residential_percent_change_from_baseline, maxgap = 3, na.rm = F)

#interpolate between missing days for all counties
interpolate.zoo <- function(df){
  df$residential_percent_change_from_baseline <- na.approx(df$residential_percent_change_from_baseline, maxgap = 3, na.rm = F)
  return(df)
}

glimpse(merged)

cty <- unique(merged[,"County_State"])
glimpse(cty)

int_merged <- merged

for (ch in cty){
  single_county <- filter(int_merged, County_State == ch)
  single_county$residential_percent_change_from_baseline <- na.approx(single_county$residential_percent_change_from_baseline, maxgap = 3, na.rm = F)
  single_county$grocery_and_pharmacy_percent_change_from_baseline <- na.approx(single_county$grocery_and_pharmacy_percent_change_from_baseline, maxgap = 3, na.rm = F)
  single_county$retail_and_recreation_percent_change_from_baseline <- na.approx(single_county$retail_and_recreation_percent_change_from_baseline, maxgap = 3, na.rm = F)
  single_county$workplaces_percent_change_from_baseline <- na.approx(single_county$workplaces_percent_change_from_baseline, maxgap = 3, na.rm = F)
  single_county$transit_stations_percent_change_from_baseline <- na.approx(single_county$transit_stations_percent_change_from_baseline, maxgap = 3, na.rm = F)
  single_county$parks_percent_change_from_baseline <- na.approx(single_county$parks_percent_change_from_baseline, maxgap = 3, na.rm = F)
  int_merged[which(int_merged$County_State == ch),] <- single_county
  }

merged_clean <- select(int_merged,c(country_region_code,country_region,County_State,date,retail_and_recreation_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,workplaces_percent_change_from_baseline,DMA,id))
#merged_clean <- int_merged[,-c("transit_stations_percent_change_from_baseline","parks_percent_change_from_baseline","residential_percent_change_from_baseline")]
merged_clean <- filter(merged_clean, retail_and_recreation_percent_change_from_baseline != "NA")
merged_clean <- filter(merged_clean, grocery_and_pharmacy_percent_change_from_baseline != "NA")
merged_clean <- filter(merged_clean, workplaces_percent_change_from_baseline != "NA")
merged_clean <- filter(merged_clean, merged_clean$DMA != "NA")

#the categories with the most NA's are parks, transit stations, residential
sum(is.na(int_merged$retail_and_recreation_percent_change_from_baseline))/nrow(int_merged)
sum(is.na(int_merged$grocery_and_pharmacy_percent_change_from_baseline))/nrow(int_merged)
sum(is.na(int_merged$parks_percent_change_from_baseline))/nrow(int_merged)
sum(is.na(int_merged$transit_stations_percent_change_from_baseline))/nrow(int_merged)
sum(is.na(int_merged$workplaces_percent_change_from_baseline))/nrow(int_merged)
sum(is.na(int_merged$residential_percent_change_from_baseline))/nrow(int_merged)
  

#remove dashes from date
merged_clean$date <- mapply(gsub, "-","",merged_clean$date)

merged_clean$id <- paste(merged_clean$date, merged_clean$DMA, sep = "_")


#This file has removed 3 of the locations(parks,transit stations, residential) and interpolated in the others
write.csv(merged_clean, "mobility_w_dma_clean.csv")

  
#Aggregate up to DMA level with using weighted average of population
acs.pop <- read.csv("ACS.csv")
acs.pop$Geographic.Area.Name <- as.character(acs.pop$Geographic.Area.Name)

split <- strsplit(acs.pop$Geographic.Area.Name,", ")
mat  <- matrix(unlist(split), ncol=2, byrow=TRUE)
df   <- as.data.frame(mat)
colnames(df) <- c("County", "State")

acs.pop <- cbind(acs.pop,df)

#remove "County"
acs.pop$County <- mapply(gsub," County","",acs.pop$County)
#replace state name with abbreviation
acs.pop$State <- state.abb[match(acs.pop$State,state.name)]

acs.pop <- unite(acs.pop, County_State, c(County,State), sep = ",", remove=TRUE)

acs.pop <- select(acs.pop,TotPop,County_State)


#Changes to ACS
acs.pop$County_State <- mapply(gsub,"DeKalb","Dekalb",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Anchorage Municipality","Anchorage Borough",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Juneau City and Borough,AK","Juneau Borough,AK",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Miami-Dade,FL","Dade,FL",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McDuffie,GA","Mcduffie,GA",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"DuPage,IL","Dupage,IL",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"LaSalle,IL","La Salle,IL",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McHenry,IL","Mchenry,IL",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McLean,IL","Mclean,IL",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Dekalb,IN","De Kalb,IN",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"LaPorte,IN","La Porte,IN",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McPherson,KS","Mcpherson,KS",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McCracken,KY","Mccracken,KY",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"LaSalle Parish,LA","La Salle Parish,LA",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"St. John the Baptist Parish,LA","St. John The Baptist Parish,LA",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Prince George's,MD","Prince George'S,MD",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"St. Mary's,MD","St. Mary'S,MD",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McLeod,MN","Mcleod,MN",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"DeSoto,MS","Desoto,MS",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McDonald,MO","Mcdonald,MO",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Lewis and Clark,MT","Lewis And Clark,MT",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Doï¿½a Ana,NM","Dona Ana,NM",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"DeSoto,FL","Desoto,FL",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McIntosh,GA","Mcintosh,GA",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McDonough,IL","Mcdonough,IL",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"LaGrange,IN","Lagrange,IN",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McCreary,KY","Mccreary,KY",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Queen Anne's,MD","Queen Anne'S,MD",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McKinley,NM","Mckinley,NM",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McDowell,NC","Mcdowell,NC",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McKenzie,ND","Mckenzie,ND",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McClain,OK","Mcclain,OK",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McCurtain,OK","Mccurtain,OK",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McIntosh,OK","Mcintosh,OK",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McKean,PA","Mc Kean,PA",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McCormick,SC","Mccormick,SC",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McMinn,TN","Mcminn,TN",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McNairy,TN","Mcnairy,TN",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"DeWitt,TX","Dewitt,TX",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McCulloch,TX","Mcculloch,TX",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"McLennan,TX","Mclennan,TX",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Fond du Lac,WI","Fond Du Lac,WI",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"city,VA","City,VA",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Isle of Wight,VA","Isle Of Wight,VA",acs.pop$County_State)





merged_plus_pop <- left_join(merged_clean, acs.pop, by = c("County_State" = "County_State"))
merged_plus_pop

unique(merged_plus_pop[is.na(merged_plus_pop$TotalPop), "County_State"])

#This file adds the population to the mobility and dma file
write.csv(merged_plus_pop, "mobility_dma_pop.csv")

##################################
#Aggregate mobility categories by id (date_DMA)
##################################
library(data.table)

#merged_pop <- merged_plus_pop[,c(5,6,7,9,10)]
#merged_pop <- merged_pop[c("id","retail_and_recreation_percent_change_from_baseline","grocery_and_pharmacy_percent_change_from_baseline","workplaces_percent_change_from_baseline","TotalPop")]

#agg_mobility <- aggregate(merged_pop, by = list(merged_pop$id), FUN = weighted.mean, w = list(merged_pop$TotPop))

ag_mobility <- data.table(merged_plus_pop)

retail_weighted <-ag_mobility[,list(w_retail_and_recreation_percent_change_from_baseline = weighted.mean(retail_and_recreation_percent_change_from_baseline,TotPop)),by=id]
grocery_weighted <-ag_mobility[,list(w_grocery_and_pharmacy_percent_change_from_baseline = weighted.mean(grocery_and_pharmacy_percent_change_from_baseline,TotPop)),by=id]
work_weighted <-ag_mobility[,list(w_workplaces_percent_change_from_baseline = weighted.mean(workplaces_percent_change_from_baseline,TotPop)),by=id]

mobility_agg <- left_join(retail_weighted, grocery_weighted, by = c("id" = "id"))
mobility_agg <- left_join(mobility_agg, work_weighted, by = c("id" = "id"))


#this is the mobility data aggregated up to the dma level using population as a weighted mean
write.csv(mobility_agg, "mobility_agg.csv")

