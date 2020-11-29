library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)


global_mobility <- read.csv("raw_data/2020_US_Region_Mobility_Report.csv")
county_mobility <- filter(global_mobility, country_region_code == "US" & sub_region_2 != "")
glimpse(county_mobility)

#remove "County"
county_mobility$sub_region_2 <- mapply(gsub," County","",county_mobility$sub_region_2)
#remove "Parish"
county_mobility$sub_region_2 <- mapply(gsub," Parish","",county_mobility$sub_region_2)

# Alaska Boroughs
county_mobility$sub_region_2 <- mapply(gsub,"Anchorage","Anchorage Borough",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Fairbanks North Star","Fairbanks North Star Borough",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Matanuska-Susitna","Matanuska-Susitna Borough",county_mobility$sub_region_2)
#Change "Miami-dade" to "Dade"
county_mobility$sub_region_2 <- mapply(gsub,"Miami-Dade","Dade",county_mobility$sub_region_2)
#Change "Doã±a" to "Dona"
county_mobility$sub_region_2 <- mapply(gsub,"DoÃ±a","Dona",county_mobility$sub_region_2)
#Change "Broomfield" to "Boulder"
county_mobility$sub_region_2 <- mapply(gsub,"Broomfield","Boulder",county_mobility$sub_region_2)
# Shannon County changed it's name in 2015
county_mobility$sub_region_2 <- mapply(gsub,"Oglala Lakota","Shannon",county_mobility$sub_region_2)

#Changes to VA
county_mobility$sub_region_2 <- mapply(gsub,"Alexandria","Alexandria city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Charlottesville","Charlottesville city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Chesapeake","Chesapeake city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Colonial Heights","Colonial Heights city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Danville","Danville city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Emporia","Emporia city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Falls Church","Falls Church city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Fredericksburg","Fredericksburg city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Galax","Galax city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Harrisonburg","Harrisonburg city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Hopewell","Hopewell city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Lynchburg","Lynchburg city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Manassas","Manassas city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Manassas Park","Manassas Park city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Martinsville","Martinsville city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Newport News","Newport News city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Norton","Norton city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Petersburg","Petersburg city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Poquoson","Poquoson city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Portsmouth","Portsmouth city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Radford","Radford city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Staunton","Staunton city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Virgina Beach","Virginia Beach city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Waynesboro","Waynesboro city",county_mobility$sub_region_2)
county_mobility$sub_region_2 <- mapply(gsub,"Winchester","Winchester city",county_mobility$sub_region_2)


mobility_county_state <- unite(county_mobility,
                              County_State, c(sub_region_2,sub_region_1), sep = ",", remove=TRUE)

mobility_county_state$County_State<-trimws(mobility_county_state$County_State, which = c("both"))
str(mobility_county_state$County_State)

#Final changes to VA
mobility_county_state$County_State <- mapply(gsub,"Buena Vista,Virginia","Buena Vista city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Norfolk,Virginia","Norfolk city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Suffolk,Virginia","Suffolk city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Hampton,Virginia","Hampton city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Salem,Virginia","Salem city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Covington,Virginia","Covington city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Lexington,Virginia","Lexington city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Williamsburg,Virginia","Williamsburg city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Bristol,Virginia","Bristol city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Manassas city Park,Virginia","Manassas Park city,Virginia",mobility_county_state$County_State)
mobility_county_state$County_State <- mapply(gsub,"Virginia Beach,Virginia","Virginia Beach city,Virginia",mobility_county_state$County_State)

# LaSalle, LA spelled different than La Salle, TX
mobility_county_state$County_State <- mapply(gsub,"La Salle,Louisiana","LaSalle,Louisiana",mobility_county_state$County_State)
# Juneau, AK vs Juneau, WI
mobility_county_state$County_State <- mapply(gsub,"Juneau,Alaska","Juneau Borough,Alaska",mobility_county_state$County_State)


#County to DMA
dma <- read.csv("county_dma2.csv")
dma$CNTYFP <- NULL
dma$CNTYTVHH <- NULL
dma$DMAINDEX <- NULL
dma$STATEFP <- NULL
dma$County <- trimws(dma$COUNTY, which = c("both"))
dma$COUNTY <-NULL
dma$DMA <- trimws(dma$DMA, which = c("both"))
dma$STATE <-trimws(dma$STATE,which = c("both"))
dma$STATE_ABBR <- NULL
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

write.csv(merged[,-c(4:6)], "Data/mobility_output/mobility_w_dma.csv")

#Removing all NA
clean_merged <- filter(merged, retail_and_recreation_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, grocery_and_pharmacy_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, parks_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, transit_stations_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, workplaces_percent_change_from_baseline != "NA")
clean_merged <- filter(clean_merged, residential_percent_change_from_baseline != "NA")

#interpolate between missing days for one county
autauga <- filter(merged, County_State == "Autauga,Alabama")
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

acs.pop <- unite(acs.pop, County_State, c(County,State), sep = ",", remove=TRUE)

acs.pop <- select(acs.pop,TotPop,County_State)


#Changes to ACS
acs.pop$County_State <- mapply(gsub,"Anchorage Municipality","Anchorage Borough",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Juneau City and Borough","Juneau Borough",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Miami-Dade","Dade",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"LaSalle Parish","La Salle Parish",acs.pop$County_State)
acs.pop$County_State <- mapply(gsub,"Do.*a Ana","Dona Ana",acs.pop$County_State)

merged_plus_pop <- left_join(merged_clean, acs.pop, by = c("County_State" = "County_State"))
merged_plus_pop

unique(merged_plus_pop[is.na(merged_plus_pop$TotalPop), "County_State"])

#This file adds the population to the mobility and dma file
write.csv(merged_plus_pop, "Data/mobility_output/mobility_dma_pop.csv")

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
write.csv(mobility_agg, "Data/mobility_output/mobility_agg.csv")

