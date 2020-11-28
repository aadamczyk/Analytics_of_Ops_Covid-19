library(readr)
library(dplyr)

nyt <- read_csv("cleaned_case_mask_data.csv")

# Fix minor spelling discrepancies
nyt$county[nyt$county == "Anchorage"] <- "Anchorage Borough"
nyt$county[nyt$county == "Juneau City and Borough"] <- "Juneau Borough"
nyt$county[nyt$county == "Miami-Dade"] <- "Dade"
nyt$county[nyt$county == "Baltimore city"] <- "Baltimore City"
nyt$county[nyt$county == "St. Louis city"] <- "St. Louis City"
nyt$county[nyt$county == "Doña Ana"] <- "Dona Ana"
# For some reason Broomfield County is missing, but it's clearly in the Denver DMA
nyt$county[nyt$county == "Broomfield"] <- "Denver"
nyt$county[nyt$county == "Oglala Lakota"] <- "Pennington"

# Remove some missing counties
nyt %>%
    filter(county != "Aleutians East Borough" &
               county != "Aleutians West Census Area" &
               county != "Bethel Census Area" &
               county != "Bristol Bay Borough" &
               county != "Denali Borough" &
               county != "Dillingham Census Area" &
               county != "Haines Borough" &
               county != "Ketchikan Gateway Borough" &
               county != "Kodiak Island Borough" &
               county != "Kusilvak Census Area" &
               county != "Lake and Peninsula Borough" &
               county != "Nome Census Area" &
               county != "North Slope Borough" &
               county != "Northwest Arctic Borough" &
               county != "Petersburg Borough" &
               county != "Prince of Wales-Hyder Census Area" &
               county != "Sitka City and Borough" &
               county != "Skagway Municipality" &
               county != "Southeast Fairbanks Census Area" &
               county != "Valdez-Cordova Census Area" &
               county != "Wrangell City and Borough" &
               county != "Yukon-Koyukuk Census Area")-> nyt

county_dma <- read_csv("county_dma2.csv")

nyt <- left_join(nyt, county_dma, by = c("state" = "STATE", "county" = "COUNTY"))

nyt %>%
    select(date, DMA, cases, deaths, new_cases, new_deaths, NEVER, RARELY,
           SOMETIMES, FREQUENTLY, ALWAYS) -> nyt

write_csv(nyt, "Data/cleaned_case_mask_DMAs.csv")
