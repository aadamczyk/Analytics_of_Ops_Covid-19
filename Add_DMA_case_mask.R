library(readr)
library(dplyr)
library(stringr)

nyt <- read_csv("cleaned_case_mask_data.csv")

# Fix minor spelling discrepancies
nyt$county[nyt$county == "Anchorage"] <- "Anchorage Borough"
nyt$county[nyt$county == "Juneau City and Borough"] <- "Juneau Borough"
nyt$county[nyt$county == "Miami-Dade"] <- "Dade"
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

# Add populations and aggregate
acs <- read_csv("ACS.csv")
acs %>%
    select(`Geographic Area Name`, TotPop) %>%
    mutate(geo = str_remove_all(`Geographic Area Name`, " County"),
           geo = str_remove_all(geo, " Parish")) %>%
    select(-`Geographic Area Name`) -> acs

acs$geo[acs$geo == "Anchorage Municipality, Alaska"] <- "Anchorage Borough, Alaska"
acs$geo[acs$geo == "Juneau City and Borough, Alaska"] <- "Juneau Borough, Alaska"
acs$geo[acs$geo == "Miami-Dade, Florida"] <- "Dade, Florida"
acs$geo[acs$geo == "Do<U+FFFD>a Ana, New Mexico"] <- "Dona Ana, New Mexico"

nyt %>%
    mutate(geo = paste0(county, ", ", state)) -> nyt

nyt <- left_join(nyt, acs, by = c("geo" = "geo"))

nyt %>%
    select(date, DMA, TotPop, cases, deaths, new_cases, new_deaths, NEVER, RARELY,
           SOMETIMES, FREQUENTLY, ALWAYS) -> nyt

nyt %>%
    group_by(date, DMA) %>%
    mutate(DMA_pop = sum(TotPop), pop_share = TotPop / DMA_pop) %>%
    mutate(NEVER = NEVER * pop_share,
           RARELY = RARELY * pop_share,
           SOMETIMES = SOMETIMES * pop_share,
           FREQUENTLY = FREQUENTLY * pop_share,
           ALWAYS = ALWAYS * pop_share) %>%
    summarise(cases = sum(cases),
              deaths = sum(deaths),
              new_cases = sum(new_cases),
              new_deaths = sum(new_deaths),
              NEVER = sum(NEVER),
              RARELY = sum(RARELY),
              SOMETIMES = sum(SOMETIMES),
              FREQUENTLY = sum(FREQUENTLY),
              ALWAYS = sum(ALWAYS)) -> nyt

write_csv(nyt, "Data/cleaned_case_mask_DMAs.csv")
