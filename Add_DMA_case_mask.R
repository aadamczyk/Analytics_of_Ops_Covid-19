library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

nyt <- read_csv("cleaned_case_mask_data_1126.csv")

nyt %>%
    rename(county = County, state = State) -> nyt

# Fix minor spelling discrepancies
nyt$county[nyt$county == "Anchorage"] <- "Anchorage Borough"
nyt$county[nyt$county == "Juneau City and Borough"] <- "Juneau Borough"
nyt$county[nyt$county == "Miami-Dade"] <- "Dade"
nyt$county[nyt$county == "Baltimore City"] <- "Baltimore city"
nyt$county[nyt$county == "St. Louis City"] <- "St. Louis city"
nyt$county[nyt$county == "Doña Ana"] <- "Dona Ana"
# For some reason Broomfield County is missing, but it's clearly in the Denver DMA
# May be because only became a county in 2001, split off from Boulder
nyt$county[nyt$county == "Broomfield"] <- "Boulder"
# Shannon County changed it's name in 2015
nyt$county[nyt$county == "Oglala Lakota"] <- "Shannon"

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

# Create new rows for when there was no NYT data
all_dates <- unique(nyt$date)
all_counties <- unique(nyt[,c("county","state")])
all_counties <- paste(all_counties$county, all_counties$state, sep = "--")

combination_input <- list(date = all_dates[], location = all_counties[])

combination_input %>%
    cross_df() -> all_combinations

all_combinations$date <- as_date(all_combinations$date)

# Split apart the parts of the data that change each day from the constants
# before merging those back onto the data
nyt %>%
    select(date, county, state, cases:new_deaths) -> nyt_daily
nyt %>%
    select(county, state, DMA, NEVER:ALWAYS) %>%
    unique() -> nyt_permanent

all_combinations %>%
    mutate(cut_point = str_locate(location, "--")[,1],
           county = str_sub(location, end = cut_point-1),
           state = str_sub(location, start = cut_point+2)) %>%
    select(date, county, state) -> all_combinations

nyt_daily <- full_join(nyt_daily, all_combinations)
nyt_daily$cases[is.na(nyt_daily$cases)] <- 0
nyt_daily$deaths[is.na(nyt_daily$deaths)] <- 0
nyt_daily$new_cases[is.na(nyt_daily$new_cases)] <- 0
nyt_daily$new_deaths[is.na(nyt_daily$new_deaths)] <- 0

nyt <- full_join(nyt_daily, nyt_permanent)

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
# Shannon County changed it's name in 2015
acs$geo[acs$geo == "Oglala Lakota, South Dakota"] <- "Shannon, South Dakota"

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
