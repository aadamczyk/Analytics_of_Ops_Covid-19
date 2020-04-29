library(tidyverse)
library(stringr)

acs <- read_csv("ACS.csv")

county_DMA <- read_csv("county_dma.csv")

county_DMA$Geo <- paste(county_DMA$COUNTY, county_DMA$STATE, sep = ",")
county_DMA$Geo <- tolower(county_DMA$Geo)

acs$`Geographic Area Name` <- tolower(acs$`Geographic Area Name`)
#PR isn't in any DMA
acs <- acs[!grepl("puerto rico", acs$`Geographic Area Name`),]
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "county",
                                              replacement = "")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = " , ",
                                              replacement = ",")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "alabama",
                                              replacement = "al")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "arizona",
                                              replacement = "az")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "alaska",
                                              replacement = "ak")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "arkansas$",
                                              replacement = "ar")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "california$",
                                              replacement = "ca")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "colorado$",
                                              replacement = "co")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "connecticut",
                                              replacement = "ct")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "delaware$",
                                              replacement = "de")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = " district of columbia$",
                                              replacement = "dc")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "florida",
                                              replacement = "fl")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "miami-dade",
                                              replacement = "dade")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "georgia",
                                              replacement = "ga")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "hawaii$",
                                              replacement = "hi")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "idaho$",
                                              replacement = "id")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "illinois",
                                              replacement = "il")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "indiana$",
                                              replacement = "in")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "iowa$",
                                              replacement = "ia")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "kansas$",
                                              replacement = "ks")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "kentucky$",
                                              replacement = "ky")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "louisiana$",
                                              replacement = "la")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = ", ",
                                              replacement = ",")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "lasalle",
                                              replacement = "la salle")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "dekalb,in",
                                              replacement = "de kalb,in")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "laporte",
                                              replacement = "la porte")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "maine$",
                                              replacement = "me")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "maryland$",
                                              replacement = "md")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "massachusetts$",
                                              replacement = "ma")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "michigan$",
                                              replacement = "mi")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "minnesota$",
                                              replacement = "mn")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "mississippi$",
                                              replacement = "ms")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "missouri$",
                                              replacement = "mo")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "montana$",
                                              replacement = "mt")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "nebraska$",
                                              replacement = "ne")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "nevada$",
                                              replacement = "nv")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "new hampshire$",
                                              replacement = "nh")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "new jersey$",
                                              replacement = "nj")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "new mexico$",
                                              replacement = "nm")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "de baca",
                                              replacement = "debaca")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "new york$",
                                              replacement = "ny")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "north carolina$",
                                              replacement = "nc")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "north dakota$",
                                              replacement = "nd")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "ohio$",
                                              replacement = "oh")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "oklahoma$",
                                              replacement = "ok")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "oregon$",
                                              replacement = "or")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "pennsylvania$",
                                              replacement = "pa")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "mckean",
                                              replacement = "mc kean")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "rhode island$",
                                              replacement = "ri")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "south carolina$",
                                              replacement = "sc")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "south dakota$",
                                              replacement = "sd")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "tennessee$",
                                              replacement = "tn")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "texas$",
                                              replacement = "tx")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "utah$",
                                              replacement = "ut")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "vermont$",
                                              replacement = "vt")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "virginia$",
                                              replacement = "va")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "washington$",
                                              replacement = "wa")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "west va$",
                                              replacement = "wv")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "wisconsin$",
                                              replacement = "wi")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "wyoming$",
                                              replacement = "wy")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "anchorage municipality",
                                              replacement = "anchorage borough")
acs$`Geographic Area Name` <- str_replace_all(string = acs$`Geographic Area Name`,
                                              pattern = "juneau city and borough",
                                              replacement = "juneau borough")

acs$`Geographic Area Name`[!(acs$`Geographic Area Name` %in% county_DMA$Geo)]

county_DMA$Geo[!(county_DMA$Geo %in% acs$`Geographic Area Name`)]

acs_dma <- inner_join(acs, county_DMA, by = c("Geographic Area Name" = "Geo"))

acs_dma %>%
    select(-c(STATE, COUNTY, STATEFP, CNTYFP, CNTYTVHH, DMAINDEX, id,
              `Geographic Area Name`)) -> acs_dma

acs_dma %>%
    group_by(DMA) %>%
    summarise_all(list(sum = sum)) -> acs_dma


scaleDivision <- function(x, y) (x/y)
acs_dma %>%
    mutate_at(vars(starts_with("TotalPop")),
                 scaleDivision, y = acs_dma$TotPop_sum) %>%
    mutate_at(vars(starts_with("Pop5Up")),
              scaleDivision, y = acs_dma$Pop5Up_sum) %>%
    select(-Pop5Up_sum) %>%
    mutate_at(vars(starts_with("Pop25Up")),
              scaleDivision, y = acs_dma$Pop25Up_sum) %>%
    select(-Pop25Up_sum) %>%
    mutate_at(vars(starts_with("TotalCivPop")),
              scaleDivision, y = acs_dma$TotalCivNonInstiPop_sum) %>%
    mutate_at(vars(starts_with("TotalCivIncome")),
              scaleDivision, y = acs_dma$TotalCivNonInstiPop_sum) %>%
    mutate_at(vars(starts_with("Houses")),
              scaleDivision, y = acs_dma$TotalHouseholds_sum) %>%
    mutate_at(vars(starts_with("TotalCivNonInstiUnInsured")),
              scaleDivision, y = acs_dma$TotalCivNonInstiPop_sum) %>%
    mutate_at(vars(starts_with("TotEmp")),
              scaleDivision, y = acs_dma$TotCivEmpPop16Up_sum) %>%
    mutate_at(vars(starts_with("TotWorker")),
              scaleDivision, y = acs_dma$TotalWorkers16Up_sum) %>%
    mutate_at(vars(starts_with("Housing")),
              scaleDivision, y = acs_dma$TotalOccupiedHousingUnits_sum) %>%
    select(-c(TotalHouseholds_sum, TotalCivNonInstiPop_sum,
              TotCivEmpPop16Up_sum, TotalWorkers16Up_sum,
              TotalOccupiedHousingUnits_sum, TotalHousePopulation_sum))-> acs_dma

write_csv(acs_dma, "acs_dma_final.csv")
