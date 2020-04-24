library(tidyverse)

cleanACS <- function(df) {
    df %>%
        select(-starts_with("Margin")) %>%
        transmute_all(as.numeric) -> temp
    temp$id <- df$id
    temp$`Geographic Area Name` <- df$`Geographic Area Name`
    return(temp)
}

# Age ---------------------------------------------------------------------

age <- read_csv("../Raw Data/ACSST1Y2018.S0101_data_with_overlays_2020-04-24T114534.csv",
                       skip = 1)
acs <- cleanACS(age)

# Race --------------------------------------------------------------------

race <- read_csv("../Raw Data/ACSDT1Y2018.B02001_data_with_overlays_2020-04-24T114843.csv",
                       skip = 1)
race <- cleanACS(race)

acs <- left_join(acs, race, by = "id")

# Ethnicity ---------------------------------------------------------------

ethnicity <- read_csv("../Raw Data/ACSDT1Y2018.B04004_data_with_overlays_2020-04-24T115051.csv",
                 skip = 1)
ethnicity <- cleanACS(ethnicity)

acs <- left_join(acs, ethnicity, by = "id")


# Housing Type ---------------------------------------------------------------

housetype <- read_csv("../Raw Data/ACSST1Y2018.S1101_data_with_overlays_2020-04-24T115854.csv",
                      skip = 1)
housetype <- cleanACS(housetype)

acs <- left_join(acs, housetype, by = "id")

# Disability ---------------------------------------------------------------

disability <- read_csv("../Raw Data/ACSST1Y2018.S1810_data_with_overlays_2020-04-24T115954.csv",
                      skip = 1)
disability <- cleanACS(disability)

acs <- left_join(acs, disability, by = "id")

# Insurance ---------------------------------------------------------------

insurance <- read_csv("../Raw Data/ACSST1Y2018.S2703_data_with_overlays_2020-04-24T120143.csv",
                       skip = 1)
insurance <- cleanACS(insurance)

acs <- left_join(acs, insurance, by = "id")

# Education ---------------------------------------------------------------

education <- read_csv("../Raw Data/ACSST1Y2018.S1501_data_with_overlays_2020-04-24T120238.csv",
                      skip = 1)
education <- cleanACS(education)

acs <- left_join(acs, education, by = "id")

# Transportation ---------------------------------------------------------------

transportation <- read_csv("../Raw Data/ACSST1Y2018.S0801_data_with_overlays_2020-04-24T120332.csv",
                      skip = 1)
transportation <- cleanACS(transportation)

acs <- left_join(acs, transportation, by = "id")

# Employement ---------------------------------------------------------------

employement <- read_csv("../Raw Data/ACSST1Y2018.S2301_data_with_overlays_2020-04-24T120418.csv",
                           skip = 1)
employement <- cleanACS(employement)

acs <- left_join(acs, employement, by = "id")

# Occupation ---------------------------------------------------------------

occupation <- read_csv("../Raw Data/ACSST1Y2018.S2411_data_with_overlays_2020-04-24T120502.csv",
                        skip = 1)
occupation <- cleanACS(occupation)

acs <- left_join(acs, occupation, by = "id")

# Income ---------------------------------------------------------------

income <- read_csv("../Raw Data/ACSST1Y2018.S1901_data_with_overlays_2020-04-24T120618.csv",
                   skip = 1)
income <- cleanACS(income)

acs <- left_join(acs, income, by = "id")

# Poverty ---------------------------------------------------------------

poverty <- read_csv("../Raw Data/ACSST1Y2018.S1701_data_with_overlays_2020-04-24T120714.csv",
                   skip = 1)
poverty <- cleanACS(poverty)

acs <- left_join(acs, poverty, by = "id")

# Foodstamps ---------------------------------------------------------------

foodstamps <- read_csv("../Raw Data/ACSST1Y2018.S2201_data_with_overlays_2020-04-24T120755.csv",
                    skip = 1)
foodstamps <- cleanACS(foodstamps)

acs <- left_join(acs, foodstamps, by = "id")

# Household ---------------------------------------------------------------

household <- read_csv("../Raw Data/ACSST1Y2018.S2501_data_with_overlays_2020-04-24T120833.csv",
                       skip = 1)
household <- cleanACS(household)

acs <- left_join(acs, household, by = "id")
