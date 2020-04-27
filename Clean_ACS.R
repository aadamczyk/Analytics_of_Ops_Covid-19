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

age <- read_csv("../Raw Data/ACSST5Y2018.S0101_data_with_overlays_2020-04-26T231254.csv",
                       skip = 1)
acs <- cleanACS(age)

# Language ---------------------------------------------------------------------

language <- read_csv("../Raw Data/ACSST5Y2018.S1603_data_with_overlays_2020-04-26T231636.csv",
                skip = 1)

language <- cleanACS(language)

acs <- left_join(acs, language, by = "id")

# Disability ---------------------------------------------------------------------

disability <- read_csv("../Raw Data/ACSST5Y2018.S1810_data_with_overlays_2020-04-26T232305.csv",
                     skip = 1)

disability <- cleanACS(disability)

acs <- left_join(acs, disability, by = "id")

# Food Stamps ---------------------------------------------------------------------

foodStamps <- read_csv("../Raw Data/ACSST5Y2018.S2201_data_with_overlays_2020-04-26T232507.csv",
                       skip = 1)

foodStamps <- cleanACS(foodStamps)

acs <- left_join(acs, foodStamps, by = "id")

# Health Insurance ---------------------------------------------------------------------

healthInsurance <- read_csv("../Raw Data/ACSST5Y2018.S2701_data_with_overlays_2020-04-26T232706.csv",
                       skip = 1)

healthInsurance <- cleanACS(healthInsurance)

acs <- left_join(acs, healthInsurance, by = "id")

# Employment ---------------------------------------------------------------------

employment <- read_csv("../Raw Data/ACSST5Y2018.S2407_data_with_overlays_2020-04-26T232937.csv",
                            skip = 1)

employment <- cleanACS(employment)

acs <- left_join(acs, employment, by = "id")

# Commute ---------------------------------------------------------------------

commute <- read_csv("../Raw Data/ACSST5Y2018.S0802_data_with_overlays_2020-04-26T233517.csv",
                       skip = 1)

commute <- cleanACS(commute)

acs <- left_join(acs, commute, by = "id")

# Housing Traits ---------------------------------------------------------------------

housing <- read_csv("../Raw Data/ACSST5Y2018.S2504_data_with_overlays_2020-04-26T233740.csv",
                    skip = 1)

housing <- cleanACS(housing)

acs <- left_join(acs, housing, by = "id")
