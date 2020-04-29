library(tidyverse)

data <- "../Raw Data/ACSST5Y2018.S0802_data_with_overlays_2020-04-26T233517.csv"
metadata <- "../Raw Data/Metadata/ACSST5Y2018.S0802_metadata_2020-04-26T233517.csv"

cleanACS <- function(data, metadata){
    df <- read_csv(data, skip = 2, col_names = F, guess_max = 3000)
    dfNames <- read_csv(data, n_max = 1)
    dfNames <- colnames(dfNames)

    meta <- read_csv(metadata, col_names = F)

    meta[3][is.na(meta[3])] <- 0

    meta %>%
        filter(X3 == 1) %>%
        select(X1) -> desiredColumns

    df <- df[,dfNames %in% desiredColumns$X1]
    dfNames <- dfNames[dfNames %in% desiredColumns$X1]
    dfNames <- as.data.frame(dfNames, stringsAsFactors = F)
    dfNames <- left_join(dfNames, meta, by = c("dfNames" = "X1"))

    colnames(df) <- dfNames$X4

    df %>%
        transmute_all(as.numeric) %>%
        replace(is.na(.),0) -> temp

    temp$id <- df$id
    if(colnames(temp)[2] == "Geographic Area Name"){
        temp$`Geographic Area Name` <- df$`Geographic Area Name`
    }

    return(temp)
}

# Age ---------------------------------------------------------------------

age <- "../Raw Data/ACSST5Y2018.S0101_data_with_overlays_2020-04-26T231254.csv"
ageMeta <- "../Raw Data/Metadata/ACSST5Y2018.S0101_metadata_2020-04-26T231254.csv"

age <- cleanACS(data = age, metadata = ageMeta)

acs <- age

write_csv(x = acs, path = "ACS.csv")

# Language ---------------------------------------------------------------------

language <- "../Raw Data/ACSST5Y2018.S1603_data_with_overlays_2020-04-26T231636.csv"
languageMeta <- "../Raw Data/Metadata/ACSST5Y2018.S1603_metadata_2020-04-26T231636.csv"

language <- cleanACS(data = language, metadata = languageMeta)

acs <- left_join(acs, language, by = "id")

# Disability ---------------------------------------------------------------------

disability <- "../Raw Data/ACSST5Y2018.S1810_data_with_overlays_2020-04-26T232305.csv"
disabilityMeta <- "../Raw Data/Metadata/ACSST5Y2018.S1810_metadata_2020-04-26T232305.csv"

disability <-  cleanACS(data = disability, metadata = disabilityMeta)

acs <- left_join(acs, disability, by = "id")

# Food Stamps ---------------------------------------------------------------------

foodStamps <- "../Raw Data/ACSST5Y2018.S2201_data_with_overlays_2020-04-26T232507.csv"
foodStampsMeta <- "../Raw Data/Metadata/ACSST5Y2018.S2201_metadata_2020-04-26T232507.csv"

foodStamps <- cleanACS(data = foodStamps, metadata = foodStampsMeta)

acs <- left_join(acs, foodStamps, by = "id")

# Health Insurance ---------------------------------------------------------------------

healthInsurance <- "../Raw Data/ACSST5Y2018.S2701_data_with_overlays_2020-04-26T232706.csv"
healthInsuranceMeta <- "../Raw Data/Metadata/ACSST5Y2018.S2701_metadata_2020-04-26T232706.csv"

healthInsurance <- cleanACS(data = healthInsurance, metadata = healthInsuranceMeta)

acs <- left_join(acs, healthInsurance, by = "id")

# Employment ---------------------------------------------------------------------

employment <- "../Raw Data/ACSST5Y2018.S2407_data_with_overlays_2020-04-26T232937.csv"
employmentMeta <- "../Raw Data/Metadata/ACSST5Y2018.S2407_metadata_2020-04-26T232937.csv"

employment <- cleanACS(data = employment, metadata = employmentMeta)

acs <- left_join(acs, employment, by = "id")

# Commute ---------------------------------------------------------------------

commute <- "../Raw Data/ACSST5Y2018.S0802_data_with_overlays_2020-04-26T233517.csv"
commuteMeta <- "../Raw Data/Metadata/ACSST5Y2018.S0802_metadata_2020-04-26T233517.csv"

commute <- cleanACS(data = commute, metadata = commuteMeta)

acs <- left_join(acs, commute, by = "id")

# Housing Traits ---------------------------------------------------------------------

housing <- "../Raw Data/ACSST5Y2018.S2504_data_with_overlays_2020-04-26T233740.csv"
housingMeta <- "../Raw Data/Metadata/ACSST5Y2018.S2504_metadata_2020-04-26T233740.csv"

housing <- cleanACS(data = housing, metadata = housingMeta)

acs <- left_join(acs, housing, by = "id")

write.csv(acs, file = "ACS.csv", row.names = F)
