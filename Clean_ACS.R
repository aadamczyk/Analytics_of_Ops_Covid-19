library(tidyverse)

data <- "../Raw Data/ACSST5Y2018.S0101_data_with_overlays_2020-04-26T231254.csv"
metadata <- "../Raw Data/Metadata/ACSST5Y2018.S0101_metadata_2020-04-26T231254.csv"

cleanACS <- function(data, metadata){
    df <- read_csv(data, skip = 1)
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

    return(df)
}

# Age ---------------------------------------------------------------------

age <- "../Raw Data/ACSST5Y2018.S0101_data_with_overlays_2020-04-26T231254.csv"
ageMeta <- "../Raw Data/Metadata/ACSST5Y2018.S0101_metadata_2020-04-26T231254.csv"

age <- cleanACS(data = age, metadata = ageMeta)

acs <- age

write_csv(x = acs, path = "ACS.csv")
