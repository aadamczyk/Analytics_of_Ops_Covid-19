library(tidyverse)

# Age ---------------------------------------------------------------------

age <- read_csv("../Raw Data/ACSST5Y2018.S0101_data_with_overlays_2020-04-26T231254.csv",
                       skip = 1)
agenames <- read_csv("../Raw Data/ACSST5Y2018.S0101_data_with_overlays_2020-04-26T231254.csv",
                     n_max = 1)
agenames <- colnames(agenames)

ageMeta <- read_csv("../Raw Data/Metadata/ACSST5Y2018.S0101_metadata_2020-04-26T231254.csv",
                    col_names = F)

ageMeta[3][is.na(ageMeta[3])] <- 0

ageMeta %>%
    filter(X3 == 1) %>%
    select(X1) -> desiredColumns

age <- age[,agenames %in% desiredColumns$X1]
agenames <- agenames[agenames %in% desiredColumns$X1]
agenames <- as.data.frame(agenames, stringsAsFactors = F)
agenames <- left_join(agenames, ageMeta, by = c("agenames" = "X1"))

colnames(age) <- agenames$X4

acs <- age

write_csv(x = acs, path = "ACS.csv")
