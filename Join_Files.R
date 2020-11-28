library(readr)
library(stringr)
library(dplyr)
library(lubridate)

acs_dma <- read_csv("acs_dma_final.csv")

nyt_dma <- read_csv("Data/cleaned_case_mask_DMAs.csv")

nyt_acs_dma <- left_join(nyt_dma, acs_dma, by = "DMA")

mobility <- read_csv("Data/mobility_output/mobility_agg.csv")

mobility %>%
    mutate(date = ymd(str_sub(id, start = 1, end = 8)),
           DMA = str_sub(id, start = 10)) %>%
    select(-X1) -> mobility

# Filter out data from before the mobility dataset starts
nyt_acs_dma %>%
    filter(date >= "2020-02-15") -> nyt_acs_dma

# TODO: should have 210 DMAs, not 216. Have errors in mobility creation file
mobility %>%
    select(DMA) %>%
    unique() %>%
    arrange() -> temp

nyt_acs_mob <- full_join(nyt_acs_dma, mobility,
                         by = c("date" = "date", "DMA" = "DMA"))

# TODO: should have 210 DMAs, not 216. Have errors in mobility creation file
nyt_acs_mob %>%
    select(DMA) %>%
    unique() %>%
    arrange() -> temp

# Missing Data Summary
missing <- nyt_acs_mob[!complete.cases(nyt_acs_mob),]

missing %>%
    select(date:cases, TotPop_sum, id:w_workplaces_percent_change_from_baseline) -> missing

missing %>%
    group_by(DMA) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

mice::md.pattern(missing)

# TODO impute missing variables
# library("Hmisc")
# impute(abalone$height, mean)

nyt_acs_mob %>%
    select(-id) -> nyt_acs_mob

trends <- read_csv("Data/TrendsData.csv")

dma_location <- read_csv("raw_data/dmaGeoCodeCrosswalk.csv")
dma_location <- dma_location[,c("DMA", "geo_code")]

trends <- left_join(trends, dma_location, by = c("geo" = "geo_code"))

# Palm Springs doesn't cover a full county, so we'll drop it
table(trends[!complete.cases(trends),][,"geo"])->temp
trends <- trends[complete.cases(trends),]

#TODO missing data for "ALPENA (MI)" "US-MI-583" - No trend information

trends %>%
    select(date, DMA, hits.adj) -> trends

trends %>%
    select(date, DMA, hits.adj) -> trends

nyt_acs_mob_trend <- left_join(nyt_acs_mob, trends,
                               by = c("date" = "date", "DMA" = "DMA"))

write_csv(nyt_acs_mob_trend, "nyt_acs_mob_trend.csv")
