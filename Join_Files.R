library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyr)

acs_dma <- read_csv("acs_dma_final.csv")

nyt_dma <- read_csv("Data/cleaned_case_mask_DMAs.csv")

nyt_acs_dma <- left_join(nyt_dma, acs_dma, by = "DMA")

mobility <- read_csv("Data/mobility_output/mobility_agg.csv")

mobility %>%
    mutate(date = ymd(str_sub(id, start = 1, end = 8)),
           DMA = str_sub(id, start = 10)) %>%
    select(-X1) -> mobility

# Filter out data from before the mobility dataset starts
# And after the case data ends
nyt_acs_dma %>%
    filter(date >= "2020-02-15",
           date <= "2020-10-09") -> nyt_acs_dma

nyt_acs_mob <- left_join(nyt_acs_dma, mobility,
                         by = c("date" = "date", "DMA" = "DMA"))

nyt_acs_mob %>%
    select(-id) -> nyt_acs_mob

nyt_acs_mob %>%
    rename(mob_retail_and_rec = w_retail_and_recreation_percent_change_from_baseline,
           mob_grocery = w_grocery_and_pharmacy_percent_change_from_baseline,
           mob_work = w_workplaces_percent_change_from_baseline) -> nyt_acs_mob

# Fill in missing values with the average from their DMA
nyt_acs_mob %>%
    group_by(DMA) %>%
    mutate(mob_retail_and_rec = ifelse(is.na(mob_retail_and_rec),
                                     mean(mob_retail_and_rec,na.rm=TRUE),
                                     mob_retail_and_rec),
           mob_grocery = ifelse(is.na(mob_grocery),
                                     mean(mob_grocery,na.rm=TRUE),
                                     mob_grocery),
           mob_work = ifelse(is.na(mob_work),
                                     mean(mob_work,na.rm=TRUE),
                                     mob_work)) %>%
    ungroup() -> nyt_acs_mob

# For ones that are completely missing, replace with the average value from the
# US for that day
nyt_acs_mob %>%
    group_by(date) %>%
    mutate(mob_retail_and_rec = ifelse(is.na(mob_retail_and_rec),
                                       mean(mob_retail_and_rec,na.rm=TRUE),
                                       mob_retail_and_rec),
           mob_grocery = ifelse(is.na(mob_grocery),
                                mean(mob_grocery,na.rm=TRUE),
                                mob_grocery),
           mob_work = ifelse(is.na(mob_work),
                             mean(mob_work,na.rm=TRUE),
                             mob_work)) %>%
    ungroup() -> nyt_acs_mob

trends <- read_csv("Data/TrendsData.csv")

dma_location <- read_csv("raw_data/dmaGeoCodeCrosswalk.csv")
dma_location <- dma_location[,c("DMA", "geo_code")]

trends <- left_join(trends, dma_location, by = c("geo" = "geo_code"))

#Remove Palm Springs since it doesn't cover a full county
trends %>%
    filter(geo != "US-CA-804") -> trends

trends %>%
    select(date, DMA, hits.adj) -> trends

# Impute missing values in trends
# Take the average search volume for the country for that day since no data
# is available for the DMA
trends %>%
    group_by(date) %>%
    mutate(hits.adj = ifelse(is.na(hits.adj),
                             mean(hits.adj, na.rm = TRUE),
                             hits.adj)) %>%
    ungroup() -> trends

nyt_acs_mob_trend <- left_join(nyt_acs_mob, trends,
                               by = c("date" = "date", "DMA" = "DMA"))

write_csv(nyt_acs_mob_trend, "Data/Final_Merge_COVID.csv")
