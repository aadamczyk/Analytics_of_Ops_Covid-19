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

nyt_acs_mob <- left_join(nyt_acs_dma, mobility,
                         by = c("date" = "date", "DMA" = "DMA"))

missing <- nyt_acs_mob[!complete.cases(nyt_acs_mob),]

missing %>%
    select(date, DMA, id) -> missing

nyt_acs_mob <-

write_csv(nyt_acs_dma, "nyt_acs_dma.csv")
