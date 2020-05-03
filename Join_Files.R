library(readr)
library(stringr)

acs_dma <- read_csv("acs_dma_final.csv")

nyt_dma <- read_csv("nytimes_dma_4.csv")

nyt_dma %>%
    select(-X1) -> nyt_dma

nyt_acs_dma <- left_join(nyt_dma, acs_dma, by = "DMA")

colSums(is.na(nyt_acs_dma))

write_csv(nyt_acs_dma, "nyt_acs_dma.csv")
