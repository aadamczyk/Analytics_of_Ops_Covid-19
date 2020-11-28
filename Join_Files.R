library(readr)
library(stringr)

acs_dma <- read_csv("acs_dma_final.csv")

nyt_dma <- read_csv("cleaned_case_mask_data.csv")

nyt_dma %>%
    select(-X1) %>%
    mutate(DMA = str_sub(string = Category,
                         start = str_locate(Category, "_")[,1]+1)) -> nyt_dma

nyt_acs_dma <- left_join(nyt_dma, acs_dma, by = "DMA")

colSums(is.na(nyt_acs_dma))

nyt_acs_dma %>%
    filter(is.na(TotPop_sum)) -> missingACS

write_csv(nyt_acs_dma, "nyt_acs_dma.csv")
