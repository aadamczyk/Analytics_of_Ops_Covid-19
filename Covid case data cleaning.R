##15.774 - Analytics of Operations Management
##Final project - Optimizing covid vaccine distribution

install.packages("tidyverse")

#set working directory
setwd("C:/Users/Nikhil/Dropbox (MIT)/MIT/01 - Sloan/02 - Fall 2020 Semester/15.774 - Analytics of Operations Management/Final Project/Data")

#clear environments
cat("\014")
rm(list = ls(all.names = TRUE))

##Load and sort covid case data
library(tidyverse)
case_df = read.csv(file="us-counties.csv")
head(case_df)

case_df_sort = case_df[order(case_df$fips, case_df$date),]

##--Function to create new case count and death data for each county--##
clean_data <- function(df){
  temp_df <- arrange(df, fips, date)
  
#calculate new daily cases
  temp_df <- temp_df %>% mutate(new_cases = ifelse(is.na(lag(cases)), cases, ifelse(cases - lag(cases) < 0, 0, cases - lag(cases))))
  
#calculate new daily deaths
  temp_df <- temp_df %>% mutate(new_deaths = ifelse(is.na(lag(deaths)), deaths, ifelse(deaths - lag(deaths) < 0, 0, deaths - lag(deaths))))
  
  return(temp_df)
}

#create dataframe of cleaned data (removes 6,835 values not tied to a county, 53221763/949813075 cases ~5%)
results_df <- data.frame(Date=as.Date(character()),
                         county=character(), 
                         state=character(), 
                         fips=integer(), cases = integer(), deaths = integer(), new_cases = integer(), new_deaths = integer())

for(fip in unique(case_df_sort$fips)){
    df_calc <- case_df_sort %>% filter(fips == fip)
    df_clean <- clean_data(df_calc)
    results_df <- rbind(results_df, df_clean)
}

##merge mask data
mask_df = read.csv(file="mask-use-by-county.csv")
merged_df <- merge(mask_df, results_df, by.x = "COUNTYFP", by.y = "fips", all.y = TRUE)

#remove counties with no mask data (Puerto Rico, Virgin Islands, Northern Mariana Islands --> 15,597 observations)
clean_df <- merged_df %>% filter(is.na(NEVER) == FALSE)

#write to .csv
write.csv(clean_df, 'cleaned_case_mask_data.csv')


#unused
# length(unique(results_df$fips))
# length(unique(case_df$fips))
# case_df %>% group_by(fips) %>% count(fips, sort = TRUE)

# merged_df %>% filter(is.na(NEVER)) %>% group_by(state) %>% count(state, sort = TRUE)
