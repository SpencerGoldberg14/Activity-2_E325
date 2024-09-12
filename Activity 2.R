#install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

# parse our data
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")

year(streamH$dateF)

# join site info to steam gauge height
floods <- full_join(streamH, siteinfo, by="siteID")

peace <- floods %>% 
  filter(siteID == 2295637)

example <- floods %>%
  filter(gheight.ft >= 10)
plot(peace$dateF, peace$gheight.ft, type="l")

max_ht <- floods %>%
  group_by(names) %>%
  summarize(max_height_ft=max(gheight.ft, na.rm=TRUE),
            mean_ft = mean(gheight.ft, na.rm=TRUE))

#prompt 3: What was the earliest date that each river reached the flood stage?
flood_date <- floods %>%
  filter(gheight.ft>=flood.ft) %>%
  group_by(names) %>%
  summarize(min_date=min(dateF))

