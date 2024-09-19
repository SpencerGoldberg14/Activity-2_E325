#install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

# In-Class Prompt 1: join site info to steam gauge height
floods <- full_join(streamH, siteinfo, by="siteID")
floods <- left_join(streamH, siteinfo, by="siteID")
floods <- right_join(streamH, siteinfo, by="siteID")
floods <- inner_join(streamH, siteinfo, by="siteID") 
# Different join types don't result in different outcomes

# In-Class Prompt 2: parse date for the Floods data frame
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")
year(streamH$dateF)

# In-Class Prompt 3: find the earliest date that each river reached the flood stage
flood_date <- floods %>%
  filter(gheight.ft>=flood.ft) %>%
  group_by(names) %>%
  summarize(min_date=min(dateF))

# HW Question 1: Make a separate plot of the stream stage data for each river
plot.stream.stage <- (siteID = 2256500,
                      si)

example <- floods %>%
  filter(gheight.ft >= 10)
plot(peace$dateF, peace$gheight.ft, type="l")



mutate_floods <- simple_floods %>%
  mutate(gheight.m = gheight.ft * .3048,
         flood.m = flood.ft *.3048)

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

#homework part 1: group 5
which(floodStage$height.ft == 10)
which(floodStage$siteID == 2312000)

# use select to create a df without variables
simple_floods <- floods %>%
  select(-c('agency', 'siteID', 'datetime', 'moderate.ft', 'action.ft'))

# use mutate to create columns for metric units
mutate_floods <- simple_floods %>%
  mutate(gheight.m = gheight.ft * .3048,
         flood.m = flood.ft *.3048)

# simple histogram 
Sante_Fe <- floods %>% filter(siteID = 232500)
hist(Sante_Fe$gheight.ft,
     main = xyz,
     xlab = c(0,16))
