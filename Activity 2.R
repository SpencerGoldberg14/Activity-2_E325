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

# In-Class Prompt 3: find the earliest date that each river reached the flood stage
flood_date <- floods %>%
  filter(gheight.ft>=flood.ft) %>%
  group_by(names) %>%
  summarize(min_date=min(dateF))

# HW Question 1: Make a separate plot of the stream stage data for each river
# new data frame for palmade 
palmade <- floods[floods$siteID == 2256500,]

# plot palmade
plot(palmade$dateF, 
     palmade$gheight.ft,
     type = "p",
     xlab = "Date",
     ylab = "Stream Stage (ft)",
     main = "Palmade Stream Stage Data")

# new data frame for trilby
trilby <- floods[floods$siteID == 2312000,]

# plot trilby
plot(trilby$dateF,
     trilby$gheight.ft,
     type = "p",
     xlab = "Date",
     ylab = "Stream Stage (ft)",
     main = "Trilby Stream Stage Data")

# new data frame for fort white
fort_white <- floods[floods$siteID == 2322500,]

# plot fort white
plot(fort_white$dateF,
     fort_white$gheight.ft,
     type = "p",
     xlab = "Date",
     ylab = "Stream Stage (ft)",
     main = "Fort White Stream Stage Data")

# new data frame for zolfo springs
zolfo_springs <- floods[floods$siteID == 2295637,]

# plot zolfo springs
plot(zolfo_springs$dateF,
     zolfo_springs$gheight.ft,
     type = "p",
     xlab = "Date",
     ylab = "Stream Stage (ft)",
     main = "Zolfo Springs Stream Stage Data")

# HW Question 2: What was the earliest date of occurrence for each flood category in each river? 
flood_action <- floods %>%
  filter(gheight.ft>=action.ft) %>%
  group_by(names) %>%
  summarize(min_date=min(dateF))

flood_flood <- floods %>%
  filter(gheight.ft>=flood.ft) %>%
  group_by(names) %>%
  summarize(min_date=min(dateF))

flood_moderate <- floods %>%
  filter(gheight.ft>=moderate.ft) %>%
  group_by(names) %>%
  summarize(min_date=min(dateF))

flood_major <- floods %>%
  filter(gheight.ft>=major.ft) %>%
  group_by(names) %>%
  summarize(min_date=min(dateF))

# HW Question 3: Which river had the highest stream stage above its listed height in the major flood category?
stream_major <- floods$gheight.ft - floods$major.ft
river_major <- floods$names[which(stream_major == max(stream_major))]
print(river_major)