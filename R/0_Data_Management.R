# este va a ser el mejor TFG ever

# -------------------------------------------------------------------------
# SCRIPT TO PREPARE THE DATA: S-index (avaiable) - missing species counts / site / year
# Author: [Papallones Urbanitas Team: YM, DGC]
# Inputs:
#   - eBMS counts with uBMs included. The data comes from the schemes, the code to merge them and to calculate:
#   - eBMS s-index, the code (as above) is in: Gdrive>PROJECTS_DOING>SATURNO>URBAN TRENDS. Also in GitHUb (less complete and no data)
# Outputs:
#   - DB combined: S-index, counts, failed species - rows
# -------------------------------------------------------------------------

# 1. LIBRARIES & SETUP ---------------------------------------------------
# Load necessary libraries

library(data.table)
library(tidyverse)
#library(lubridate)

# 2. eBMS DATA -----------------------------------------------------------

## 2.1 eBMS s-index data. per species, site, year. Calculated following Colom et al 2025
# Give the path to the data, load them all and create an unique DB. 
# path <- "/Users/SUITCASE/Downloads/sindex_results" #YM computer

# bind all files
# list.files(path, pattern="\\.csv$", full.names=TRUE) 
# sindex_eu <- rbindlist(lapply(list.files(path, pattern="\\.csv$", full.names=TRUE), fread))
# write.table(sindex_eu, "Data/Db_sindex_all.txt", row.names = F, sep=";")
# once done you can directly read the DB from your local folder:
sindex_eu <- read.delim("Data/Db_sindex_all.txt", sep=";")

## 2.2 eBMS raw count data. To merge with s-index and detect those that failed
count <- read.csv2("Data/ebms_uBMS_count_2023.txt", sep=";")[, 3:9]

## 2.3 merge (unir) las DBs filling those that were count but there is no info on s-index
# columns to be merged, need to have same names:
sindex_eu <- sindex_eu %>%
  rename(YEAR = M_YEAR)  

count <- count %>%
  rename(
    #SITE_ID   = transect_id,
    #SPECIES   = species_name,
    YEAR      = year   
  )

# calculate counts per species, site and year (now they are by week) in the counts DBå
count_year <- count %>%
  group_by(SITE_ID, SPECIES, YEAR) %>%
  summarise(COUNT = sum(COUNT, na.rm = TRUE))


# Merge DBs, add colums of counts and of failed:
sindex_eu <- sindex_eu %>%
  full_join(count_year, by = c("SITE_ID","SPECIES","YEAR")) %>%
  mutate(
    SUCCESS = ifelse((is.na(SINDEX) & COUNT > 0) |
                       (SINDEX == 0 & COUNT > 0) |
                       is.infinite(SINDEX),
                     "failed", "ok")
  )

## 2.4 Check results of sindex-counts to look for potential errors 
# YM checked: all count that == NA s-index is zero (as it should be), al sindex that are == NA , have counts (==> are those that failed)
# TBDecided:
 #1. Remove sindex == 0, means absence.
 #2. Check outliers: very high counts with very low sindex and vice versa*
plot(sindex_eu$COUNT,sindex_eu$SINDEX)
plot(sindex_eu$COUNT,sindex_eu$SINDEX, xlim = c(0,10000), ylim=c(0,50000)) 
 # outliers in sindex high values
check1<- sindex_eu %>% #inf de DE, los demás casi todos BMS Vasco
  filter(SINDEX > 10000, COUNT > 0, COUNT < 10000) %>%
  arrange(desc(SINDEX))
# outliers in sindex low values, high count values
plot(sindex_eu$COUNT, sindex_eu$SINDEX, ylim = c(0,100))
with(subset(sindex_eu, SUCCESS == "ok"),
     plot(COUNT, SINDEX, xlim=c(0,200), ylim = c(0,100)))
check2 <- sindex_eu %>%
  filter(SUCCESS == "ok", SINDEX < 10, COUNT > 0, COUNT < 20) %>%
  arrange(desc(SINDEX)) # todas las raras son failed already

# Criteria to deal with outliers
 #1. Check 1=> categorised them all as failed, and then infer from extrapolation. Do not remove => you remove species from the community. Unless the entire site / year is removed
 #2. Check 2=> outliers are all already categorised as failure. To be exptrapolated