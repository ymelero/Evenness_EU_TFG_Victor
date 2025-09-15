# -------------------------------------------------------------------------
# SCRIPT TO PREPARE THE DATA: S-index (avaiable) - missing species counts / site / year
# Author: [Papallones Urbanitas Team: YM, DGC]
# Inputs:
#   - eBMS counts
#   - eBMS s-index
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
path <- "/Users/SUITCASE/Downloads/sindex_results" #YM computer

# bind all files
list.files(path, pattern="\\.csv$", full.names=TRUE) 
sindex_eu <- rbindlist(lapply(list.files(path, pattern="\\.csv$", full.names=TRUE), fread))
write.table(sindex_eu, "Data/Db_sindex_all.txt", row.names = F, sep=";")
# once done you can directly read the DB from your local folder:
sindex_eu <- read.delim("Data/Db_sindex_all.txt", sep=";")

## 2.2 eBMS raw count data. To merge with s-index and detect those that failed
count <- read.csv2("Data/ebms_count.csv", sep=",")

## 2.3 merge (unir) las DBs filling those that were count but there is no info on s-index
# columns to be merged, need to have same names:
sindex_eu <- sindex_eu %>%
  rename(YEAR = M_YEAR)  

count <- count %>%
  rename(
    SITE_ID   = transect_id,
    SPECIES   = species_name,
    YEAR      = year   # si tu columna de año se llama distinto, renómbrala aquí
  )

# calculate counts per species, site and year (now they are by week)
count_year <- count %>%
  group_by(SITE_ID, SPECIES, YEAR) %>%
  summarise(COUNT = sum(count, na.rm = TRUE))

# YM TO CHECK WHY counts_year HAS LESS ROWS THAT SINDEX. dups?


# Merge DBs, add colums of counts and of failed:
sindex_eu <- sindex_eu %>%
  full_join(count_year, by = c("SITE_ID","SPECIES","YEAR")) %>%
  mutate(
    COUNT  = replace_na(COUNT, 0),
    FAILED = ifelse(COUNT > 0 & is.na(SINDEX), "failed", "ok")
  )

# YM TO CHECK: There are some sindex 0s ==> ok (Not failed) pero que tienen conteos NO zero!! ==> NEEDS TO BE CORRECTED
plot(sindex_full$SINDEX, sindex_full$COUNT)
# este va a ser el mejor TFG ever