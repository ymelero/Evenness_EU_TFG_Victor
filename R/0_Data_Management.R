# este va a ser el mejor TFG ever

# -------------------------------------------------------------------------
# SCRIPT TO PREPARE THE DATA: S-index (avaiable) - missing species counts / site / year
# Author: [Papallones Urbanitas Team: YM, DGC]
# Inputs:
#   - eBMS counts with uBMs included. The data comes from the schemes, the code to merge them and to calculate:
#   - eBMS s-index, the code (as above) is in: Gdrive>PROJECTS_DOING>SATURNO>URBAN TRENDS. Also in GitHUb (less complete and no data)
# Outputs:
#   - DB combined: S-index, counts, failed species - rows & extrapolated S-index.
# -------------------------------------------------------------------------

# 1. LIBRARIES & SETUP ---------------------------------------------------
# Load necessary libraries

library(data.table)
library(tidyverse)
#library(lubridate)

# 2. eBMS DATA -----------------------------------------------------------

## 2.1 eBMS s-index data. per species, site, year. Calculated following Colom et al 2025

# **** NOTE: change the "path" to where the data is stored in your computer *****

# path <- "/Users/SUITCASE/Downloads/sindex_results/" #YM computer
path <- "/home/david/trabajo/projects/BCN/butterfly_ebms_data/" #DGC computer

sindex_eu <- read.delim(paste0(path,"Db_sindex_all.txt"),sep=";")

## 2.2 eBMS raw count data. To merge with s-index and detect those that failed
count <- read.csv2(paste0(path,"ebms_uBMS_count_2023.txt"),sep=";")[, 3:9]

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

# calculate counts per species, site and year (now they are by week) in the counts DB
count_year <- count %>%
  group_by(SITE_ID, SPECIES, YEAR) %>%
  summarise(COUNT = sum(COUNT, na.rm = TRUE))

# Merge DBs, add colums of counts and of failed:
# criteria for removing / failed:
 # 1 - Removed sindex == 0 & count NA, since it means absence.
 # 2 - Set as failed if sindex == NA | 0 | Inf & count > 0, since it means absence.
sindex_eu <- sindex_eu %>%
  full_join(count_year, by = c("SITE_ID","SPECIES","YEAR")) %>%
  mutate(
    SUCCESS = ifelse((is.na(SINDEX) & COUNT > 0) |
                       (SINDEX == 0 & COUNT > 0) |
                       (is.infinite(SINDEX) & COUNT > 0),
                     "failed", "ok")
  ) %>%
  filter(!(is.na(COUNT) & SINDEX == 0)) # since it implies the species was absent (not detected) in the site-year

## 2.4 generate a unique bioregion/time id
sindex_eu <- sindex_eu %>%
  unite(REGION_ID, GEO_REGION,RCLIM,sep="--")
table(sindex_eu$REGION_ID)

## 2.5 Check results of sindex-counts to look for potential errors 

# Criteria for outliers:
 # 1 - High sindexes values from non count's outliers => set as failed (see checks below). Visual limit set in Sindex > 20000 (lineal correlation lost there)
 # 2 - High count data: 2.1: remove sites with clear visual outliers (check2) COUNTS > 10000. The rest are already set as failed bc sindex = 0

# YM raw Checks s.indexes' outliers: very high counts with very low sindex and vice versa
# plot(sindex_eu$COUNT,sindex_eu$SINDEX)
# plot(sindex_eu$COUNT,sindex_eu$SINDEX, xlim = c(0,10000), ylim=c(0,50000)) 
# abline(h = 20000, col = "red", lty = 2)

# outliers in sindex high values
check1<- sindex_eu %>% #inf de DE, los demás casi todos BMS Vasco
  filter(SINDEX > 20000, COUNT > 0, COUNT < 10000) %>% #values based on the plot (observational). There are no sindex outliers with COUNT > 10K
  arrange(desc(SINDEX))
# outliers in sindex low values, high count values
# plot(sindex_eu$COUNT, sindex_eu$SINDEX, ylim = c(0,100))
# with(subset(sindex_eu, SUCCESS == "ok"),
#      plot(COUNT, SINDEX, xlim=c(0,200), ylim = c(0,100)))
check2 <- sindex_eu %>%
  filter(SINDEX < 1, COUNT > 9000) %>%  # todas las raras son failed already
  arrange(desc(COUNT)) 

# Apply criteria for outliers
sindex_eu <- sindex_eu %>%
  mutate(SUCCESS = ifelse(SUCCESS == "ok" & SINDEX > 20000 & COUNT < 10000, # criteria 1
                     "failed", SUCCESS)) %>%
  filter(!SITE_ID %in% SITE_ID[SINDEX < 1 & COUNT > 10000]) # criteria 2

# with(subset(sindex_eu_sucess, SUCCESS == "ok"),
#     plot(COUNT, SINDEX))


# 3. Extrapolate FAILED SINDEX  -----------------------------------------------------------

# DB: sindex_eu
# criteria for extrapolating:
# 1 - max count of fail > max count of success: REVISAR SITE 
# 2 - r2 of the model good enough (>0.7)

# so, for each sp-site-year with fails
# 1 - check condition 1 above
# 2 - create linear model using sp-site (all years)
# 3 - get r2 and check condition 2 above
# 4 - if fails, create new linear model using sp-region that year (combo GEO_REGION & RCLIM) 
# 5 - get r2 and check condition 2 above
# 6 - if fails, create new linear model using sp-region all years (combo GEO_REGION & RCLIM) 
# 7 - get r2 and check condition 2 above
# 8 - condition 1 above or 1-8 do not work => remove site for all sp and years?? 


# -------------------------------------------------------------------------

# number of fails/successes per site
# 1 - number of sites?
length(unique(sindex_eu$SITE_ID))
# per region?
sites_per_region <- sindex_eu %>%
  group_by(REGION_ID) %>%
  summarise(num_sites = length(unique(SITE_ID)))

# successes and fails per site
success_per_site <- sindex_eu %>%
  group_by(SITE_ID, YEAR) %>%
  summarise(num_success = sum(SUCCESS == "ok"), 
            num_fails = sum(SUCCESS == "failed"),
            proportion_success = sum(SUCCESS == "ok")/n())
zero_success <- subset(success_per_site, proportion_success == 0)
# length(unique(zero_success$SITE_ID))




