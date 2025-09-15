# -------------------------------------------------------------------------
# SCRIPT TO PREPARE THE DATA: S-index (avaiable) - missing species counts / site / year
# Author: [Papallones Urbanitas Team: YM]
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
path <- "/Volumes/GoogleDrive/My Drive/DATA/s-indexes_ebms_Until2023" #YM computer

# bind all files
list.files(path, pattern="\\.csv$", full.names=TRUE) 
Db <- rbindlist(lapply(list.files(path, pattern="\\.csv$", full.names=TRUE), fread))

# este va a ser el mejor TFG ever