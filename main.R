rm(list=ls())
library(tidyverse)
library(lubridate)
library(ks)
library(future.apply)
library(cowplot)
library(janitor)
library(ggplot2)
library(tidymodels)
library(vip)
library(DALEXtra)
library(RColorBrewer)
library(ncdf4)
library(rms)
library(rnaturalearth)
library(sf)
library(introdataviz)
library(ggdist)
library(ggh4x)
library(tools)

sapply(list.files("code", full.names = T), source)

### --------------------------------------------------------------------------
### DATA PROCESSING
### --------------------------------------------------------------------------

#write_eobs_csv_from_nc(start_year = 1950,
#                       end_year = 2021,
#                       input = "data/EOBS/nc_input/",
#                       output = "data/EOBS/csv_output/")

# read list of sites
list_of_sites <- list.files("data/EOBS/csv_output/")

# helper function to avoid overwriting existing files 
sdi_helper_fun <- function(x, SDI) {
  file <- paste0("data/SDI/",SDI,"/",SDI,"_",str_remove(x, "_input"))
  if(file.exists(file)) {
    print(paste0(x, "exists, skipped."))
  } else {
    calculate_SDI(x, SDI = SDI, input_path = "data/EOBS/csv_output/")
  }
}

lapply(list_of_sites, sdi_helper_fun, SDI = "SPEI")
lapply(list_of_sites, sdi_helper_fun, SDI = "SPI")
lapply(list_of_sites, sdi_helper_fun, SDI = "SSMI")

## PREPARE WarmWinter2020 ICOS DATA
# dont run, already aggregated
#aggregate_ww20()

### --------------------------------------------------------------------------
### Plots
### --------------------------------------------------------------------------

# Fig. 2 MAP
plot_map_stations() 
plot_map_ts(station = "FI-Hyy") # takes ~3 minutes

# Fig. 3 Technical Validation
plot_technical_validation()

# summarise performance values
read_csv("data/metrics_eobs_icos_comparison.csv") %>%
  select(-c(site, n)) %>%
  pivot_longer(-c(var)) %>%
  group_by(var, name) %>%
  summarise(mean_cl_boot(value))

# Fig 4. Example SDI Transfromation
plot_schematic_sdi()

# Fig 5. TS of drought occurrence at ICOS ecosystem sites 
plot_ts_icos_affected()

# Fig 6. 
plan(multisession)
plot_swc_performance_comparison()
plan(sequential)

# Fig 7. 
plot_correlation_field() 
