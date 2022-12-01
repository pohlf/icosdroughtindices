# process warm winter
aggregate_ww20 <- function() {
  
  files <- list.files(path = "/home/fpohl/Dokumente/WW20", 
                      pattern = "FULLSET_DD", 
                      full.names = T, 
                      recursive = T)
  
  data <- lapply(files, function(x) read_csv(x) %>% 
                   mutate(site = str_split(x, pattern = "_")[[1]][2])) %>% 
    bind_rows() %>%
    na_if(-9999) %>% 
    mutate(Date = lubridate::ymd(TIMESTAMP))
  
  meta <- read_csv("data/ICOS/meta_full.csv")
  meta <- meta %>% select(site, ID, lon, lat, elevation, ecosystem)
  
  data <- data %>% left_join(., meta, by = "site")
  
  ### QC ###
  ### SWC QC
  
  names(data %>% select(contains("Reco")))
  
  data_qc <- data %>%
    mutate(GPP_NT_VUT_MEAN_QC = 1,
           GPP_DT_VUT_MEAN_QC = 1,
           RECO_NT_VUT_MEAN_QC = 1,
           RECO_DT_VUT_MEAN_QC = 1) %>%
    select(Date, site, site, ID, lon, lat, elevation, ecosystem,
           starts_with("SWC"), 
           NEE_VUT_MEAN, NEE_VUT_MEAN_QC,
           GPP_NT_VUT_MEAN, GPP_NT_VUT_MEAN_QC,
           GPP_DT_VUT_MEAN, GPP_DT_VUT_MEAN_QC,
           RECO_NT_VUT_MEAN, RECO_NT_VUT_MEAN_QC,
           RECO_DT_VUT_MEAN, RECO_DT_VUT_MEAN_QC,
           TA_F, TA_F_QC, 
           P_F, P_F_QC,
           SW_IN_F, SW_IN_F_QC,
           LE_F_MDS, LE_F_MDS_QC) %>%
    pivot_longer(-c(Date,site, ID, lon, lat, elevation, ecosystem)) %>%
    mutate(type = if_else(str_detect(name, "QC"), "QC", "value")) %>%
    mutate(name = str_remove(name, "_QC")) %>%
    pivot_wider(names_from = "type") %>%
    mutate(value = if_else(QC == 1, value, NA_real_)) %>%
    select(-QC) %>%
    drop_na() %>%
    pivot_wider()
  
  # half hourly for tmin/tmax
  files_hh <- list.files(path = "/home/fpohl/Dokumente/WW20", 
                         pattern = "FULLSET_HH", 
                         full.names = T, 
                         recursive = T)
  
  df_hh <- tibble()
  for(i in files_hh) {
    print(i)
    x <- read_csv(i) %>% 
      mutate(site = str_split(i, pattern = "_")[[1]][2]) %>%
      mutate(Date.time = ymd_hm(TIMESTAMP_START)) %>%
      group_by(site, Date = date(Date.time)) %>%
      summarise(Tmin = min(TA_F, na.rm = T),
                Tmax = max(TA_F, na.rm = T)) %>%
      na_if(-9999)
    
    df_hh <- bind_rows(df_hh, x)
    rm(x)
  }
  
  data_qc <- data_qc %>% left_join(., df_hh) 
  
  
  ### ------------------------------------------
  ### PET Hargreaves
  ### ------------------------------------------
  
  ### 1. estimate Rg after Allen et al. (1994)
  
  # number of day in the year
  doy <- yday(data_qc$Date)
  
  # solar declination in rad
  delta <- 0.409 * sin(0.0172 * doy - 1.39)
  
  # relative distance Earth-Sun
  dr <- 1 + 0.033 * cos(0.0172 * doy)
  
  # sunset hour angle in rad
  latrad <- data_qc$lat/57.2957795 # = 1 rad
  sunset <- -tan(latrad) * tan(delta)
  omegas <- sunset * 0
  omegas[abs(sunset)<=1] <- acos(sunset[abs(sunset)<=1])
  
  # Rg, MJ m-2 d-1
  data_qc$SW_IN_est <- 37.6* dr *(omegas * sin(latrad) * sin(delta) + cos(latrad) * cos(delta) * sin(omegas))
  
  #ggplot(data, aes(SW_IN_F, Ra)) +
  #  geom_point()
  
  ### 2. PET Hargreaves
  data_qc$PET <- 0.0023 * 0.408 * (data_qc$SW_IN_F/10) * (data_qc$TA_F + 17.8) * (data_qc$Tmax - data_qc$Tmin)^0.5
  data_qc$PET_est <- 0.0023 * 0.408 * data_qc$SW_IN_est * (data_qc$TA_F + 17.8) * (data_qc$Tmax - data_qc$Tmin)^0.5
  data_qc$PET <- ifelse(data_qc$PET < 0, 0, data_qc$PET)
  data_qc$PET_est <- ifelse(data_qc$PET_est < 0, 0, data_qc$PET_est)
  
  #write data
  write_csv(data_qc, "data/ICOS/WW20_dd.csv")
  return(NULL)
}