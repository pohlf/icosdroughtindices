write_eobs_csv_from_nc <- function(start_year = 1950, 
                                   end_year = 2021,
                                   input = "data/EOBS/nc_input/",
                                   output = "data/EOBS/csv_output/") {
  
  nn <- end_year - start_year + 1
  input_files <- list.files(path = input)
  
  read_nc <- function(file) {
    # open nc file
    nc_tmp <- nc_open(paste0(input,file))
    # get variable names
    var_names <- attributes(nc_tmp$var)$names
    # extract each variable
    list_var <- lapply(var_names, function(x) ncvar_get(nc_tmp, x))
    # create df from list
    df <- bind_cols(list_var) 
    # set names of vars
    names(df) <- var_names
    
    df <- df %>%
      mutate(Date = seq(as.Date("1950-01-01"), as.Date("2021-12-31"), by = "day")) %>%
      # average out 29th february for all variables
      pivot_longer(-Date) %>%
      group_by(name) %>%
      mutate(value = if_else(format(Date+1,"%m-%d") == "02-29", 
                             zoo::rollmean(value, 2, fill = NA),
                             value)
      ) %>%
      dplyr::filter(format(Date,"%m-%d") != "02-29") %>%
      pivot_wider() %>%
      mutate(Doy = rep(seq(1, 365), nn),
             bal = pre - pet,
             ID = parse_number(file)) %>%
      rename(SM_top = theta30cm_div_thetaS30cm, 
             SM_full = theta_div_thetaS,
             lon = lon_2,
             lat = lat_2)
    return(df)
  }
  
  plan(multisession)
  list_df <- future_lapply(input_files, read_nc)

  site_names <- read_csv("data/ICOS/icos_list_lon_lat.csv") %>% pull(site)
  
  list_df <- setNames(list_df, site_names)
  
  future_lapply(names(list_df), function(x) {
    df <- list_df[[x]] %>% 
      dplyr::select(Date, lon, lat, Doy,
                    PREC = pre,
                    TG = tavg,
                    TN = tmin,
                    TX = tmax,
                    AET = aET,
                    SM_top,
                    SM_full)
    
    ### ------------------------------------------
    ### PET Hargreaves
    ### ------------------------------------------
    
    ### 1. estimate Rg after Allen et al. (1994)
    
    doy <- df$Doy
    
    # solar declination in rad
    delta <- 0.409 * sin(0.0172 * doy - 1.39)
    
    # relative distance Earth-Sun
    dr <- 1 + 0.033 * cos(0.0172 * doy)
    
    # sunset hour angle in rad
    latrad <- df$lat/57.2957795 # = 1 rad
    sunset <- -tan(latrad) * tan(delta)
    omegas <- sunset * 0
    omegas[abs(sunset)<=1] <- acos(sunset[abs(sunset)<=1])
    
    # Rg, MJ m-2 d-1
    df$RG <- 37.6* dr *(omegas * sin(latrad) * sin(delta) + cos(latrad) * cos(delta) * sin(omegas))
    
    # estimate PET
    df$PET <- 0.0023 * 0.408 * (df$RG) * (df$TG + 17.8) * (df$TX - df$TN)^0.5
    df$PET <- ifelse(df$PET < 0, 0, df$PET)

    write_csv(df, file = paste0(output,x,"_input.csv"))
    return(NULL)
  })
}