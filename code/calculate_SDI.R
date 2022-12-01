calculate_SDI <- function(icos_site, SDI, input_path) {

  
  approx_normal <- Vectorize(function(prob) {
    C0 <- 2.515517
    C1 <- 0.802853
    C2 <- 0.010328
    d1 <- 1.432788
    d2 <- 0.189269
    d3 <- 0.001308
    
    if (prob <= 0.5) {
      w <- sqrt(log(1 / prob^2 ))
      prob_norm <- (-1) * (w - ( (C0 + C1 * w + C2 * w^2) / (1 + d1 * w + d2*w^2 + d3*w^3) ) )
    } else if (prob > 0.5) {
      w <- sqrt(log(1 / (1 - prob )^2))
      prob_norm <- w - ( (C0 + C1 * w + C2 * w^2) / (1 + d1 * w + d2*w^2 + d3*w^3) ) 
    }
    return(prob_norm)
  })
  
  kde_per_doy <- function(j) {
    x <- df %>%
      dplyr::filter(Doy == j) %>%
      select(Date, BAL = paste0("BAL_",i)) %>%
      drop_na() %>%
      mutate(agg = i)
    
    xbal <-x %>% pull(BAL)
    
    # calculate bandwidth
    f_kde <- kde(xbal)
    # get probabilities
    
    x$prob <- pkde(xbal, f_kde)
    return(x)
  }
  
  kde_per_doy_prec <- function(j) {
    x <- df %>%
      dplyr::filter(Doy == j) %>%
      select(Date, BAL = paste0("PREC_",i)) %>%
      drop_na() %>%
      mutate(agg = i)
    
    xbal <-x %>% pull(BAL)
    prob_zero <- length(which(xbal==0)) / length(xbal)
    
    if(length(xbal[xbal!=0]) > 2) {
      # calculate bandwidth
      f_kde <- kde(xbal[xbal!=0])
      # get probabilities
      
      x$prob <- prob_zero + (1-prob_zero)*pkde(xbal, f_kde)
    } else {
      x$prob <- 1
    }
    return(x)
  }
  
  kde_per_doy_sm <- function(j) {
    x <- df %>%
      dplyr::filter(Doy == j) %>%
      select(Date, BAL = paste0("SM_",i)) %>%
      drop_na() %>%
      mutate(agg = i)
    
    xbal <-x %>% pull(BAL)
    
    # calculate bandwidth
    f_kde <- kde(xbal)
    # get probabilities
    
    x$prob <- pkde(xbal, f_kde)
    return(x)
  }
  
  file <- paste0(input_path,icos_site)
  df <- read_csv(file)

  agg <- c(seq(5,365,5),seq(370,720,10))
  n <- 365
  start <- min(year(df$Date))
  end <- max(year(df$Date))
  nn <- end - start +1 
  
  dummy <- data.frame()
  plan(multisession)
  
  if(SDI == "SPEI") {
    df$bal <- df$PREC - df$PET
    #calculate aggregated water balance
    list_bal <- lapply(agg, function(x) {y <- data.table::frollmean(df$bal, x, fill = NA, align = "right", na.rm = T); return(y)})
    df <- bind_cols(df, as_tibble(list_bal, .name_repair = function(x) paste0("BAL_", agg)))
    
    for(i in agg){
      print(c("aggregation time:", i))
      # i = time-scale for aggregation
      #KDE for each Doy
      list <- future_lapply(1:365, kde_per_doy)
      dummy <- bind_rows(dummy, bind_rows(list))
    }
  } else if(SDI == "SPI") {
    #calculate aggregated water balanc
    list_prec <- lapply(agg, function(x) {y <- data.table::frollsum(df$PREC, x, fill = NA, align = "right", na.rm = T); return(y)})
    df <- bind_cols(df, as_tibble(list_prec, .name_repair = function(x) paste0("PREC_", agg)))
    
    for(i in agg){
      print(c("aggregation time:", i))
      # i = time-scale for aggregation
      #KDE for each Doy
      list <- future_lapply(1:365, kde_per_doy_prec)
      dummy <- bind_rows(dummy, bind_rows(list))
    }
  } else if(SDI == "SMI") {
    
    for(i in c("top","full")){
      print(c("aggregation time:", i))
      # i = time-scale for aggregation
      #KDE for each Doy
      list <- future_lapply(1:365, kde_per_doy_sm)
      dummy <- bind_rows(dummy, bind_rows(list))
    }
  }
  
  dummy <- dummy %>%
    mutate(SDI = approx_normal(prob)) %>%
    rename(!!SDI := SDI) %>% 
    select(-c(prob, starts_with("BAL"), starts_with("PREC"))) %>%
    pivot_longer(-c(Date, agg)) %>%
    pivot_wider(names_from = c(name, agg))
  
  df <- left_join(df, dummy, by = "Date") %>%
    select(Date, starts_with("SMI"), starts_with("SPI"), starts_with("SPEI"))
  
  file_out <- paste0(SDI,"_",str_remove(icos_site, "_input"))
  
  write_csv(df, paste0("data/SDI/",SDI,"/",file_out))
}
