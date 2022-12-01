plot_swc_performance_comparison <- function() {
  # data preparation
  path <- "data/EOBS/csv_output/"
  files <- list.files(path)
  df_eobs <- future_lapply(files, function(x) read_csv(paste0(path,x)) %>% 
                            mutate(site = str_remove(file_path_sans_ext(x), "_input")) %>%
                            select(Date, site, SM_top, SM_full)) %>% 
    bind_rows() 
  
  path <- "data/SDI/SMI/"
  files <- list.files(path)
  df_smi <- future_lapply(files, function(x) read_csv(paste0(path,x)) %>% 
                            mutate(site = str_remove(file_path_sans_ext(x), "SMI_")) %>%
                            select(Date, site, SMI_top, SMI_full)) %>% 
    bind_rows() 
  
  
  path <- "data/SDI/SPEI/"
  files <- list.files(path)
  df_spei <- future_lapply(files, function(x) read_csv(paste0(path,x)) %>% 
                             mutate(site = str_remove(file_path_sans_ext(x), "SPEI_")) %>%
                             select(Date, site, SPEI_90, SPEI_365)) %>% 
    bind_rows() 
  
  path <- "data/SDI/SPI/"
  files <- list.files(path)
  df_spi <- future_lapply(files, function(x) read_csv(paste0(path,x)) %>% 
                            mutate(site = str_remove(file_path_sans_ext(x), "SPI_")) %>%
                            select(Date, site, SPI_90, SPI_365)) %>% 
    bind_rows() 
  
  
  df_ww20 <- read_csv("data/ICOS/WW20_dd.csv") 
  
  df <- df_smi %>% 
    left_join(df_spei) %>%
    left_join(df_spi) %>%
    left_join(df_eobs) %>%
    left_join(df_ww20)
  rm(df_spi, df_spei, df_smi, df_eobs)
  
  fit_rcs <- function(icos_site, target, k) {
    if(target == "GPP") {
      df <- rename(df, target = GPP_NT_VUT_MEAN)
    } else if(target == "NEE") {
      df <- rename(df, target = NEE_VUT_MEAN)
    } else if(target == "Reco") {
      df <- rename(df, target = RECO_NT_VUT_MEAN)
    } else if(target == "ET") {
      df <- rename(df, target = LE_F_MDS)
    }

    data <- df %>% 
      dplyr::filter(site == icos_site) %>%
      dplyr::filter(month(Date) %in% c(5,6,7,8,9)) %>%
      select(Date, 
             target,
             SW_IN = SW_IN_F,
             SWC = SWC_F_MDS_3, 
             SM_top, SMI_top,
             SM_full, SMI_full,
             SPEI_90, 
             SPEI_365,
             SPI_90, 
             SPI_365) %>%
      drop_na()
    
    if(nrow(data) > 50) {
      
      options(datadist = datadist(data))
      mod_swc <- ols(target ~ rcs(SW_IN, k) + rcs(SWC, k), data = data,x = T, y = T)
      mod_sm30 <- ols(target ~ rcs(SW_IN, k) + rcs(SM_top, k), data = data,x = T, y = T)
      mod_sm200 <- ols(target ~ rcs(SW_IN, k) + rcs(SM_full, k), data = data,x = T, y = T)
      
      regression_stats <- function(mod, target) {
        r2 <- mod$stats["R2"]
        aic <- AIC(mod)
        
        #chi square
        anova <- anova(mod)
        dof <- anova[, "d.f."]
        F <- anova[, "F"]
        chisqp <- F * dof - dof
        names <- c("predic1_lin", "predic1_nonlin", "predic2_lin", "predic2_nonlin", "total_nonlin", "total", "error")
        names(chisqp) <- names
        
        summary <- data.frame(
          target = target,
          AIC = aic,
          R2 = r2,
          predic1 = mod$Design$name[1],
          predic2 = mod$Design$name[2],
          site = icos_site)
        
        summary <- as.data.frame(chisqp) %>% 
          rownames_to_column() %>%
          pivot_wider(names_from = rowname, values_from = chisqp) %>%
          bind_cols(summary, .)
        
        rownames(summary) <- NULL
        return(summary)
      }
      
      summary <- lapply(list(mod_swc, mod_sm30,mod_sm200), regression_stats, target) %>% bind_rows()
    } else {
      summary <- NULL
    }
    
    return(summary)
  }
  
  list_of_sites <- unique(df_ww20$site)
  kk <- 5
  list_of_results_gpp <- lapply(list_of_sites, fit_rcs, "GPP", k = kk)
  list_of_results_et <- lapply(list_of_sites, fit_rcs, "ET", k = kk)
  list_of_results_nee <- lapply(list_of_sites, fit_rcs, "NEE", k = kk)
  list_of_results_reco <- lapply(list_of_sites, fit_rcs, "Reco", k = kk)
  
  results <- bind_rows(
    list_of_results_gpp %>% bind_rows(),
    list_of_results_et %>% bind_rows(),
    list_of_results_nee %>% bind_rows(),
    list_of_results_reco %>% bind_rows(),
  )
  
  results <- left_join(results, df_ww20 %>% select(site, ecosystem) %>% distinct())
  
  results <- results %>%
    mutate(predic2 = case_when(
      predic2 == "SWC" ~ "ICOS\n(30cm)",
      predic2 == "SM_top" ~ "mHM\n(top)",
      predic2 == "SM_full" ~ "mHM\n(full)"
    ))
  
  A <- results %>%
    select(target, site, predic = predic2, R2, ecosystem) %>%
    ggplot(aes(reorder(factor(predic),R2,na.rm = TRUE), R2, fill = predic)) +
    theme_cowplot() +
    background_grid(major = "y", minor = "y") +
    stat_halfeye() +
    ylab("R2") + xlab(NULL) + 
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = percent_format()) +
    scale_fill_brewer() +
    facet_wrap(~target, nrow = 1)
  
  B <- results %>%
    mutate(importance = (predic2_lin + predic2_nonlin)/total) %>%
    select(target, site, predic = predic2, importance, R2) %>%
    pivot_longer(-c(site, predic, target, R2)) %>%
    ggplot(aes(reorder(factor(predic),R2,na.rm = TRUE), value, fill = predic)) +
    theme_cowplot() +
    background_grid(major = "y", minor = "y") +
    stat_halfeye() +
    ylab("feature importance") + xlab(NULL) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = percent_format()) +
    theme(legend.position = "none") +
    scale_fill_brewer() +
    facet_wrap(~target, nrow = 1)
  
  plot_out <- plot_grid(A,B, ncol = 1, labels = "auto")  
  save_plot("plots/swc30_predictor_performance.eps", plot_out, bg = "white", base_width = 11, base_height = 7)
  return(NULL)
}


