plot_technical_validation <- function() {
  # read E-OBS data
  path_eobs <- "data/EOBS/csv_output/"
  df_eobs <- dir(path_eobs) %>%
    set_names() %>%
    map(~ read_csv(file.path(path_eobs, .)))  %>%
    imap(~ transform(.x, site = .y)) %>%
    bind_rows() %>%
    mutate(site = str_remove(site, "_input.csv"))
  
  # read ICOS data
  df_icos <- read_csv("data/ICOS/WW20_dd.csv")
  
  df_all <- left_join(df_icos, 
                      df_eobs, by = c("site", "Date"))
  
  df_plot <- df_all %>%
    mutate(ET_icos = bigleaf::LE.to.ET(LE_F_MDS, TA_F)*(24*60*60)) %>%
    select(Date, site, ecosystem,
           PREC_eobs = PREC, 
           PREC_icos = P_F, 
           TN_icos = Tmin, 
           TX_icos = Tmax, 
           TN_eobs = TN, 
           TX_eobs = TX,
           PET_eobs = PET.y,
           PET_icos = PET.x,
           AET_eobs = AET,
           AET_icos = ET_icos,
           zSM_eobs = SM_top,
           zSM_icos = SWC_F_MDS_1,
    ) %>%
    group_by(site) %>%
    mutate(zSM_eobs = scale(zSM_eobs),
           zSM_icos = scale(zSM_icos)
    ) %>%
    ungroup() %>%
    pivot_longer(-c(Date, site, ecosystem)) %>%
    mutate(source = str_extract(name, "eobs|icos"),
           var = str_remove(name, "_eobs|_icos")) %>%
    mutate(source = str_to_upper(source)) %>%
    select(-name) %>%
    pivot_wider(names_from = "source") 
  
  df_compare_sites <- df_plot %>%
    drop_na() %>%
    group_by(site, var) %>%
    summarise(nRMSE = caret::RMSE(EOBS, ICOS) / (max(ICOS) - min(ICOS)),
              R2 = caret::R2(EOBS, ICOS),
              n = n()) %>%
    mutate(R2 = as.vector(R2)) %>%
    arrange(R2)
  write_csv(df_compare_sites, "data/metrics_eobs_icos_comparison.csv")
  
  plot_metrics <- function(metric_of_interest) {
    df_compare_sites %>%
      rename(metric = !!metric_of_interest) %>%
      ggplot(aes(x = var, y = metric)) +
      #geom_violin() + 
      stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", color = "black") +
      theme_cowplot() +
      xlab(NULL)  + ylab(paste0(metric_of_interest)) +
      background_grid(major = "y") +
      {if(metric_of_interest == "nRMSE") scale_y_continuous(limits = c(0,0.25), expand = c(0,0))} + 
      {if(metric_of_interest == "R2") scale_y_continuous(limits = c(0,1), expand = c(0,0))}
  }
  
  plot_comparison <- function(var_of_interest, zero_lines = TRUE, coords_obs_pred = TRUE) {
    df_plot %>%
      dplyr::filter(var == var_of_interest) %>%
      ggplot(aes(ICOS, EOBS)) +
      {if(zero_lines) geom_hline(yintercept = 0, linewidth = 0.2) } + 
      {if(zero_lines) geom_vline(xintercept = 0, linewidth = 0.2) } +
      geom_bin2d(bins = 250) +
      theme_cowplot() +
      scale_fill_continuous(type = "viridis") +
      geom_abline(linetype = "dashed") +
      {if(coords_obs_pred) tune::coord_obs_pred()}
  }
  
  a <- plot_comparison("TN") + xlab(expression("ICOS TN " (degree*C))) + ylab(expression("E-OBS TN " (degree*C)))
  b <- plot_comparison("TX") + xlab(expression("ICOS TX " (degree*C))) + ylab(expression("E-OBS TX " (degree*C)))
  c <- plot_comparison("PREC", zero_lines = F, coords_obs_pred = F) +
    scale_x_continuous(trans = "pseudo_log", limits = c(0.01,256), expand = c(0,0), breaks = c(1,5,25,50, 100,250), 
                       labels = c(1,5,25,"","",250)) +
    scale_y_continuous(trans = "pseudo_log", limits = c(0.01,256), expand = c(0,0), breaks = c(1,5,25,50,100,250)) +
    coord_equal() + xlab("ICOS PREC (mm)") + ylab("E-OBS PREC (mm)")
  d <- plot_comparison("PET", zero_lines = F) + xlab("ICOS PET (mm)") + ylab("E-OBS PET (mm)") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  e <- plot_comparison("AET", coords_obs_pred = F) +
    scale_x_continuous(trans = "pseudo_log", limits = c(-1,10), expand = c(0,0), breaks = c(0,1,2,5,10)) +
    scale_y_continuous(trans = "pseudo_log", limits = c(-1,10), expand = c(0,0), breaks = c(0,1,2,5,10)) +
    coord_equal() + xlab("ICOS AET (mm)") + ylab("mHM AET (mm)")
  f <- plot_comparison("zSM") + xlab("ICOS zSM 30cm") + ylab("mHM zSM_top")
  
  ppA <- cowplot::plot_grid(a,b,c,d,e,f, nrow = 2, labels = "auto")
  
  g <- plot_metrics("nRMSE")
  h <- plot_metrics("R2")
  ppB <- plot_grid(g, h, nrow = 1, labels = c("g","h"))
  
  pp_final <- plot_grid(ppA, ppB, rel_heights = c(3,1), nrow = 2)
  
  save_plot("plots/comparison_eobs_icos.png", pp_final, base_width = 11, base_height = 7, bg = "white")
  
  
}




#rm(df_eobs)

#df_icos %>% select(Date, ID, site)
#df_eobs %>% select(Date, ID, site)

#metrics


