plot_map_ts <- function(station) {
  
  normalize <- function(x,b,a, ...) {
    return(a + (x - min(x, ...) ) * (b-a) / (max(x, ...) - min(x, ...) ) )
  }
  
  # station SDI files
  station_files <- list.files("data/SDI/", recursive = T, pattern = station, full.names = T)
  
  # read standardized drought indices + ICOS data
  df <- station_files %>%
    map(function(x) read_csv(x) %>% 
          mutate(site = paste0(station))
    ) %>%
    bind_rows() %>% 
    left_join(read_csv(paste0("data/EOBS/csv_output/",station,"_input.csv"))) %>%
    left_join(read_csv("data/ICOS/WW20_dd.csv") %>% filter(site == station), by = c("Date", "site")) %>% 
    rename(SWC_mhm = SM_full, SWC_icos = SWC_F_MDS_5) %>%
    mutate(SWC_mhm = normalize(SWC_mhm, a = min(SWC_icos, na.rm = T), b = max(SWC_icos, na.rm = T))) 
  
  min_date <- min(df %>% select(Date, SWC_icos) %>% drop_na() %>% pull(Date))
  max_date <- max(df %>% select(Date, SWC_icos) %>% drop_na() %>% pull(Date))
  
  plot_ts <- function(var) {
    if(var == "SM") {
      df_plot <- df %>% 
        select(Date, Model = SWC_mhm, ICOS = SWC_icos, SDI = SSMI_full) %>%
        distinct()
      title_A <- "Soil moisture"
      title_B <- "SSMI"
      unit <- "[vol. %]"
    } else if (var == "Prec") {
      df_plot <- df %>% 
        select(Date, Model = PREC, ICOS = P_F, SDI = SPI_365) %>%
        distinct()
      title_A <- "Precipitation"
      title_B <- "SPI"
      unit <- "[mm]"
    } else if (var == "Tavg") {
      df_plot <- df %>%
        select(Date, Model = TG, ICOS = TA_F, SDI = SPEI_365)
      title_A <- "Temperature"
      title_B <- "SPEI"
      unit <- "[°C]"
    }
    
    A <- ggplot(df_plot, aes(Date)) +
      geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.7) +
      geom_line(aes(y = Model, linetype = "modelled"), linewidth = 0.4) +
      geom_point(aes(y = ICOS, shape = "measured"), size = 0.3, colour = "#53A2BE") +
      theme_cowplot() +
      xlab(NULL) + ylab(unit) +
      scale_linetype_discrete(NULL) + scale_shape_discrete(NULL) +
      theme(legend.position = "none") + 
      scale_x_date(expand = c(0,0)) + 
      scale_y_continuous(expand = c(0,0)) +
      ggtitle(bquote(plain(.(title_A))))
    
    B <- df_plot %>%
      ggplot(aes(Date, SDI)) +
      geom_tile(width = 2, height = 5, mapping = aes(fill = SDI, y = 0)) +
      theme_cowplot() +
      scale_fill_gradientn(NULL, 
                           colors = brewer.pal(11, "RdBu"), 
                           limits = c(-2.5,2.5),
                           #label = signs_format(accuracy = 1, add_plusses = TRUE),
                           label = signs::signs_format(accuracy = 1),
                           na.value = NA) +
      xlab(NULL) + ylab(NULL) +
      theme_cowplot() +
      scale_x_date(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(legend.position = "none",
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) +
      ggtitle(bquote(plain(.(title_B))))
    
    plot_out <- plot_grid(A, B)
    return(plot_out)
  }
  
  plot_sm <- plot_ts("SM")
  plot_prec <- plot_ts("Prec")
  plot_tavg <- plot_ts("Tavg")
  
  save_plot("plots/map_part_swc.png", plot_sm, base_width = 9, base_height = 2, bg = "white")
  save_plot("plots/map_part_prec.png", plot_prec, base_width = 9, base_height = 2, bg = "white")
  save_plot("plots/map_part_tavg.png", plot_tavg, base_width = 9, base_height = 2, bg = "white")
  
  legend_plot <- df %>% 
    filter(year(Date) == 2020) %>%
    select(Date, Model = PREC, ICOS = P_F, SDI = SPI_365) %>%
    ggplot(aes(Date)) +
    geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = Inf, fill = "validation"),  
              alpha = 0.7) +
    geom_line(aes(y = Model, linetype = "mHM (model) or E-OBS (observations)"), linewidth = 0.4) +
    geom_point(aes(y = ICOS, shape = "ICOS"), size = 0.3, colour = "#53A2BE") +
    theme_cowplot() +
    scale_linetype_discrete(NULL) + 
    scale_shape_discrete(NULL) +
    scale_fill_manual(NULL, values = "gray") +
    theme(legend.position = "bottom") +
    guides(shape = guide_legend(override.aes = list(size = 3, alpha = 1)),
           linetype = guide_legend(override.aes = list(size = 2)))  
  legend <- get_legend(legend_plot)
  save_plot("plots/map_legend.png", legend, dpi = 500)
  
  legend2 <- get_legend(df_plot %>%
    ggplot(aes(Date, SSMI)) +
    geom_tile(width = 2, height = 5, mapping = aes(fill = SDI, y = 0)) +
    theme_cowplot() +
    scale_fill_gradientn(NULL, 
                         colors = brewer.pal(11, "RdBu"), 
                         limits = c(-2.5,2.5),
                         #label = signs_format(accuracy = 1, add_plusses = TRUE),
                         label = signs::signs_format(accuracy = 1),
                         na.value = NA) +
    xlab(NULL) + ylab(NULL) +
    theme_cowplot() +
    scale_x_date(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(legend.position = "top", 
          legend.key.width= unit(3, 'cm'),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()))
  save_plot("plots/map_legend2.png", legend2, dpi = 500, bg = "transparent")
  return(NULL)
}