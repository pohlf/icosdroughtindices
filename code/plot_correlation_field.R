plot_correlation_field <- function() {
  sites <- c("DE-Gri",
             "DE-Hai",
             "DE-Kli",
             "BE-Dor",
             "FR-Hes",
             "IT-BCi")
  
  df_icos <- read_csv("data/ICOS/WW20_dd.csv")%>% 
    filter(site %in% sites) %>%
    select(Date, site, GPP = GPP_NT_VUT_MEAN) %>%
    filter(!(month(Date) == 2 & day(Date) == 29)) %>%
    arrange(site, Date) %>%
    mutate(Doy = rep(seq(1,365), nrow(.)/365))
  
  df_z <- df_icos %>%
    group_by(site, Doy) %>%
    summarise(mean_gpp = mean(GPP),
              sd_gpp = sd(GPP)) %>%
    ungroup() %>%
    right_join(df_icos) %>%
    mutate(zGPP = (GPP - mean_gpp) / sd_gpp)
  
  
  # station SPEI files
  path <- "data/SDI/SPEI/"
  site_files <- paste0("SPEI_", sites,".csv")
  
  df_spei <- lapply(site_files, function(x) read_csv(paste0(path,x)) %>% 
                      mutate(site = str_remove(x, "SPEI_"),
                             site = str_remove(site, ".csv"))) %>%
    bind_rows()
  
  df_z <- df_z %>%
    left_join(df_spei)
  
  df_plot <- df_z %>%
    select(site, Doy, zGPP, starts_with("SPEI")) %>%
    pivot_longer(-c(site,Doy,zGPP)) %>%
    group_by(site, Doy, name) %>%
    summarise(cor = cor(zGPP, value))
  
  p <- df_plot %>%
    mutate(site = case_when(
      site == "DE-Gri" ~ "DE-Gri (GRA) 2004 - 2020",
      site == "DE-Hai" ~ "DE-Hai (DBF) 2000 - 2020",
      site == "DE-Kli" ~ "DE-Kli (CRO) 2004 - 2020",
      site == "BE-Dor" ~ "BE-Dor (GRA) 2011 - 2020",
      site == "FR-Hes" ~ "FR-Hes (DBF) 2014 - 2020",
      site == "IT-BCi" ~ "IT-BCi (CRO) 2004 - 2020"
    )) %>%
    mutate(site = factor(site, levels = c("DE-Gri (GRA) 2004 - 2020", 
                                          "DE-Hai (DBF) 2000 - 2020",
                                          "DE-Kli (CRO) 2004 - 2020",
                                          "BE-Dor (GRA) 2011 - 2020", 
                                          "FR-Hes (DBF) 2014 - 2020", 
                                          "IT-BCi (CRO) 2004 - 2020"))) %>%
    mutate(Date = as.Date(Doy, origin = "1999-12-31")) %>%
    filter(month(Date) %in% c(4,5,6,7,8,9,10)) %>%
    mutate(agg = parse_number(name)) %>%
    filter(agg < 370) %>%
    ggplot(aes(Date, agg, fill = cor)) +
    geom_tile() +
    theme_cowplot() +
    scale_fill_distiller("correlation", 
                         palette = "RdBu", 
                         limits = c(-1,1),
                         label = signs::signs_format(accuracy = 0.1)) +
    scale_x_date(expand = c(0,0), 
                 date_labels = "%b", 
                 breaks = "2 months",
                 guide = "axis_minor"
    ) +
    scale_y_continuous(expand = c(0,0), breaks = pretty_breaks()) +
    xlab("zGPP") + ylab("SPEI_x [days]") +
    facet_wrap(~site, scales = "free_x") +
    theme(legend.title = element_text(angle = 90, vjust = 1),
          strip.background=element_rect(fill="white"),
          strip.text.x = element_text(size = 11, hjust = 0, face = "bold")
    )

  save_plot("plots/correlation_gpp_spei.png", p, base_width = 10, bg = "white")
}