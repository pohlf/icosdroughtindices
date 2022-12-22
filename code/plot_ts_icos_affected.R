plot_ts_icos_affected <- function() {
  df_spei <- read_SDI("SPEI", agg = 365)
  df_spi <- read_SDI("SPI", agg = 365)
  df_smi <- read_SDI("SMI", agg = "full")
  
  df <- 
    left_join(df_spei, df_spi) %>%
    left_join(df_smi) %>%
    pivot_longer(-c(Date, site), names_to = "index") %>%
    drop_na() %>%
    mutate(drought_mckee = case_when(
      value <= -1 & value >-1.5 ~ "moderate",
      value <= -1.5 & value >-2. ~ "severe",
      value <= -2. ~ "extreme",
      TRUE ~ NA_character_
    )) %>%
    mutate(drought_agnew = case_when(
      value <= -0.84 & value >-1.28 ~ "moderate",
      value <= -1.28 & value >-1.65 ~ "severe",
      value <= -1.65 ~ "extreme",
      TRUE ~ NA_character_
    )) %>%
    mutate(country = str_extract(site, "^.{2}"))
  
  
  ### how many icos sites affected
  nn <- length(unique(df$site))
  palette <- c("#481D24","#F03B20","#FFC857")
  
  df_plot <- df %>%
    drop_na() %>%
    pivot_longer(-c(Date,site,index,value,country), 
                 names_to = "category_source", 
                 values_to = "drought_category") %>%
    mutate(drought_category = factor(drought_category, levels = c("extreme", "severe", "moderate"))) %>%
    group_by(Date, index, category_source, drought_category) %>%
    #group_by(Date, index, drought_mckee) %>%
    summarise(n = n() / nn) 
  
  plot_function <- function(df_plot) {
    
  }
  
  p <- df_plot %>% 
    filter(year(Date) > 2010) %>%
    ggplot(aes(Date, y = n)) +
    theme_cowplot() +
    background_grid(major = "y") +
    geom_col(mapping = aes(fill = drought_category), colour = NA, width = 2, alpha = 1) +
    scale_x_date(expand = c(0,0), breaks = pretty_breaks(n = 7)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1), labels = percent_format(), breaks = c(0.25,0.5,0.75,1)) +
    xlab(NULL) + ylab("number of ICOS sites affected") +
    #geom_smooth(mapping = aes(colour = "local trend"), se = F) +
    #facet_wrap(~category_source, ncol = 1) +
    scale_fill_manual("drought", values = palette) +
    facet_wrap(~index, ncol = 1, strip.position = "right") +
    facet_grid(vars(index), vars(category_source))
  
  save_plot("plots/icos_sites_affected.eps", p, 
            bg = "white", 
            base_width = 10)
  return(NULL)
}

