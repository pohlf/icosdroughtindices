plot_schematic_sdi <- function() {
  # normal distribution simulation
  sim_dat = data.frame(x = seq(-3, 3, length.out = 1001))
  sim_dat$y_density = dnorm(sim_dat$x, mean = 0, sd = 1)
  sim_dat$y_cumulative = pnorm(sim_dat$x, mean = 0, sd = 1)
  labels <- c("extreme dry", "severe dry","moderate dry", "mild / normal", "moderate wet", "severe wet", "extreme wet")
  sim_dat$mckee = cut(sim_dat$x, breaks = c(-Inf, -2, -1.5, -1, 1, 1.5, 2, Inf), labels = labels)
  sim_dat$agnew = cut(sim_dat$x, breaks = c(-Inf, -1.65, -1.28, -0.84, 0.84, 1.28, 1.65, Inf), labels = labels)
  
  # hyytiälä data
  df_hyy <- read_csv("data/EOBS/csv_output/FI-Hyy_input.csv")
  
  x_hyy <- df_hyy %>% 
    dplyr::filter(month(Date) == 7) %>%
    mutate(BAL = PET - PREC) %>%
    group_by(Year = year(Date)) %>%
    summarise(BAL = sum(BAL))
  
  xBAL <-x_hyy %>% pull(BAL)
  
  # calculate bandwidth
  f_kde <- kde(xBAL)
  # get probabilities
  x_hyy$prob <- pkde(xBAL, f_kde)
  
  x_hyy$prob_norm <- qnorm(x_hyy$prob)
  x_hyy$mckee = cut(x_hyy$prob_norm, breaks = c(-Inf, -2, -1.5, -1, 1, 1.5, 2, Inf), labels = labels)
  x_hyy$agnew = cut(x_hyy$prob_norm, breaks = c(-Inf, -1.65, -1.28, -0.84, 0.84, 1.28, 1.65, Inf), labels = labels)
  
  
  
  A <- 
    ggplot(x_hyy, aes(x = BAL)) +
    stat_ecdf(geom = "point", size = 0.6) +
    stat_ecdf(aes(colour = "ECDF"), geom = "line") +
    geom_line(aes(y = prob, colour = "KDE")) +
    theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0)) +
    ylab(bquote(atop("empirical", "cumulative probability"))) +
    xlab("BAL_30") +
    scale_color_manual(NULL, values = c("black", "red")) +
    geom_segment(aes(x = 0.780, y = 0, xend = 0.780, yend = 0.213)) +
      geom_segment(aes(x = 0.780, y = 0.213, xend = -90, yend = 0.213),
                   arrow = arrow(length = unit(4, "mm")))
  
  x_hyy %>%
    dplyr::filter(BAL >= -10) %>%
    arrange(BAL)
  
  x_hyy[x_hyy$Year == 2004,]
  
  B <- ggplot(sim_dat, aes(x,y_cumulative)) + 
    stat_ecdf(data = x_hyy, aes(x = prob_norm, y = NULL, colour = "ECDF")) +
    geom_line(aes(colour = "N(0,1)")) +
    theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    ylab(bquote(atop("normalized", "cumulative probability"))) + xlab("SPEI_30") +
    coord_cartesian(clip = "off", xlim = c(-3, 3)) +
    theme(plot.margin = margin(5.5, 120, 5.5, 20, "pt"),
          legend.position = c(1.2,0.5)) +
    #geom_area(aes(x = 2.5, y = y_cumulative, fill = mckee)) +
    geom_tile(aes(x = 3.5, y = y_cumulative, fill = agnew), height = 0.01, width = 0.4, show.legend = F) +
    geom_tile(aes(x = 4, y = y_cumulative, fill = mckee), height = 0.01, width = 0.4, show.legend = F)  +
    scale_fill_brewer(NULL, palette = "RdBu") +
    scale_color_manual(NULL, values = c("black", "red")) +
    geom_segment(aes(x = -3.3, y = 0.213, xend = -0.796, yend = 0.213), linetype = "dashed") +
    geom_segment(aes(x = -0.796, y = 0.213, xend = -0.796, yend = 0),
                 arrow = arrow(length = unit(4, "mm"))) 
  
  C <- ggplot() +
    geom_histogram(aes(x = x_hyy$BAL, y = after_stat(density)), bins = 50) +
    geom_line(aes(f_kde$eval.points, f_kde$estimate), colour = "red") +
    theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    xlab("BAL_30") + ylab("empirical density")
  
  D <- ggplot(sim_dat, aes(x, y_density)) + 
    geom_area(fill = "white", colour = "black") +
    theme_cowplot() +
    geom_tile(aes(x = x, y = -0.1, fill = mckee), height = 0.03) +
    geom_tile(aes(x = x, y = -0.15, fill = agnew), height = 0.03) +
    scale_fill_brewer(NULL, palette = "RdBu")  +
    coord_cartesian(clip = "off", ylim = c(0, NA), xlim = c(-3,3)) +
    theme(plot.margin = margin(5.5, 20, 50, 5.5, "pt")) +
    ylab("normalized density") + xlab("SPEI_30") +
    scale_y_continuous(expand = c(0,0))+
    scale_x_continuous(breaks = scales::pretty_breaks())
  
  #save_plot("plots/sdi_concept.png", p)
  #plot_out <- plot_grid(A,B, pp, p, nrow = 1, align = "h", axis = "b", rel_widths = c(0.2,0.2,0.2,0.4))
  #save_plot("plots/sdi_concept.png", plot_out, bg = "white", base_width = 14)
  
  legend <- get_legend(D)
  plots <- plot_grid(A, B, C, D + theme(legend.position = "none"), 
                     ncol = 2, 
                     align = "hv", 
                     axis = "tblr",
                     labels = "auto")
  plot_out <- plot_grid(plots, legend, nrow = 1, rel_widths = c(0.8,0.2))
  plot_out
  plots
  save_plot("plots/sdi_concept.png", plots, base_width = 12, base_height = 7)
  return(NULL)
}

