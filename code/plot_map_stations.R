plot_map_stations <- function() {
  icos_meta <- read_csv("data/ICOS/meta_full.csv")
  
  # get countries
  map_data <- ne_countries(
    scale = "medium",
    type = "map_units",
    
    returnclass = "sf"
  )
  
  # to spatial points
  icos_sf <- st_as_sf(icos_meta, coords = c("lon", "lat"), crs = 4326)
  
  # display window
  extend_fct <- 1
  disp_win_wgs84 <- st_sfc(st_point(c(min(icos_meta$lon)-extend_fct, min(icos_meta$lat)-extend_fct)),
                           st_point(c(max(icos_meta$lon)+extend_fct, max(icos_meta$lat)+extend_fct)),
                           crs = 4326
  )
  
  # map
  p <- ggplot(map_data) +
    geom_sf() +
    geom_sf(data = icos_sf, aes(color = ICOS, shape = WW20)) +
    theme_minimal_grid() +
    coord_sf(
      xlim = st_coordinates(disp_win_wgs84)[, "X"],
      ylim = st_coordinates(disp_win_wgs84)[, "Y"]
    ) +
    scale_shape_manual(NULL, 
                       values = c(1,19),
                       labels = c("no validation","")) +
    scale_color_manual(NULL, 
                       labels = c("FLUXNET", "ICOS"), 
                       values = c("#0A2239", "#53A2BE")) +
    theme(legend.position = "bottom") +
    guides(shape = guide_legend(override.aes = list(shape = c(1, NA) ) ) )
  save_plot("plots/map.png", p, base_height = 5)
  return(NULL)
}