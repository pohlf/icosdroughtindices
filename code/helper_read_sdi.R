read_SDI <- function(SDI, agg) {
  path <- paste0("data/SDI/",SDI,"/")
  files <- list.files(path, pattern = ".csv")
  
  helper <- function(file){
    x <- read_csv(paste0(path,file)) %>% 
      mutate(site = str_remove(file, "SPI_|SPEI_|SSMI_|.csv")) %>% 
      select(Date, site, paste0(SDI,"_",agg))
  }
  
  plan(multisession)
  list_spei <- future_lapply(files, helper)
  
  df <- list_spei %>% bind_rows()
  return(df)
}
