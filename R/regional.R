
regional.collect_fires <- function(iso2, date_from, date_to){
  
  g1 <- creahelpers::get_adm(level=1, iso2=iso2)
  g2 <- creahelpers::get_adm(level=2, iso2=iso2)
  
  g <- bind_rows(sf::st_as_sf(g1) %>% mutate(level=1),
                 sf::st_as_sf(g2) %>% mutate(level=2))
  
  readRenviron(".Renviron")
  
  fires <- creatrajs::fire.aggregate(date_from, date_to, geometries = g)
  
  fires %>%
    select_at(grep("date|GID_.*|^NAME_.*",names(.))) %>%
    mutate(iso2=countrycode::countrycode(GID_0,"iso3c","iso2c"))
}


regional.update_year <- function(iso2s, year){
  
  folder <- "cache"
  dir.create(folder, F, T)
  
  for(iso2 in iso2s){
    
    date_from <- paste0(year,"-01-01")
    date_to <- min(lubridate::today(), paste0(year,"-12-31"))
    
    f <- regional.download(iso2, year, folder)  
    if(file.exists(f)){
      fires <- readRDS(f)
      date_from <- max(as.Date(fires$acq_date)) - lubridate::days(2) # Take a buffer
    }else{
      fires <- NULL
    }
    
    
  }
  
  
}


#' Download regional fire data for a given iso2 and given year
#'
#' @param iso2 
#' @param year 
#' @param folder 
#'
#' @return file path of downloaded file (note: it may not exist)
#' @export
#'
#' @examples
regional.download <- function(iso2, year, folder){
  file_name <- sprintf("%s_%s.RDS", tolower(iso2), year)
  file_path <- file.path(folder, file_name)
  gcs.download_regional_fire(fs=file_name, dest_folder=folder, only_if_modified_since=T, overwrite=T)
  return(file_path)
}