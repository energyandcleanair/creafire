flaring.date_to_localpath <- function(date, ext='csv'){
  folder <- file.path(creahelpers::get_gis_dir(), 'fire', 'nvf')
  basename <- sprintf('nvf_%s.%s', strftime(as.Date(date),'%Y%m%d'),ext)
  return(file.path(folder, basename))
}

flaring.download_nvf_date <- function(date, force=F){

  library(httr)
  library(jsonlite)
  library(utils)
  library(R.utils)
  
  readRenviron(".Renviron")
  # Retrieve access token

  output_file <- flaring.date_to_localpath(date, ext='csv')
  output_file_gz <- flaring.date_to_localpath(date, ext='csv.gz')

  if(file.exists(output_file) & !force){
    return(TRUE)
  }

  params <- list(
    client_id = 'eogdata_oidc',
    client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
    username = Sys.getenv('NVF_MINES_EMAIL'),
    password = Sys.getenv('NVF_MINES_PASSWORD'),
    grant_type = 'password'
  )
  token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
  response <- POST(token_url, body = params, encode = "form")
  access_token_list <- fromJSON(content(response,as="text",encoding="UTF-8"))
  access_token <- access_token_list$access_token

  # Submit request with token bearer and write to output file
  data_url <- sprintf('https://eogdata.mines.edu/wwwdata/viirs_products/vnf/v30//VNF_npp_d%s_noaa_v30-ez.csv.gz', strftime(as.Date(date),'%Y%m%d'))
  auth <- paste('Bearer', access_token)

  download.file(data_url, output_file_gz, mode = "wb", headers = list(Authorization = auth))
  gunzip(output_file_gz, remove=T)
  return(file.exists(output_file))

}

flaring.download_nvf <- function(date_from, date_to){
  dates <- seq.Date(as.Date(date_from), as.Date(date_to), 'day')
  sapply(dates, flaring.download_nvf_date)
}

flaring.get_nvf <- function(date){
  nvf_file <- flaring.date_to_localpath(date)
  if(!file.exists(nvf_file)){
    flaring.download_nvf_date(date)
  }
  read_csv(nvf_file, col_types = cols())
}


flaring.get_flaring_amount <- function(date, geometries){

  tryCatch({
    geometries_sp <- creahelpers::to_spdf(geometries)
    flares_raw <- flaring.get_nvf(date=date)


    # Only keep relevant
    # https://www.mdpi.com/2072-4292/13/16/3078/htm
    # RH’=σT^4S^d
    sigma = 5.67E-8
    b1 = 0.0294
    d = 0.7

    flares <- flares_raw %>%
      filter(Temp_BB > 1200,
             Temp_BB < 999999) %>%
      mutate(
        rhp = sigma * Temp_BB^4 * Area_BB^d,
        bcm_est = b1 * rhp) %>%
      select(date=Date_LTZ, lon=Lon_GMTCO, lat=Lat_GMTCO, bcm_est)


    flares_sf <- sf::st_as_sf(flares, coords=c('lon', 'lat'))
    flares_sp <- as(flares_sf, "Spatial")

    sp::proj4string(flares_sp) <- sp::proj4string(geometries_sp)

    result <- cbind(
      as.data.frame(flares_sf) %>% select(-c(date, geometry)),
      sp::over(flares_sp, geometries_sp, returnList = F)) %>%
      filter(!is.na(id)) %>%
      group_by_at(setdiff(names(.), "bcm_est")) %>%
      summarise(
        bcm_est=sum(bcm_est),
        count=n()
      ) %>%
      ungroup() %>%
      mutate(date=!!date)
    return(result)
  }, error=function(e){
    warning(sprintf("Failed for date %s", date))
    return(NULL)
  })
}



flaring.get_flaring_amounts <- function(dates, geometries){
 lapply(date, function(date){flaring.get_flaring_amount(date=date, geometries=geometries)}) %>%
    do.call(bind_rows, .)
}
  
