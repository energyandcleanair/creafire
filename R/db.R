db.get_collection <- function(collection_name){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::mongo(collection=collection_name, db="creafire", url=connection_string)
}

db.get_gridfs_weather <- function(){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::gridfs(db="creafire", prefix="weather", url=connection_string)
}

db.get_gridfs_meas <- function(){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::gridfs(db="creafire", prefix="meas", url=connection_string)
}


db.get_unique_columns_weather <- function(){
  c("location_id", "duration_hour", "hours", "height", "met_type", "buffer_km", "fire_source", "fire_split_regions")
}


db.get_unique_columns_meas <- function(){
  c("location_id", "duration_hour", "hours", "height", "met_type", "buffer_km", "fire_source", "fire_split_regions")
}

db.available_metadata <- function(fs, col_names){
  found <- fs$find()
  if(nrow(found)==0){
    # Return empty tibble
    tbl <- as_tibble(data.frame(matrix(nrow=0,ncol=length(col_names))))
    names(tbl) <- col_names
    return(tbl)
  }
  
  lapply(found$metadata, function(x){
    l <- jsonlite::fromJSON(x)
    # Replace empty list with NA
    l <- lapply(l, function(x){if(length(x)==0){NA}else{x}})
    as.data.frame(l)
  }) %>%
    do.call(bind_rows, .)
}

db.available_weather <- function(){
  db.available_metadata(fs=db.get_gridfs_weather(),
                        col_names=db.get_unique_columns_weather())
}


db.available_meas <- function(){
  db.available_metadata(fs=db.get_gridfs_meas(),
                        col_names=db.get_unique_columns_meas())
}


db.create_index <- function(collection_name, columns, index_name, unique=T){
  cmd <- sprintf('{"createIndexes":"%s",
        "indexes":[{"key":{%s},
        "name":"%s","unique": %s}]}',
                 collection_name,
                 paste(sprintf("\"%s\":1",columns), collapse=","),
                 index_name,
                 ifelse(unique, "true","false"))

  m <- db.get_collection(collection_name)
  m$run(cmd)
}


db.setup_db <- function(){
  db.create_index(collection_name="weather.files",
                     columns=paste0("metadata.", db.get_unique_columns_weather()),
                     index_name="weather_unique_index",
                     unique=T)
  
  db.create_index(collection_name="meas.files",
                  columns=paste0("metadata.", db.get_unique_columns_meas()),
                  index_name="meas_unique_index",
                  unique=T)
}


db.upload_weather <- function(weather,
                              location_id,
                              location_name=NULL,
                              weather_sources=NULL,
                              met_type,
                              height,
                              duration_hour,
                              buffer_km,
                              hours,
                              fire_source,
                              fire_split_regions=NO_SPLIT_REGION){
  
  fs <- db.get_gridfs_weather()
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "weather.RDS")
  saveRDS(weather, filepath)

  format_vector <- function(x){
    if(all(is.null(x)) || all(is.na(x))) NULL else {paste0(x, collapse=',')}
  }
  hours <- format_vector(hours)
  weather_sources <- format_vector(weather_sources)
  height <- if(is.null(height) || is.na(height)) NULL else {height}
  location_name <- if(is.null(location_name)){
    rcrea::cities(id=location_id) %>% pull(name) %>% head(1)
  }else{location_name}
  
  metadata <- list(location_id=location_id,
                   location_name=location_name,
                   duration_hour=duration_hour,
                   weather_sources=weather_sources,
                   hours=hours,
                   height=height,
                   met_type=met_type,
                   buffer_km=buffer_km,
                   fire_source=fire_source,
                   fire_split_regions=fire_split_regions)

  # Remove first if exists
  filter <- metadata[db.get_unique_columns_weather()]
  names(filter) <- paste0("metadata.", names(filter))
  found <- fs$find(jsonlite::toJSON(filter,auto_unbox=T))
  if(nrow(found)>0){
    print("Weather already exist. Replacing it")
    fs$remove(paste0("id:", found$id))
  }

  # And then upload
  fs$upload(filepath, name=basename(filepath), content_type=NULL,
            metadata=jsonlite::toJSON(metadata, auto_unbox=T))
}


db.upload_meas <- function(meas, location_id, met_type, height, duration_hour, hours, buffer_km, fire_source, fire_split_regions=NO_SPLIT_REGION){
  fs <- db.get_gridfs_meas()
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "meas.RDS")
  saveRDS(meas, filepath)

  hours <- if(all(is.null(hours)) || all(is.na(hours))) NULL else {paste0(hours, collapse=',')}
  height <- if(is.null(height) || is.na(height)) NULL else {height}
  
  metadata <- list(location_id=location_id,
                   duration_hour=duration_hour,
                   hours=hours,
                   height=height,
                   met_type=met_type,
                   buffer_km=buffer_km,
                   fire_source=fire_source,
                   fire_split_regions=fire_split_regions)
  
  # Remove first if exists
  filter <- metadata[db.get_unique_columns_meas()]
  names(filter) <- paste0("metadata.", names(filter))
  found <- fs$find(jsonlite::toJSON(filter,auto_unbox=T))
  if(nrow(found)>0){
    print("Meas already exist. Replacing it")
    fs$remove(paste0("id:", found$id))
  }
  
  # And then upload
  fs$upload(filepath, name=basename(filepath), content_type=NULL,
            metadata=jsonlite::toJSON(metadata, auto_unbox=T))
}


db.find_weather <- function(location_id,
                            met_type=NULL,
                            height=NULL,
                            duration_hour=NULL,
                            hours=NULL,
                            buffer_km=NULL,
                            fire_source=NULL,
                            fire_split_regions=NULL,
                            weather_sources=NULL){
  fs <- db.get_gridfs_weather()

  format_vector <- function(x){
    if(all(is.null(x)) || all(is.na(x))) NULL else {paste0(x, collapse=',')}
  }
  hours <- format_vector(hours)
  weather_sources <- format_vector(weather_sources)
  height <- if(is.null(height) || is.na(height)) NULL else {height}
  fire_split_regions <- if(is.null(fire_split_regions) || is.na(fire_split_regions)) NO_SPLIT_REGION else {fire_split_regions}
  
  filter <- list(metadata.location_id=location_id,
                 metadata.duration_hour=duration_hour,
                 metadata.hours=hours,
                 metadata.weather_sources=weather_sources,
                 metadata.height=height,
                 metadata.met_type=met_type,
                 metadata.buffer_km=buffer_km,
                 metadata.fire_source=fire_source,
                 metadata.fire_split_regions=fire_split_regions)

  filter <- filter[!unlist(lapply(filter, is.null))]
  fs$find(jsonlite::toJSON(filter,auto_unbox=T))
}


db.find_meas <- function(location_id,
                         met_type=NULL,
                         height=NULL,
                         duration_hour=NULL,
                         hours=NULL,
                         buffer_km=NULL,
                         fire_source=NULL,
                         fire_split_regions=NULL){
  fs <- db.get_gridfs_meas()
  
  hours <- if(all(is.null(hours)) || all(is.na(hours))) NULL else {paste0(hours, collapse=',')}
  height <- if(is.null(height) || is.na(height)) NULL else {height}
  
  filter <- list(metadata.location_id=location_id,
                 metadata.duration_hour=duration_hour,
                 metadata.hours=hours,
                 metadata.height=height,
                 metadata.met_type=met_type,
                 metadata.buffer_km=buffer_km,
                 metadata.fire_source=fire_source,
                 metadata.fire_split_regions=fire_split_regions)
  
  filter <- filter[!unlist(lapply(filter, is.null))]
  fs$find(jsonlite::toJSON(filter,auto_unbox=T))
}

db.remove_meas <- function(location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_meas()
  found <- db.find_meas(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour, hours=hours,
                           fire_source=fire_source, buffer_km=buffer_km, fire_split_regions=fire_split_regions)
  
  if(nrow(found)>0) fs$remove(paste0("id:", found$id))
  print(sprintf("%d row(s) removed", nrow(found)))
}


db.remove_weather <- function(location_id, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_weather()
  found <- db.find_weather(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour, hours=hours,
                           fire_source=fire_source, buffer_km=buffer_km, fire_split_regions=fire_split_regions)

  if(nrow(found)>0) fs$remove(paste0("id:", found$id))
  print(sprintf("%d row(s) removed", nrow(found)))
}


#' Download all weather, apply a function, and reupload
#'
#' @param f 
#' @param location_id 
#' @param met_type 
#' @param height 
#' @param duration_hour 
#' @param hours 
#' @param buffer_km 
#' @param fire_source 
#' @param fire_split_regions 
#'
#' @return
#' @export
#'
#' @examples
db.apply_function_to_weather <- function(f=db.clean_weather_function(), location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  
  weathers <- db.download_weather(location_id=location_id,
                                  met_type=met_type,
                                  height=height,
                                  duration_hour=duration_hour,
                                  hours=hours,
                                  buffer_km=buffer_km,
                                  fire_source=fire_source,
                                  fire_split_regions=fire_split_regions)
  
  if(nrow(found)==0) return(NULL)
  
  weathers <- weathers %>% mutate(weather = map(weather, f))
  
  # Reupload
  for(i in seq(nrow(weathers))){
    print(sprintf("%d/%d",i,nrow(weathers)))
    w <- weathers[i,]
    db.upload_weather(weather=w$weather,
                      location_id=w$location_id,
                      location_name=w$location_name,
                      weather_sources=w$weather_sources,
                      met_type=w$met_type,
                      height=w$height,
                      duration_hour=w$duration_hour,
                      buffer_km=w$buffer_km,
                      hours=w$hours,
                      fire_source=w$fire_source,
                      fire_split_regions = w$fire_split_regions
    )
  }
}

db.clean_weather_function <- function(){
  f <- function(x){x %>% filter(!is.na(air_temp_min), !is.infinite(air_temp_min))}
  return(f)
}

db.download_weather <- function(location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_weather()
  found <- db.find_weather(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour, hours=hours,
                         fire_source=fire_source, buffer_km=buffer_km, fire_split_regions=fire_split_regions)

  if(nrow(found)==0) return(NULL)

  result <- lapply(found$metadata, function(x){
    l <- jsonlite::fromJSON(x)
    # Replace empty list with NA
    l <- lapply(l, function(x){if(length(x)==0){NA}else{x}})
    as.data.frame(l)
    }) %>%
    do.call(bind_rows, .)

  ids <- paste0("id:",found$id)
  weather <- lapply(ids, function(id){
    filepath <- tempfile()
    fs$download(id, filepath)
    weather <- readRDS(filepath)
    file.remove(filepath)
    return(weather)
  })

  result$weather <- weather
  tibble(result)
}


db.download_meas <- function(location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, hours=NULL, buffer_km=NULL, fire_source=NULL, fire_split_regions=NULL){
  fs <- db.get_gridfs_meas()
  found <- db.find_meas(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour, hours=hours,
                           fire_source=fire_source, buffer_km=buffer_km, fire_split_regions=fire_split_regions)
  
  if(nrow(found)==0) return(NULL)
  
  result <- lapply(found$metadata, function(x){
    l <- jsonlite::fromJSON(x)
    # Replace empty list with NA
    l <- lapply(l, function(x){if(length(x)==0){NA}else{x}})
    as.data.frame(l)
  }) %>%
    do.call(bind_rows, .)
  
  ids <- paste0("id:",found$id)
  meas <- lapply(ids, function(id){
    filepath <- tempfile()
    fs$download(id, filepath)
    meas <- readRDS(filepath)
    file.remove(filepath)
    return(meas)
  })
  
  result$meas <- meas
  tibble(result)
}


db.add_location_name <- function(){
  
  fs <- db.get_gridfs_weather()
  col <- db.get_collection('weather.files')
  found <- fs$find('{}')
  
  location_ids <- lapply(found$metadata, function(x) jsonlite::fromJSON(x)$location_id) %>%
    unlist() %>%
    unique()
  
  lapply(location_ids, function(location_id){
    location_name <- rcrea::cities(id=location_id)$name
    col$update(
      query = sprintf('{"metadata.location_id": "%s"}',location_id),
      update = sprintf('{ "$set" : { "metadata.location_name" : "%s"} }', location_name),
      multiple=T
    )  
  })
}


db.add_split_regions <- function(){
  
  fs <- db.get_gridfs_weather()
  col <- db.get_collection('weather.files')
  col$update(
    query = sprintf('{"metadata.fire_split_regions": {}}'),
    update = sprintf('{ "$set" : { "metadata.fire_split_regions" : "%s"} }', NO_SPLIT_REGION),
    multiple=T
  )  
  
  fs <- db.get_gridfs_meas()
  col <- db.get_collection('meas.files')
  
  col$update(
    query = sprintf('{"metadata.fire_split_regions": {}}'),
    update = sprintf('{ "$set" : { "metadata.fire_split_regions" : "%s"} }', NO_SPLIT_REGION),
    multiple=T
  )
  
}

db.add_weather_sources <- function(){
  
  fs <- db.get_gridfs_weather()
  col <- db.get_collection('weather.files')
  col$update(
    # query those without metadata.weather_sources
    query = sprintf('{"metadata.weather_sources": {}}'),
    update = sprintf('{ "$set" : { "metadata.fire_split_regions" : "%s"} }', "noaa"),
    multiple=T
  )
}


db.clean <- function(){
  fs <- db.get_gridfs_weather()
  
  found <- fs$find('{}')
  empty <- found[found$size==0,]
  if(nrow(empty)>0) fs$remove(paste0("id:", empty$id))
  
  fs <- db.get_gridfs_meas()
  
  # Remove those that don't have process_id
  found <- fs$find('{}')
  empty <- found[found$size==0,]
  if(nrow(empty)>0) fs$remove(paste0("id:", empty$id))
  
  # old <- found[found$date < "2023-10-01",]
  # fs$remove(paste0("id:", old$id))
}


#' Upload weather and meas cached using previous system (i.e. on disk)
#'
#' @return
#' @export
#'
#' @examples
db.upload_filecached <- function(){
  library(tidyverse)
  folder <- "../../../studies/202202_biomass_burning_article/validation/"
  paths <- list.files(folder, full.names = T)
  names <- list.files(folder)

  files.weather <- tibble(name=basename(names), path=paths) %>%
    filter(stringr::str_detect(name, "\\.weather\\.")) %>%
    tidyr::separate(name, c("location_id", "details"), sep="\\.weather\\.") %>%
    tidyr::separate(details, c("buffer_km","duration_hour","height", "fire_source", "extension"), sep="\\.")
  
  files.meas <- tibble(name=basename(names), path=paths) %>%
    filter(stringr::str_detect(name, "\\.meas\\.")) %>%
    tidyr::separate(name, c("location_id", "details"), sep="\\.meas\\.") %>%
    tidyr::separate(details, c("buffer_km","duration_hour","height", "fire_source", "extension"), sep="\\.")

  files.trajs <- tibble(name=basename(names), path=paths) %>%
    filter(stringr::str_detect(name, "\\.trajs\\.")) %>%
    tidyr::separate(name, c("location_id", "details"), sep="\\.trajs\\.") %>%
    tidyr::separate(details, c("buffer_km","duration_hour","height", "extension"), sep="\\.")
  
  clean <- function(files){
    files$duration_hour <- as.numeric(gsub("h","",files$duration_hour))
    files$height <- as.numeric(gsub("m","",files$height))
    files$buffer_km <- as.numeric(gsub("km","",files$buffer_km))
    files$size <- file.info(files$path)$size
    files$fire_split_regions <- NULL
    files <- files %>% filter(size > 300)
    files$met_type <- "gdas1"
    return(files)
  }
  
  files.weather <- files.weather %>% clean()
  files.meas <- files.meas %>% clean()
  files.trajs <- files.trajs %>% clean()
  
  
  for(i in seq(nrow(files.weather))){
    print(sprintf("%d/%d",i,nrow(files.weather)))
    f <- files.weather[i,]
    db.upload_weather(weather=readRDS(f$path),
                    location_id=f$location_id,
                    height=f$height,
                    met_type=f$met_type,
                    duration_hour=f$duration_hour,
                    fire_source=f$fire_source,
                    buffer_km=f$buffer_km,
                    fire_split_regions = NULL
                    )
  }
  
  for(i in seq(nrow(files.meas))){
    print(sprintf("%d/%d",i,nrow(files.meas)))
    f <- files.meas[i,]
    db.upload_meas(meas=readRDS(f$path),
                      location_id=f$location_id,
                      height=f$height,
                      met_type=f$met_type,
                      duration_hour=f$duration_hour,
                      fire_source=f$fire_source,
                      buffer_km=f$buffer_km,
                   fire_split_regions = NULL
    )
  }
}

