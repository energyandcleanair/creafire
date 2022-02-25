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
  c("location_id", "duration_hour", "height", "met_type", "buffer_km", "fire_source", "fire_split")
}

db.get_unique_columns_meas <- function(){
  c("location_id", "duration_hour", "height", "met_type", "buffer_km", "fire_source", "fire_split")
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
                            location_id, met_type, height, duration_hour, buffer_km, fire_source, fire_split=NULL){
  fs <- db.get_gridfs_weather()
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "weather.RDS")
  saveRDS(weather, filepath)

  metadata <- list(location_id=location_id,
                   duration_hour=duration_hour,
                   height=height,
                   met_type=met_type,
                   buffer_km=buffer_km,
                   fire_source=fire_source,
                   fire_split=fire_split)

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


db.find_weather <- function(location_id, met_type=NULL, height=NULL, duration_hour=NULL, buffer_km=NULL, fire_source=NULL, fire_split=NULL){
  fs <- db.get_gridfs_weather()

  filter <- list(metadata.location_id=location_id,
                   metadata.duration_hour=duration_hour,
                   metadata.height=height,
                   metadata.met_type=met_type,
                   metadata.buffer_km=buffer_km,
                   metadata.fire_source=fire_source,
                   metadata.fire_split=fire_split)

  filter <- filter[!unlist(lapply(filter, is.null))]
  fs$find(jsonlite::toJSON(filter,auto_unbox=T))
}


db.remove_weather <- function(location_id, met_type=NULL, height=NULL, duration_hour=NULL, buffer_km=NULL, fire_source=NULL, fire_split=NULL){
  fs <- db.get_gridfs_weather()
  found <- db.find_weather(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour,
                           fire_source=fire_source, buffer_km=buffer_km, fire_split=fire_split)

  if(nrow(found)>0) fs$remove(paste0("id:", found$id))
  print(sprintf("%d row(s) removed", nrow(found)))
}


db.download_weather <- function(location_id=NULL, met_type=NULL, height=NULL, duration_hour=NULL, buffer_km=NULL, fire_source=NULL, fire_split=NULL){
  fs <- db.get_gridfs_weather()
  found <- db.find_weather(location_id=location_id, met_type=met_type, height=height, duration_hour=duration_hour,
                         fire_source=fire_source, buffer_km=buffer_km, fire_split=fire_split)

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



#' Uplaod weather cached using previous system (i.e. on disk)
#'
#' @return
#' @export
#'
#' @examples
db.upload_filecached <- function(){
  folder <- "../../../studies/202202_biomass_burning_article/validation/"
  paths <- list.files(folder, full.names = T)
  names <- list.files(folder)

  files <- tibble(name=basename(names), path=paths) %>%
    filter(stringr::str_detect(name, "\\.weather\\.")) %>%
    tidyr::separate(name, c("location_id", "details"), sep="\\.weather\\.") %>%
    tidyr::separate(details, c("buffer_km","duration_hour","height", "fire_source", "extension"), sep="\\.")

  files$duration_hour <- as.numeric(gsub("h","",files$duration_hour))
  files$height <- as.numeric(gsub("m","",files$height))
  files$buffer_km <- as.numeric(gsub("km","",files$buffer_km))
  files$size <- file.info(files$path)$size
  files$fire_split <- NULL
  files <- files %>% filter(size > 300)
  files$met_type <- "gdas1"
  
  for(i in seq(nrow(files))){
    print(sprintf("%d/%d",i,nrow(files)))
    f <- files[i,]
    db.upload_weather(weather=readRDS(f$path),
                    location_id=f$location_id,
                    height=f$height,
                    met_type=f$met_type,
                    duration_hour=f$duration_hour,
                    fire_source=f$fire_source,
                    buffer_km=f$buffer_km,
                    fire_split = NULL
                    )
  }
}

