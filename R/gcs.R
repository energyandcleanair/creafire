gcs.auth <- function(force_service_account=F){

  # To avoid interactive prompt
  # which blocks execution on ComputeEngine and AppEngine
  options(httr_oauth_cache=T)

  if(force_service_account){
    googleAuthR::gar_deauth()
  }

  if(!googleAuthR::gar_has_token()){
    suppressWarnings(readRenviron(".env"))
    suppressWarnings(readRenviron(".Renviron"))

    # First try to see if we're on a Compute Engine instance
    googleAuthR::gar_gce_auth()

    if (!googleAuthR::gar_has_token()){
      # Use USER specific credentials if set
      if(Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")!="" & !force_service_account){
        message("Using local user rights for GCS: ", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
        googleCloudStorageR::gcs_auth(json_file=Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
      }
    }

    # Used by Shiny Application
    if(Sys.getenv('GCS_AUTH_FILE')!=""){
      googleCloudStorageR::gcs_auth(Sys.getenv('GCS_AUTH_FILE'))
    }
  }
}

gcs.upload <- function(fs){
  trajs.folder <- "data/trajectories"
  trajs.bucket <- Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public")

  gcs.auth()

  lapply(fs,
         function(f){
           # Faced HTTP/2 stream 0 was not closed cleanly: PROTOCOL_ERROR
           # Fixing attempt: force using HTTP 1.1 (the number 2 is an enum value)
           handle <- curl::new_handle(verbose = TRUE)
           curl::handle_setopt(handle, http_version = 2)
           httr::set_config(httr::config(http_version = 0))

           googleCloudStorageR::gcs_upload(f,
                                           bucket=trajs.bucket,
                                           name=paste0(trajs.folder,"/",basename(f)),
                                           predefinedAcl="default",
                                           upload_type="simple")
         })
}


gcs.download <- function(fs, dest_folder, only_if_modified_since=T, overwrite=T){

  trajs.folder <- "data/trajectories"
  trajs.bucket <- Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public")


  lapply(fs, function(file){
    tryCatch({
      url <- sprintf("https://storage.googleapis.com/%s/%s/%s", trajs.bucket, trajs.folder, file)
      dest_path <- file.path(dest_folder, file)
      config <- list()
      if(only_if_modified_since && file.exists(dest_path)){
        config <- httr::add_headers(`If-Modified-Since`=httr::http_date(file.info(dest_path)$mtime))
      }

      r <- httr::GET(url=url, config=config)
      if(r$status_code==304){
        message("Cache file already up to date. No need to update ", source_path)
      }else{
        bin <- httr::content(r, "raw")
        writeBin(bin, dest_path)
      }
    }, error=function(e){
      warning("Failed to download ", file, ": ", e)
      return(F)
    })
  })
}


#' Uploading regional fire data
#'
#' @param fs
#'
#' @return
#' @export
#'
#' @examples
gcs.upload_regional_fire <- function(fs){
  trajs.folder <- "data/fire"
  trajs.bucket <- Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public")
  gcs.auth()

  lapply(fs,
         function(f){
           googleCloudStorageR::gcs_upload(f,
                                           bucket=trajs.bucket,
                                           name=paste0(trajs.folder,"/",basename(f)),
                                           predefinedAcl="default")
         })
}

#' Downloading regional fire data
#'
#' @param fs
#' @param dest_folder
#' @param only_if_modified_since
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
gcs.download_regional_fire <- function(fs, dest_folder, only_if_modified_since=T, overwrite=T){

  trajs.folder <- "data/fire"
  trajs.bucket <- Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public")

  lapply(fs, function(file){
    tryCatch({
      url <- sprintf("https://storage.googleapis.com/%s/%s/%s", trajs.bucket, trajs.folder, file)
      dest_path <- file.path(dest_folder, file)
      config <- list()
      if(only_if_modified_since && file.exists(dest_path)){
        config <- httr::add_headers(`If-Modified-Since`=httr::http_date(file.info(dest_path)$mtime))
      }

      r <- httr::GET(url=url, config=config)
      if(r$status_code==304){
        message("Cache file already up to date. No need to update ", source_path)
      }else{
        bin <- httr::content(r, "raw")
        writeBin(bin, dest_path)
      }
    }, error=function(e){
      warning("Failed to download ", file, ": ", e)
      return(F)
    })
  })
}
