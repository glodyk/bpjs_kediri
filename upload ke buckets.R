library(googleCloudStorageR)

gcs_auth("D:/Rcloud/bpjs-kediri-82616a5b1041.json")

gcs_global_bucket("bpjs-kediri")

#cek koneksi
gcs_list_objects()

upload_parquet_folder <- function(local_path, gcs_prefix="pintu_masuk"){
  
  files <- list.files(local_path,
                      recursive = TRUE,
                      full.names = TRUE)
  
  files <- files[!dir.exists(files)]
  
  base <- normalizePath(local_path)
  
  for(f in files){
    
    relative <- gsub(paste0("^", base, "/?"), "", normalizePath(f))
    gcs_name <- paste0(gcs_prefix, "/", relative)
    
    cat("Uploading:", relative, "\n")
    
    googleCloudStorageR::gcs_upload(
      file = f,
      name = gcs_name
    )
  }
}

upload_parquet_folder("D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/pintu_masuk/raw_csv/parquet_pintu_masuk")




library(googleAuthR)

# 1. autentikasi robot
gar_auth_service("D:/Rcloud/bpjs-kediri-82616a5b1041.json")

# 2. set project ID
Sys.setenv(GOOGLE_CLOUD_PROJECT = "bpjs-kediri")

# 3. set bucket default
gcs_global_bucket("bpjs-kediri")
gcs_list_objects()

gcs_auth("D:/Rcloud/bpjs-kediri-82616a5b1041.json")
gcs_global_bucket("bpjs-kediri")
gcs_list_objects()


