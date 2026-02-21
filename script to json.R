library(data.table)
library(jsonlite)
library(stringi)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/New folder")

dt <- fread(
  "Sheet 1_Full Data_kediri (des2025).csv",
  sep = ",",
  quote = "\"",
  encoding = "UTF-8",
  fill = TRUE,
  data.table = TRUE,
  showProgress = TRUE
)

dim(dt)

dt <- dt[, lapply(.SD, function(x){
  
  x <- as.character(x)
  
  # hilangkan line break dalam cell
  x <- stri_replace_all_regex(x, "[\\r\\n]+", " ")
  
  # hilangkan karakter kontrol
  x <- stri_replace_all_regex(x, "[[:cntrl:]]", " ")
  
  # rapikan spasi
  x <- stri_trim_both(x)
  
  # kosongkan NA
  x[x == "NA"] <- NA
  
  return(x)
  
})]

names(dt) <- make.names(names(dt))

library(jsonlite)

con <- file("BPJS_BIGQUERY_DES25.json", open = "w", encoding = "UTF-8")

n <- nrow(dt)

cat("Mulai konversi ke JSON...\n")

for(i in seq_len(n)){
  
  # ubah 1 baris menjadi named list
  row_list <- as.list(dt[i])
  
  # convert ke JSON satu baris
  line <- toJSON(
    row_list,
    auto_unbox = TRUE,
    na = "null",
    null = "null"
  )
  
  # tulis ke file
  writeLines(line, con, useBytes = TRUE)
  
  # progress setiap 5000 baris
  if(i %% 5000 == 0){
    cat("Sudah:", i, "dari", n, "\n")
  }
}

close(con)

cat("SELESAI! File BPJS_BIGQUERY.json siap diupload\n")
