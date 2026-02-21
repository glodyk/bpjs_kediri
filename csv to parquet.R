# STEP 1 — Siapkan struktur folder (PENTING SEKALI)
Documents/bpjs_project/
  │
  ├── raw_csv/
  ├── parquet/
  └── script/
  
# STEP 2 — Install package R
install.packages(c(
  "arrow",
  "data.table",
  "dplyr",
  "stringr",
  "googleCloudStorageR",
  "bigrquery"
))

# ================= CBG ===========================================================
# STEP 3 — R membaca SEMUA CSV sekaligus
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/New folder/raw_csv")

library(data.table)

# ambil semua file csv
files <- list.files(pattern="\\.csv$", full.names=TRUE)
files

# baca semua file sekaligus (ini super penting)
dt <- rbindlist(lapply(files, fread), fill=TRUE)

dim(dt)

readLines(files[1], n = 5)

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# ubah tanggal
library(data.table)

dt[, tglpelayanan := as.IDate(tglpelayanan, format="%m/%d/%Y")]
dt[, tgldtgsjp    := as.IDate(tgldtgsjp,    format="%m/%d/%Y")]
dt[, tglplgsjp    := as.IDate(tglplgsjp,    format="%m/%d/%Y")]
dt[, tglreg       := as.IDate(tglreg,       format="%m/%d/%Y")]
dt[, tglstjkeu    := as.IDate(tglstjkeu,    format="%m/%d/%Y")]
dt[, tglversjp    := as.IDate(tglversjp,    format="%m/%d/%Y")]

str(dt$tglpelayanan)

# buat kolom tahun
dt[, year := format(tglpelayanan, "%Y")]

# STEP 5 — Ubah ke PARQUET

library(arrow)

write_dataset(
  dt,
  path = "D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/New folder/raw_csv/parquet_cbg",
  format = "parquet",
  partitioning = "year"
)

# ================= PINTU MASUK ===========================================================
# STEP 3 — R membaca SEMUA CSV sekaligus
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/pintu_masuk/raw_csv")
library(data.table)
kolom_perlu <- c(
  "Nosep","Flagseptagih","Ispcare","jenisrujukaninternal","Kddiag","Kddokter",
  "Kdjnsppk","Kdjnsppkperujuk","Kdkc","Kdkcperujuk","Kdkr","Kdkrperujuk","Kdppk",
  "Kdppkperujuk","Kelasrsmenkes","Kelasrsperujuk","Nmdiag","Nmdokter","Nmjnsppk",
  "Nmjnsppkperujuk","Nmkc","Nmkcperujuk","Nmkr","Nmkrperujuk","Nmppk","Nmppkperujuk",
  "Nokapst","Norjkawalsep","Politujsep","Potensiprb","Spesialistik","Sumber","Tacc",
  "Tglpelayanan"
)

files <- list.files(pattern="\\.csv$", full.names=TRUE)

dt <- rbindlist(
  lapply(files, function(f)
    fread(f, select = kolom_perlu, showProgress = FALSE)
  ),
  fill = TRUE
)

dim(dt)

readLines(files[1], n = 5)

# baca 0 baris saja → hanya struktur kolom
header <- fread(files[1], nrows = 0)
names(header)

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# ubah tanggal
library(data.table)
dt[, tglpelayanan := as.IDate(tglpelayanan, format="%m/%d/%Y")]
str(dt$tglpelayanan)

# buat kolom tahun
dt[, year := format(tglpelayanan, "%Y")]

# STEP 5 — Ubah ke PARQUET
library(arrow)

write_dataset(
  dt,
  path = "D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/pintu_masuk/raw_csv/parquet_pintu_masuk",
  format = "parquet",
  partitioning = "year"
)

# ================= RUJUKAN FKTP ===========================================================
# STEP 3 — R membaca SEMUA CSV sekaligus
setwd("D:/data gresik/FactKunjungan/raw_csv")
library(data.table)
kolom_perlu <- c("nmstatuspulang","alasantacc","anamnesa","beratbadan","diastole","dokter",
                 "heart_rate","imt","jenistenagamedis","jkpst","kd_diagnosa","kd_diagnosa2",
                 "kd_diagnosa3","kdbu","kddati2kunjungan","kddati2peserta","kddati2rujukan",
                 "kdjnspeserta","kdkcbu","kdkckunjungan","kdkcpeserta","kdkcrujukan","kdkrbu",
                 "kdkrkunjungan","kdkrpeserta","kdkrrujukan","kdpoli","kdppkkunjungan",
                 "kdppkrujukan","kdpropkunjungan","kdproppeserta","kdproprujukan","kdstatuspulang",
                 "kdtacc","kdtindakan","kdtkp","keluhan","kunjsakit","lingkarperut","nm_diagnosa",
                 "nm_diagnosa2","nm_diagnosa3","nmbu","nmdati2kunjungan","nmdati2peserta",
                 "nmdati2rujukan","nmjenisprb","nmjenisprolanis","nmjnsppkkunjungan",
                 "nmjnsppkpeserta","nmjnsppkrujukan","nmkcbu","nmkckunjungan","nmkcpeserta",
                 "nmkcrujukan","nmkepemilikankunjungan","nmkepemilikanppkpeserta",
                 "nmkepemilikanppkrujukan","nmkrbu","nmpoli","nmppkkunjungan","nmppkpeserta",
                 "nmppkrujukan","nmpropkunjungan","nmproppeserta","nmproprujukan","nmpst",
                 "nmtacc","nmtindakan","nmtipeppkkunjungan","nmtipeppkpeserta","nmtipeppkrujukan",
                 "nmtkp","no_kunjungan","nokapst","pisapst","poli_rujuk","ppkpeserta",
                 "pstprb","pstprolanis","resp_rate","segmenpeserta","sistole","spesialistik",
                 "suhu","sumberpendaftaran","terapimedikamentosa","terapinonmedikamentosa",
                 "tgl_kunjungan","tglakhir_prb","tglakhir_prolanis","tgllhrpst",
                 "tglmulai_prb","tglmulai_prolanis","tglpulang","tinggibadan"
                 )

files <- list.files(pattern="\\.csv$", full.names=TRUE)

dt <- rbindlist(
  lapply(files, function(f)
    fread(f, select = kolom_perlu, showProgress = FALSE)
  ),
  fill = TRUE
)

dim(dt)
readLines(files[1], n = 5)

# baca 0 baris saja → hanya struktur kolom
header <- fread(files[1], nrows = 0)
names(header)

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# ubah tanggal
library(data.table)
dt[, tgl_kunjungan := as.IDate(tgl_kunjungan, format="%m/%d/%Y")]
dt[, tglakhir_prb := as.IDate(tglakhir_prb, format="%m/%d/%Y")]
dt[, tglakhir_prolanis := as.IDate(tglakhir_prolanis, format="%m/%d/%Y")]
dt[, tgllhrpst := as.IDate(tgllhrpst, format="%m/%d/%Y")]
dt[, tglmulai_prb := as.IDate(tglmulai_prb, format="%m/%d/%Y")]
dt[, tglmulai_prolanis := as.IDate(tglmulai_prolanis, format="%m/%d/%Y")]
dt[, tglpulang := as.IDate(tglpulang, format="%m/%d/%Y")]

str(dt$tgl_kunjungan)
str(dt$tglakhir_prb)
str(dt$tglakhir_prolanis)
str(dt$tgllhrpst)
str(dt$tglmulai_prb)
str(dt$tglmulai_prolanis)
str(dt$tglpulang)

# buat kolom tahun
dt[, year := format(tgl_kunjungan, "%Y")]

# STEP 5 — Ubah ke PARQUET
library(arrow)

write_dataset(
  dt,
  path = "D:/data gresik/FactKunjungan/raw_csv/parquet_rujukan_fktp",
  format = "parquet",
  partitioning = "year"
)

# ================= METAFISIK ===========================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/metafisik/raw")

library(data.table)
kolom_perlu <- c(
  "biayars","Bystjsep","Bytagsep","Byversep","Deskripsisd","Deskripsisi",
  "Deskripsisp","Deskripsisr","Diagsekunder","flag_biometrik","flag_iterasi",
  "jenis_kelamin","jenisppkperujuk","Kddati2Layan","Kddiagprimer","Kddokter","Kdinacbgs",
  "Kdkclayan","Kdkrlayan","Kdppklayan","kdppkperujuk","Kdsa","Kdsd","Kdsi","Kdsp","Kdsr",
  "Klsrawat","Nmdati2Layan","nmdati2perujuk","Nmdiagprimer","Nmdokter","Nminacbgs","Nmjnspulang",
  "nmkclayan","nmkcperujuk","Nmppklayan","nmppkperujuk","Nmtkp","No Bast","No Surat Bast",
  "Nokapst","Nosjp","politujsep","Prosedur","Severity Level","Tarifgrup","Tarifsa","Tarifsd",
  "Tarifsi","Tarifsp","Tarifsr","Tgl Bast","Tgldtgsep","Tglpelayanan","Tglplgsep","Umur Tahun"
)

files <- list.files(pattern="\\.csv$", full.names=TRUE)

dt <- rbindlist(
  lapply(files, function(f)
    fread(f, select = kolom_perlu, showProgress = FALSE)
  ),
  fill = TRUE
)

dim(dt)
readLines(files[1], n = 5)

# baca 0 baris saja → hanya struktur kolom
header <- fread(files[1], nrows = 0)
names(header)

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# ubah tanggal
library(data.table)
dt[, tgl_bast := as.IDate(tgl_bast, format="%m/%d/%Y")]
dt[, tgldtgsep := as.IDate(tgldtgsep, format="%m/%d/%Y")]
dt[, tglpelayanan := as.IDate(tglpelayanan, format="%m/%d/%Y")]
dt[, tglplgsep := as.IDate(tglplgsep, format="%m/%d/%Y")]

# buat kolom tahun
dt[, year := format(tglpelayanan, "%Y")]

# STEP 5 — Ubah ke PARQUET
library(arrow)

write_dataset(
  dt,
  path = "D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/metafisik/raw/metafisik_parq",
  format = "parquet",
  partitioning = "year"
)


# ================= KAPITASI ===========================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/kapitasi")
library(data.table)

# ambil semua file csv
files <- list.files(pattern="\\.csv$", full.names=TRUE)
files

# baca semua file sekaligus (ini super penting)
dt <- rbindlist(lapply(files, fread), fill=TRUE)

dim(dt)
readLines(files[1], n = 5)

# baca 0 baris saja → hanya struktur kolom
header <- fread(files[1], nrows = 0)
names(header)

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# ubah tanggal
library(data.table)
dt[, insertdate := as.IDate(insertdate, format="%m/%d/%Y")]
dt[, tanggalbayar := as.IDate(tanggalbayar, format="%m/%d/%Y")]
dt[, tglkoreksi := as.IDate(tglkoreksi, format="%m/%d/%Y")]
dt[, tglpelayanan := as.IDate(tglpelayanan, format="%m/%d/%Y")]
dt[, tglreg := as.IDate(tglreg, format="%m/%d/%Y")]
dt[, tglstjkeu := as.IDate(tglstjkeu, format="%m/%d/%Y")]
dt[, tglstjreg := as.IDate(tglstjreg, format="%m/%d/%Y")]
dt[, tglverreg := as.IDate(tglverreg, format="%m/%d/%Y")]

# buat kolom tahun
dt[, year := format(tglpelayanan, "%Y")]

# STEP 5 — Ubah ke PARQUET
library(arrow)

write_dataset(
  dt,
  path = "D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/kapitasi/kapitasi_parq",
  format = "parquet",
  partitioning = "year"
)
# ================= NONKAPITASI ===========================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/non_kapitasi")
# STEP 3 — R membaca SEMUA CSV sekaligus

library(data.table)
kolom_perlu <- c(
  "Biaya","flag_shk","Hasil","Hasil2","Isffs","Jenisppklayan","jenisppkterdaftar",
  "Jkpst","Jnsnakes","Kddati2Layan","Kddati2Terdaftar","Kddiagnosa","Kdjnspeserta",
  "Kdkclayan","Kdkcpks","Kdkcterdaftar","Kdkrlayan","Kdkrpks","Kdkrterdaftar","Kdnakes","Kdpks",
  "Kdppklayan","Kdppkterdaftar","Kdproplayan","Kdpropterdaftar","Kdstatuspulang","Kdtindakan",
  "Kdtindakansk","Kepemilikanppklayan","kepemilikanppkterdaftar","ketberkaslengkap","ketserahterima",
  "lmstjklaim","Namanakes","Nmdati2Layan","Nmdati2Terdaftar","Nmdiagnosa","Nmkclayan","Nmkcpks",
  "Nmkcterdaftar","Nmpks","Nmppklayan","Nmppkterdaftar","Nmproplayan","Nmpropterdaftar",
  "Nmstatuspulang","Nmtindakan","Nmtkp","Nofpk","Nokapst","Noreg","Nosjp","Pisapst","Pstprb",
  "Rangeumur","Segmen","status_fpk","Tglbayar","tglberkaslengkap","Tglfpk","Tglfpk Entri",
  "Tglkunjungan","Tglpelayanan","Tglpulang","Tglreg","tglselesaiverifikasi","tglserahterima",
  "tglsetujuhasilverifikasi","Tglstjkeu","Tglstjreg","Tgltindakan","Typeppklayan",
  "typeppkterdaftar","Umur"
)

files <- list.files(pattern="\\.csv$", full.names=TRUE)

dt <- rbindlist(
  lapply(files, function(f)
    fread(f, select = kolom_perlu, showProgress = FALSE)
  ),
  fill = TRUE
)

dim(dt)
readLines(files[1], n = 5)

# baca 0 baris saja → hanya struktur kolom
header <- fread(files[1], nrows = 0)
names(header)

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# ubah tanggal
library(data.table)
dt[, tglbayar := as.IDate(tglbayar, format="%m/%d/%Y")]
dt[, tglberkaslengkap := as.IDate(tglberkaslengkap, format="%m/%d/%Y")]
dt[, tglfpk := as.IDate(tglfpk, format="%m/%d/%Y")]
dt[, tglfpk_entri := as.IDate(tglfpk_entri, format="%m/%d/%Y")]
dt[, tglkunjungan := as.IDate(tglkunjungan, format="%m/%d/%Y")]
dt[, tglpelayanan := as.IDate(tglpelayanan, format="%m/%d/%Y")]
dt[, tglpulang := as.IDate(tglpulang, format="%m/%d/%Y")]
dt[, tglreg := as.IDate(tglreg, format="%m/%d/%Y")]
dt[, tglselesaiverifikasi := as.IDate(tglselesaiverifikasi, format="%m/%d/%Y")]
dt[, tglsetujuhasilverifikasi := as.IDate(tglsetujuhasilverifikasi, format="%m/%d/%Y")]
dt[, tglserahterima := as.IDate(tglserahterima, format="%m/%d/%Y")]
dt[, tglstjkeu := as.IDate(tglstjkeu, format="%m/%d/%Y")]
dt[, tglstjreg := as.IDate(tglstjreg, format="%m/%d/%Y")]
dt[, tgltindakan := as.IDate(tgltindakan, format="%m/%d/%Y")]

# buat kolom tahun
dt[, year := format(tglpelayanan, "%Y")]

# STEP 5 — Ubah ke PARQUET
library(arrow)

write_dataset(
  dt,
  path = "D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/non_kapitasi/non_kapitasi_parq",
  format = "parquet",
  partitioning = "year"
)

# ================= OBAT ===========================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/obat_hv")
library(data.table)

library(data.table)
kolom_perlu <- c(
  "biaya","biayaverifikasi","dagang","diagsekunder","flag_iterasi","generik",
  "jenis_obat_luarpaket","jenisobat","jkpst","jmlobt","katastropik","kd_diagprimer",
  "kdcmg","kddati2layan","kddati2resep","kddati2terdaftar","kddokter","kdinacbgs",
  "kdkclayan","kdkcpks","kdkcresep","kdkcterdaftar","kdkrlayan","kdkrpks","kdkrresep",
  "kdkrterdaftar","kdobat","kdpks","kdppklayan","kdppkresep","kdppkterdaftar","kdproplayan",
  "kdpropresep","kdpropterdaftar","kelashak","kelasrsmenkes","nm_diagprimer","nmcmg",
  "nmdati2layan","nmdati2resep","nmdati2terdaftar","nmdokter","nminacbgs","nmjnsppkresep",
  "nmkclayan","nmkcpks","nmkcresep","nmkcterdaftar","nmkepemilikanppkresep","nmkrlayan",
  "nmkrresep","nmkrterdaftar","nmpks","nmppklayan","nmppkresep","nmppkterdaftar",
  "nmproplayan","nmpropresep","nmpropterdaftar","nmpst","nmtkp","nmtypeppkresep","nofpk",
  "nokapst","noreg","noresep","nosjp_apotek","obat","obatluarpaket","pabrik","pisat",
  "politujsjp","pstprb","pstprolanis","refasalsjp","sediaan","segmen","tglfpk","tglfpk_entry",
  "tglpelayanan","tglreg","tglrsp","tglsjp","tglstjkeu","tglstjreg","umur"
)

files <- list.files(pattern="\\.csv$", full.names=TRUE)

dt <- rbindlist(
  lapply(files, function(f)
    fread(f, select = kolom_perlu, showProgress = FALSE)
  ),
  fill = TRUE
)

dim(dt)
readLines(files[1], n = 5)

# baca 0 baris saja → hanya struktur kolom
header <- fread(files[1], nrows = 0)
names(header)

# STEP 4 — Bersihkan kolom
library(dplyr)
library(stringr)

names(dt) <- names(dt) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_")

names(dt)[1:30]

# ubah tanggal
library(data.table)
dt[, tglfpk := as.IDate(tglfpk, format="%m/%d/%Y")]
dt[, tglfpk_entry := as.IDate(tglfpk_entry, format="%m/%d/%Y")]
dt[, tglfpk_entry := as.IDate(tglfpk_entry, format="%m/%d/%Y")]
dt[, tglpelayanan := as.IDate(tglpelayanan, format="%m/%d/%Y")]
dt[, tglreg := as.IDate(tglreg, format="%m/%d/%Y")]
dt[, tglrsp := as.IDate(tglrsp, format="%m/%d/%Y")]
dt[, tglfpk_entry := as.IDate(tglfpk_entry, format="%m/%d/%Y")]
dt[, tglsjp := as.IDate(tglstjkeu, format="%m/%d/%Y")]

# buat kolom tahun
dt[, year := format(tglpelayanan, "%Y")]

# STEP 5 — Ubah ke PARQUET
library(arrow)

write_dataset(
  dt,
  path = "D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri/obat_hv/obat_parq",
  format = "parquet",
  partitioning = "year"
)
