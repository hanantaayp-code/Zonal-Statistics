# ==============================================================================
# SKRIPSI SPILLOVER EFFECT DAN DETERMINAN LOKAL 
# KETAHANAN PANGAN DI INDONESIA TIMUR
# ==============================================================================
# NAMA		: HANANTA ANGGER YUGA PRAWIRA
# NIM		: 12020122130095
# TAHAPAN	: OTOMASI EKSTRAKSI DATA SPASIAL CURAH HUJAN (CHIRPS v2.0)#VARIABEL
# PENJELASAN 	: 	1. Skrip ini melakukan pengunduhan data raster bulanan
#			2. validasi file,
#              		3. dekompresi data
#			4. perhitungan rata-rata curah hujan tahunan
#			berbasis vektor batas administrasi kabupaten/kota.
#			5. SUMBER DATA: CHIRPS
# ==============================================================================

# ------------------------------------------------------------------------------
# BAGIAN 1: PERSIAPAN LINGKUNGAN KERJA (SYSTEM SETUP)
# ------------------------------------------------------------------------------

# 1.1. Membersihkan Memori dan Console
# Tujuannya untuk memastikan R berjalan pada lingkungan yang bersih (fresh start)
rm(list = ls()) 
gc()            
cat("\014")     

# 1.2. Pengaturan Timeout
# Memperpanjang batas waktu unduhan menjadi 600 detik (10 menit) per file
# untuk mengantisipasi koneksi internet yang tidak stabil.
options(timeout = 600)

# 1.3. Memuat Pustaka (Libraries) yang Diperlukan
# Pastikan paket-paket ini sudah terinstall sebelumnya.
library(terra)    # Untuk analisis data raster spasial
library(sf)       # Untuk analisis data vektor (shapefile)
library(dplyr)    # Untuk manajemen data frame
library(R.utils)  # WAJIB: Untuk proses dekompresi (unzip) file .gz
library(writexl)  # Untuk menyimpan hasil akhir ke format Excel (.xlsx)

# 1.4. Mengatur Direktori Kerja
setwd("C:/Users/windows/Documents/SKRIPSI FINAL") 

# 1.5. Membuat Folder Penyimpanan Data (Jika belum ada)
dir_chirps <- "CHIRPS_Data"
if (!dir.exists(dir_chirps)) dir.create(dir_chirps)


# ------------------------------------------------------------------------------
# BAGIAN 2: PERSIAPAN DATA VEKTOR (WILAYAH STUDI)
# NOTE: 1. PENULIS MENGGUNAKAN SHAPE FILE 514 KABUPATEN KOTA
#	2. PERLU FILTERING SHAPE FILE HANYA 185 WILAYAH OBSERVASI
#	2. PASTIKAN NAMA KABUPATEN DAN KOTA IDENTIK ANTARA SHAPE FILE DAN DATA SET
#	3. PASTIKAN NAMA KOLOM KABUPATEN KOTA SAMA
# ------------------------------------------------------------------------------
cat("=== [TAHAP 1] MEMUAT DATA BATAS ADMINISTRASI ===\n")

# 2.1. Membaca Shapefile Indonesia
# sf_use_s2(FALSE) digunakan untuk menonaktifkan geometri sferis (menghindari error topologi)
sf_use_s2(FALSE) 
shp_full <- st_read("INDONESIA.shp", quiet = TRUE) %>% st_make_valid()

# 2.2. Memfilter Kabupaten/Kota Sampel (185 Wilayah)
target_csv  <- read.csv("NAMAKABKOT2.csv", header = TRUE) 
list_target <- toupper(as.character(target_csv$KAB_KOTA)) 
shp_185     <- shp_full %>% filter(toupper(KAB_KOTA) %in% list_target)

# 2.3. Konversi ke Format SpatVector (Terra)
# Diperlukan untuk mempercepat proses ekstraksi data raster
shp_terra <- vect(shp_185)

cat(paste0("-> Jumlah Wilayah Studi: ", nrow(shp_185), " Kabupaten/Kota.\n"))


# ------------------------------------------------------------------------------
# BAGIAN 3: PENGUNDUHAN DAN VALIDASI DATA (DOWNLOADER MODULE)
# NOTE	1. PENULIS MENCOBA MENGGUNAKA  LIBRARY NAMUN GAGAL 
#	2. OLEH KARENA ITU PENULIS MENGGUNAKAN BASE URL
# ------------------------------------------------------------------------------
cat("\n=== [TAHAP 2] PENGUNDUHAN DATA CHIRPS (DIRECT URL) ===\n")

years    <- 2018:2022
base_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/"

for (yr in years) {
  for (mo in 1:12) {
    
    # Format nama file (YYYY.MM)
    mo_str <- sprintf("%02d", mo)
    file_gz  <- paste0("chirps-v2.0.", yr, ".", mo_str, ".tif.gz")
    path_gz  <- file.path(dir_chirps, file_gz)
    
    # URL Lengkap Sumber Data
    full_url <- paste0(base_url, file_gz)
    
    # --- VALIDASI LEVEL 1: CEK KEBERADAAN FILE ---
    # Cek apakah file .gz sudah ada DAN ukurannya valid (> 100 KB)
    # File di bawah 100 KB dianggap corrupt/rusak.
    file_exists <- file.exists(path_gz)
    file_valid  <- FALSE
    if (file_exists) {
      if (file.info(path_gz)$size > 100000) file_valid <- TRUE
    }
    
    # --- PROSES DOWNLOAD ---
    if (!file_valid) {
      cat(paste0("   [DOWNLOADING] ", file_gz, "... "))
      
      tryCatch({
        # Mode 'wb' (write binary) wajib untuk file raster
        download.file(full_url, path_gz, mode = "wb", quiet = TRUE)
        
        # Cek ulang setelah download
        if (file.info(path_gz)$size > 100000) {
          cat("BERHASIL.\n")
        } else {
          cat("GAGAL (File Corrupt/0 KB). Menghapus file...\n")
          file.remove(path_gz)
        }
      }, error = function(e) {
        cat("ERROR KONEKSI.\n")
      })
    } else {
      # Jika file sudah ada dan valid, lewati (Skip)
      # cat(paste0("   [SKIP] ", file_gz, " sudah tersedia.\n")) 
    }
  }
}


# ------------------------------------------------------------------------------
# BAGIAN 4: DEKOMPRESI DAN VALIDASI RASTER (UNZIPPER MODULE)
# ------------------------------------------------------------------------------
cat("\n=== [TAHAP 3] EKSTRAKSI FILE .GZ MENJADI .TIF ===\n")

gz_files <- list.files(dir_chirps, pattern = "\\.gz$", full.names = TRUE)

for (gz in gz_files) {
  # Tentukan nama file tujuan (.tif)
  tif_path <- sub(".gz$", "", gz)
  
  # --- VALIDASI LEVEL 2: CEK FILE HASIL EKSTRAK ---
  # Kita hanya ekstrak jika file .tif belum ada atau ukurannya mencurigakan (< 1MB)
  need_unzip <- TRUE
  if (file.exists(tif_path)) {
    if (file.info(tif_path)$size > 1000000) need_unzip <- FALSE
  }
  
  if (need_unzip) {
    cat(paste0("   [UNZIPPING] ", basename(gz), "... "))
    
    tryCatch({
      # gunzip dengan remove=FALSE agar file master (.gz) tidak hilang
      gunzip(gz, destname = tif_path, remove = FALSE, overwrite = TRUE)
      
      # Validasi Akhir
      if (file.info(tif_path)$size > 1000000) {
        cat("SUKSES.\n")
      } else {
        cat("GAGAL (Hasil ekstrak rusak).\n")
      }
    }, error = function(e) {
      cat("ERROR saat dekompresi.\n")
    })
  }
}


# ------------------------------------------------------------------------------
# BAGIAN 5: PENGOLAHAN DATA SPASIAL (ZONAL STATISTICS)
#	1. PASTIKAN FILE SUDAH TEREXTRAK DENGAN BAIK DI TAHAP SEBELUMNYA
# ------------------------------------------------------------------------------
cat("\n=== [TAHAP 4] PERHITUNGAN CURAH HUJAN TAHUNAN ===\n")

final_storage <- list()
counter <- 1

for (yr in years) {
  cat(paste0("\n>>> Memproses Tahun: ", yr, "...\n"))
  
  # 5.1. Mencari File Raster .tif
  pola_file <- paste0("chirps-v2.0.", yr, ".*\\.tif$") 
  files_tif <- list.files(dir_chirps, pattern = pola_file, full.names = TRUE)
  
  # Pastikan tidak mengambil file .gz
  files_tif <- files_tif[!grepl(".gz$", files_tif)]
  
  # Validasi Jumlah Bulan
  if(length(files_tif) < 12) {
    cat("   [WARNING] Data tahun ini tidak lengkap (<12 file). Dilewati.\n")
    next
  }
  
  # 5.2. Raster Operations
  # Membaca 12 file sekaligus (Stacking)
  r_stack <- rast(files_tif)
  
  # Memotong area sesuai batas Indonesia (Cropping)
  r_indo  <- crop(r_stack, shp_terra)
  
  # Menghitung Rata-rata Tahunan (Mean of 12 months)
  r_mean  <- mean(r_indo, na.rm=TRUE)
  
  cat("   -> Melakukan ekstraksi zonal statistics...\n")
  
  # 5.3. Ekstraksi Nilai ke Kabupaten
  # Menggunakan terra::extract untuk menghindari konflik nama fungsi
  val_hujan <- terra::extract(r_mean, shp_terra, fun=mean, na.rm=TRUE)
  
  # 5.4. Menyimpan ke Data Frame
  for(k in 1:nrow(shp_185)){
     final_storage[[counter]] <- data.frame(
        KAB_KOTA = shp_185$KAB_KOTA[k],
        Tahun    = yr,
        Avg_CurahHujan_mm_hari = round(val_hujan[k, 2], 3)
     )
     counter <- counter + 1
  }
  
  cat("   -> Selesai tahun ", yr, ".\n")
  gc() # Membersihkan sampah memori (Garbage Collection)
}


# ------------------------------------------------------------------------------
# BAGIAN 6: PENYIMPANAN HASIL AKHIR (OUTPUT)
# ------------------------------------------------------------------------------
cat("\n=== [TAHAP 5] PENYIMPANAN DATA ===\n")

if (length(final_storage) > 0) {
  # Menggabungkan list menjadi satu tabel utuh
  df_final <- bind_rows(final_storage)
  
  # Menyimpan ke Excel
  output_name <- "Data_Skripsi_CurahHujan_CHIRPS_Final.xlsx"
  write_xlsx(df_final, output_name)
  
  cat(paste0("SUKSES! Data berhasil disimpan sebagai: ", output_name, "\n"))
  print(head(df_final))
  
} else {
  cat("GAGAL: Tidak ada data yang berhasil diproses.\n")
}

cat("\n=== PROSES SELESAI ===\n")
