# ==============================================================================================
# SKRIPSI SPILLOVER EFFECT DAN DETERMINAN LOKAL
# KETAHANAN PANGAN DI INDONESIA TIMUR
# ==============================================================================================
# NAMA          : HANANTA ANGGER YUGA PRAWIRA
# NIM           : 12020122130095
# TAHAPAN       : OTOMASI EKSTRAKSI DATA SPASIAL LAHAN PERTANIAN (SENTINEL-2)
# VARIABEL      : PERSENTASE LUAS LAHAN (SAWAH + LADANG)
# PENJELASAN    : 1. Skrip ini mengakses API ArcGIS ImageServer secara manual (Direct URL).
#                 2. Menggunakan parameter waktu 'Epoch Time' (1 Juli) untuk memastikan
#                    variansi data antar tahun (Time-Variant).
#                 3. Melakukan cropping dan masking sesuai batas administrasi kabupaten.
#                 4. Menghitung Zonal Statistics (frekuensi piksel) untuk kelas:
#                    - Kelas 4: Flooded Vegetation (Sawah/Lahan Basah)
#                    - Kelas 5: Crops (Pertanian Lahan Kering/Ladang)
#                 5. SUMBER DATA: Sentinel-2 10m Land Use/Land Cover (Esri/Impact Observatory)
# ==============================================================================================

# ----------------------------------------------------------------------------------------------
# BAGIAN 1: PERSIAPAN LINGKUNGAN KERJA (SYSTEM SETUP)
# ----------------------------------------------------------------------------------------------

# 1.1. Membersihkan Memori (Fresh Start)
rm(list = ls()) 
gc()            
cat("\014")     

# 1.2. Memuat Pustaka (Libraries)
library(terra)    # Pengolahan data raster spasial
library(sf)       # Pengolahan data vektor (shapefile)
library(dplyr)    # Manipulasi data frame
library(writexl)  # Menyimpan output ke Excel (.xlsx)

# 1.3. Mengatur Direktori Kerja
setwd("C:/Users/windows/Documents/SKRIPSI FINAL") 

# 1.4. Pengaturan Geometri
# Mematikan S2 Geometry engine agar R memproses peta sebagai bidang datar (Planar)
# Ini mencegah error topologi saat proses masking/cropping
sf_use_s2(FALSE) 


# ----------------------------------------------------------------------------------------------
# BAGIAN 2: INPUT DATA VEKTOR (WILAYAH STUDI)
# ----------------------------------------------------------------------------------------------
cat("=== [TAHAP 1] MEMUAT DATA BATAS ADMINISTRASI ===\n")

# 2.1. Membaca Shapefile Indonesia
# St_make_valid digunakan untuk memperbaiki geometri poligon yang mungkin error
shp_full <- st_read("INDONESIA.shp", quiet = TRUE) %>% st_make_valid()

# 2.2. Membaca Daftar Sampel (185 Kabupaten/Kota)
target_csv <- read.csv("NAMAKABKOT2.csv", header = TRUE) 

# 2.3. Filtering Data
# Mencocokkan nama kabupaten di Shapefile dengan daftar di CSV
list_target <- toupper(as.character(target_csv$KAB_KOTA)) 
shp_185     <- shp_full %>% filter(toupper(KAB_KOTA) %in% list_target)

cat(paste0("-> Target Analisis: ", nrow(shp_185), " Kabupaten/Kota.\n"))


# ----------------------------------------------------------------------------------------------
# BAGIAN 3: DEFINISI FUNGSI URL (EPOCH TIME LOGIC)
# ----------------------------------------------------------------------------------------------
# Fungsi ini bertugas merakit URL request ke server Esri.
# PENTING: Menggunakan timestamp '1 Juli' (Mid-Year) agar data merepresentasikan
# kondisi tahun berjalan, bukan sisa tahun sebelumnya.

get_map_url <- function(bbox, year) {
  
  # Konversi Tahun ke Epoch Time (Milliseconds sejak 1 Jan 1970)
  # Tanggal yang digunakan: 1 Juli Pukul 00:00:00 UTC
  epoch <- switch(as.character(year),
                  "2018" = "1530403200000",
                  "2019" = "1561939200000",
                  "2020" = "1593561600000",
                  "2021" = "1625097600000",
                  "2022" = "1656633600000")
  
  # Mengambil koordinat kotak pembatas (Bounding Box) per kabupaten
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]
  
  # Base URL ArcGIS ImageServer Export
  base <- "https://ic.imagery1.arcgis.com/arcgis/rest/services/Sentinel2_10m_LandCover/ImageServer/exportImage"
  
  # Merakit Parameter URL
  # size=1500,1500 : Memaksa server memberikan resolusi tinggi (High-Res)
  params <- paste0(
    "?bbox=", xmin, ",", ymin, ",", xmax, ",", ymax,
    "&bboxSR=4326",
    "&size=1500,1500",     
    "&imageSR=4326",
    "&time=", epoch,       # Parameter Waktu Kunci
    "&format=tiff",
    "&pixelType=U8",
    "&f=image"
  )
  
  return(paste0(base, params))
}


# ----------------------------------------------------------------------------------------------
# BAGIAN 4: LOOPING EKSTRAKSI DATA (CORE PROCESS)
# ----------------------------------------------------------------------------------------------
cat("\n=== [TAHAP 2] MEMULAI EKSTRAKSI DATA SENTINEL-2 ===\n")

years <- 2018:2022
final_storage <- list()
counter <- 1

# Loop Utama: Berdasarkan Kabupaten (Agar resolusi tetap tinggi per wilayah)
for (i in 1:nrow(shp_185)) {
  
  kab_now  <- shp_185[i, ]
  nama_kab <- kab_now$KAB_KOTA
  bbox     <- st_bbox(kab_now)
  
  # Menampilkan progres di console
  cat(paste0("\nProcessing [", i, "/", nrow(shp_185), "]: ", nama_kab, "\n"))
  
  # Konversi vektor ke format Terra untuk proses masking
  roi_terra <- vect(kab_now)
  
  # Loop Waktu: 2018 s.d. 2022
  for (yr in years) {
    
    # 4.1. Mendapatkan URL Spesifik
    my_url <- get_map_url(bbox, yr)
    
    # 4.2. Download & Load Raster (Virtual)
    # Menggunakan try() untuk error handling jika koneksi server putus
    r_temp <- try(rast(my_url), silent = TRUE)
    
    if (!inherits(r_temp, "try-error")) {
      
      # 4.3. Masking (Memotong raster sesuai bentuk poligon kabupaten)
      r_mask <- try(mask(r_temp, roi_terra), silent = TRUE)
      
      if (!inherits(r_mask, "try-error")) {
        
        # 4.4. Zonal Statistics (Menghitung Frekuensi Pixel)
        vals    <- freq(r_mask)
        df_vals <- as.data.frame(vals)
        tot_pix <- sum(df_vals$count, na.rm=TRUE)
        
        if (tot_pix > 0) {
          # Klasifikasi Pixel Sentinel-2 LULC:
          # Value 4 = Flooded Vegetation (Sawah/Lahan Basah)
          # Value 5 = Crops (Ladang/Kebun/Pertanian Lahan Kering)
          
          sawah  <- sum(df_vals$count[df_vals$value == 4], na.rm=TRUE)
          ladang <- sum(df_vals$count[df_vals$value == 5], na.rm=TRUE)
          
          # Menghitung Persentase
          pct_sawah  <- round((sawah/tot_pix)*100, 3)
          pct_ladang <- round((ladang/tot_pix)*100, 3)
          pct_agri   <- pct_sawah + pct_ladang
          
          # Tampilkan hasil sementara di console (untuk monitoring)
          cat(paste0("   -> ", yr, ": Total Pertanian ", pct_agri, "% \n"))
          
          # 4.5. Simpan ke List Sementara
          final_storage[[counter]] <- data.frame(
            KAB_KOTA = nama_kab,
            Tahun = yr,
            Persen_Sawah = pct_sawah,
            Persen_Ladang = pct_ladang,
            Total_LahanPertanian = pct_agri
          )
          counter <- counter + 1
        }
      }
    }
    # Membersihkan RAM setiap iterasi (Penting!)
    gc() 
  }
}


# ----------------------------------------------------------------------------------------------
# BAGIAN 5: PENYIMPANAN HASIL AKHIR (OUTPUT)
# ----------------------------------------------------------------------------------------------
cat("\n=== [TAHAP 3] PENYIMPANAN DATA ===\n")

if (length(final_storage) > 0) {
  # Menggabungkan seluruh list menjadi satu tabel Data Frame
  df_fix <- bind_rows(final_storage)
  
  # Nama File Output
  nama_file_excel <- "Data_Skripsi_LahanPertanian_Sentinel2_Final.xlsx"
  
  # Menyimpan ke format Excel
  write_xlsx(df_fix, nama_file_excel)
  
  cat(paste0("\nSUKSES! Data berhasil disimpan sebagai: ", nama_file_excel, "\n"))
  print(head(df_fix)) # Tampilkan preview data
  
} else {
  cat("GAGAL: Tidak ada data yang berhasil diambil. Cek koneksi internet.\n")
}

cat("\n=== PROSES SELESAI ===\n")
