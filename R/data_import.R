
# TÜİK Enerji Teması üzerinden tablonun bağlantısını çek
enerji_tablolari <- statistical_tables("5")

# Türkçe karakter hatası almamak için İngilizce ve joker karakterle arama
kati_yakut_tablo <- enerji_tablolari %>% 
  filter(grepl("solid|kat.*yak", table_name, ignore.case = TRUE), grepl("istab", node_type, ignore.case = TRUE))

kati_yakut_url <- kati_yakut_tablo$table_url[1]

# Güvenlik kontrolü
if(is.na(kati_yakut_url)) stop("TÜİK API'den bağlantı alınamadı!")

# Bot korumasını aşarak veriyi geçici belleğe (tmp) indir
tmp_xls <- tempfile(fileext = ".xls")
resp <- GET(
  url = kati_yakut_url,
  add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0.0.0"),
  write_disk(tmp_xls, overwrite = TRUE)
)
stop_for_status(resp)

# TÜİK kapak sayfalarını atlamak için en çok satırı olan asıl sayfayı bul
sayfalar <- excel_sheets(tmp_xls)
en_dolu_sayfa <- sayfalar[1]
max_satir <- 0
for(s in sayfalar) {
  gecici_df <- read_excel(tmp_xls, sheet = s, col_names = FALSE)
  if(nrow(gecici_df) > max_satir) {
    max_satir <- nrow(gecici_df)
    en_dolu_sayfa <- s
  }
}

df_raw <- read_excel(tmp_xls, sheet = en_dolu_sayfa, col_names = FALSE)
cat("Veri başarıyla indirildi. Analiz edilen sayfa:", en_dolu_sayfa, "\n")
