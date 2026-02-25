library(shiny)
library(shinythemes)
library(mirt)
library(dplyr)
library(openxlsx)
library(stringr)

# =========================================================
# PARAMETER ITEM GPCM (TETAP)
# =========================================================
item_params <- data.frame(
  item = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12", "B13", "B14"),
  a = c(0.551188258369724, 0.347278746235286, 0.423153109494036, 0.242269256977915,
        0.424897450125876, 0.441685842567005, 0.685019215110543, 0.327915677770387,
        0.205372877408807, 0.507793072806735, 0.392796685178935, 0.452633023754929,
        0.209075000229552, 0.453059087202066),
  b1 = c(-5.70233262774502, -7.6407654850103, -5.69025028104883, -7.62522201136247,
         -6.7065341608023, -3.79618790109145, -3.00451624000066, -4.38510894613797,
         -6.85140388272922, 5.32659651173973, -5.04014387980025, -4.29239470820237,
         -5.09038543690274, -3.59439729012826),
  b2 = c(2.97740164667956, 9.15425525531704, 8.8711620055864, 15.8796546096696,
         6.38408271230076, 7.75390405177723, 4.19757986153328, 11.5262186862213,
         11.7077337821047, -3.93496526088181, 6.43628499888291, 7.41937821392672,
         8.95189681481609, 4.96102319839692),
  b3 = c(0.215259133906838, -6.80092203491361, -6.28299292272102, -2.6374041040685,
         -2.88898640623316, -4.7437593918691, -3.47114621994601, -3.67732906994872,
         -8.01900209992875, 0.778295616745237, -4.42479258463278, -3.67047413166161,
         -4.3690110153741, -3.45113957136089),
  b4 = c(-4.12876132716235, 5.98812150063031, 3.305086186927, -0.473714277627942,
         -0.941621454032359, -0.0360094000464393, -0.765597063510316, -0.412540035305785,
         7.16966998832853, -3.28995320756362, 2.24189533079334, -1.29943383936443,
         4.48767508625716, -0.0081706666205612),
  b5 = c(4.62325198305092, -8.75447713555153, -6.76482305023342, -9.51541648189403,
         -5.29314397137411, -4.86782708552792, -2.62246984556501, -3.11992811897395,
         -9.95600304156053, 3.11093701463339, -5.23520420850557, -3.06081876317566,
         -8.69008412431119, -3.50467264517234),
  b6 = c(-5.40200735093978, 6.90310225877108, 5.76036202616016, 15.7828288477982,
         12.3301146015054, 8.29716263466952, 6.42133275403696, 3.82997728621077,
         18.50801186331, -4.64909593679535, 4.5507791876346, 5.06942854914247,
         14.7817476913949, 7.37694537608467),
  b7 = c(NA, -9.2827872010461, -7.92210191519827, -14.0292249486431,
         -2.2359573780378, -3.54215070853232, -2.41104413992002, -3.88727965517397,
         -12.9097754529739, NA, -6.14048957397528, -5.88467183394799,
         -10.2828090588389, -2.91763599703316)
)

# =========================================================
# KELOMPOK ITEM
# =========================================================
grp_level <- list(
  Level1 = c("B1","B5","B11"),
  Level2 = c("B2","B6","B12"),
  Level3 = c("B3","B7","B9","B13","B14"),
  Level4 = c("B4","B8","B10")
)

grp_bloom_rev <- list(
  C4 = c("B1","B5","B11","B3","B7","B9","B13","B14"),
  C5 = c("B2","B6","B12"),
  C6 = c("B4","B8","B10")
)

# =========================================================
# HELPERS
# =========================================================
cek_items <- function(items, df) {
  miss <- setdiff(items, names(df))
  if (length(miss) > 0) stop("Kolom item tidak ditemukan: ", paste(miss, collapse = ", "))
}

make_pars_list_gpcm <- function(item_params_sub) {
  b_cols <- grep("^b", names(item_params_sub), value = TRUE)
  out <- vector("list", length = nrow(item_params_sub))
  names(out) <- item_params_sub$item
  for (i in seq_len(nrow(item_params_sub))) {
    a <- item_params_sub$a[i]
    bs <- as.numeric(item_params_sub[i, b_cols])
    bs <- bs[!is.na(bs)]
    out[[i]] <- list(a1 = a, d = -a * bs)
  }
  out
}

score_theta_group <- function(df_resp, items, item_params) {
  cek_items(items, df_resp)
  df_sub <- df_resp[, items, drop = FALSE]
  ip_sub <- item_params[item_params$item %in% items, , drop = FALSE]
  pars_list <- make_pars_list_gpcm(ip_sub)
  
  mod <- mirt(
    df_sub, 1, itemtype = "gpcm",
    pars = pars_list,
    technical = list(customTheta = matrix(seq(-4, 4, length = 31), ncol = 1)),
    verbose = FALSE
  )
  th <- fscores(mod, method = "EAP")
  as.numeric(th[,1])
}

kategori_0_100 <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 45, 55, 65, 80, Inf),
    labels = c("Sangat Kurang", "Kurang", "Cukup", "Baik", "Sangat Baik"),
    right = TRUE
  )
}

deskripsi_skor <- function(x) {
  data.frame(
    Statistik = c("Mean", "SD", "Var", "Min", "Max"),
    Nilai = round(c(mean(x, na.rm = TRUE),
                    sd(x, na.rm = TRUE),
                    var(x, na.rm = TRUE),
                    min(x, na.rm = TRUE),
                    max(x, na.rm = TRUE)), 2),
    row.names = NULL
  )
}

rekap_kategori <- function(kat_factor) {
  df <- as.data.frame(table(kat_factor))
  colnames(df) <- c("Kategori", "Jumlah_Responden")
  df$Persen <- paste0(round(df$Jumlah_Responden / sum(df$Jumlah_Responden) * 100, 1), "%")
  df
}

detect_sep <- function(path, n = 8) {
  lines <- readLines(path, n = n, warn = FALSE)
  if (length(lines) == 0) return(list(sep = ";", confident = FALSE))
  count_sep <- sum(str_count(lines, fixed(";")))
  count_com <- sum(str_count(lines, fixed(",")))
  count_tab <- sum(str_count(lines, fixed("\t")))
  counts <- c(";" = count_sep, "," = count_com, "\t" = count_tab)
  best <- names(counts)[which.max(counts)]
  sorted <- sort(counts, decreasing = TRUE)
  confident <- (sorted[1] >= 3) && (sorted[1] >= (sorted[2] + 2))
  list(sep = best, confident = confident, counts = counts)
}

safe_read <- function(path, sep) read.csv(path, sep = sep, check.names = FALSE)

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Pengolahan Data Kemampuan dan Nilai Tes Pengukuran Penalaran Kombinatorial"),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        style="background:#f8f9fa; padding:12px; border-radius:10px; border:1px solid #e5e5e5;",
        fileInput("datafile", "Upload Data Respons Siswa (.csv)", accept = ".csv"),
        tags$small("Kolom pertama boleh ID. Kolom item harus B1–B14."),
        br(),
        radioButtons(
          "sep_mode", "Separator CSV",
          choices = c("Auto-detect (disarankan)" = "auto", "Pilih manual" = "manual"),
          selected = "auto", inline = TRUE
        ),
        conditionalPanel(
          condition = "input.sep_mode == 'manual'",
          selectInput("sep_manual", "Pilih pemisah",
                      choices = c("Titik koma ;"=";", "Koma ,"=",", "Tab"="\t"),
                      selected = ";")
        ),
        actionButton("proses", "Hitung Theta & Profil", class="btn-primary"),
        tags$hr(),
        downloadButton("download_hasil", "⬇️ Download Hasil Lengkap (Excel)"),
        downloadButton("download_rekap_all", "⬇️ Download Semua Rekap (Excel)")
      ),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Total",
                 h4("Deskripsi Skor Total (0–100)"), tableOutput("desc_total"),
                 h4("Ringkasan Kategori Total"), tableOutput("rek_total")
        ),
        tabPanel("Level 1",
                 h4("Deskripsi Skor Level 1 (0–100)"), tableOutput("desc_L1"),
                 h4("Ringkasan Kategori Level 1"), tableOutput("rek_L1")
        ),
        tabPanel("Level 2",
                 h4("Deskripsi Skor Level 2 (0–100)"), tableOutput("desc_L2"),
                 h4("Ringkasan Kategori Level 2"), tableOutput("rek_L2")
        ),
        tabPanel("Level 3",
                 h4("Deskripsi Skor Level 3 (0–100)"), tableOutput("desc_L3"),
                 h4("Ringkasan Kategori Level 3"), tableOutput("rek_L3")
        ),
        tabPanel("Level 4",
                 h4("Deskripsi Skor Level 4 (0–100)"), tableOutput("desc_L4"),
                 h4("Ringkasan Kategori Level 4"), tableOutput("rek_L4")
        ),
        tabPanel("Bloom Rev C4",
                 h4("Deskripsi Skor Bloom Rev C4 (0–100)"), tableOutput("desc_C4"),
                 h4("Ringkasan Kategori Bloom Rev C4"), tableOutput("rek_C4")
        ),
        tabPanel("Bloom Rev C5",
                 h4("Deskripsi Skor Bloom Rev C5 (0–100)"), tableOutput("desc_C5"),
                 h4("Ringkasan Kategori Bloom Rev C5"), tableOutput("rek_C5")
        ),
        tabPanel("Bloom Rev C6",
                 h4("Deskripsi Skor Bloom Rev C6 (0–100)"), tableOutput("desc_C6"),
                 h4("Ringkasan Kategori Bloom Rev C6"), tableOutput("rek_C6")
        ),
        tabPanel("Profil (Baik+Sangat Baik)",
                 h4("Ringkasan Profil Level Kombinatorial (berdasarkan jumlah Baik+Sangat Baik)"),
                 tableOutput("profil_level_good"),
                 h4("Ringkasan Profil Bloom Rev (berdasarkan jumlah Baik+Sangat Baik)"),
                 tableOutput("profil_bloom_good")
        ),
        tabPanel("Hasil Lengkap",
                 h4("Tabel Hasil Lengkap"), tableOutput("hasil_table")
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    hasil = NULL,
    desc = list(),
    rekap = list(),
    profil_level = NULL,
    profil_bloom_rev = NULL
  )
  
  observeEvent(input$proses, {
    req(input$datafile)
    path <- input$datafile$datapath
    
    withProgress(message = "Memproses data...", value = 0, {
      
      incProgress(0.15, detail = "Baca file & deteksi separator")
      
      if (input$sep_mode == "manual") {
        sep_used <- input$sep_manual
      } else {
        det <- detect_sep(path, n = 8)
        sep_used <- det$sep
      }
      
      dat <- tryCatch(safe_read(path, sep_used), error = function(e) NULL)
      
      if (is.null(dat) || ncol(dat) < 2) {
        for (s in c(";", ",", "\t")) {
          dat2 <- tryCatch(safe_read(path, s), error = function(e) NULL)
          if (!is.null(dat2) && ncol(dat2) > 1) { dat <- dat2; sep_used <- s; break }
        }
      }
      
      if (is.null(dat) || ncol(dat) < 2) {
        showNotification("Gagal membaca file. Coba pilih separator manual.", type = "error", duration = 8)
        return()
      }
      
      incProgress(0.30, detail = "Validasi kolom item")
      
      ID <- dat[[1]]
      resp <- dat %>% select(where(is.numeric))
      
      all_items <- unique(c(unlist(grp_level), unlist(grp_bloom_rev)))
      miss <- setdiff(all_items, names(resp))
      if (length(miss) > 0) {
        showNotification(
          paste0("Item tidak ditemukan: ", paste(miss, collapse = ", "),
                 ". Pastikan kolom item B1–B14 dan separator benar."),
          type = "error", duration = 10
        )
        return()
      }
      
      incProgress(0.45, detail = "Hitung theta total & kelompok")
      
      th_total <- score_theta_group(resp, all_items, item_params)
      skor_total <- th_total * 12.5 + 50
      kat_total <- kategori_0_100(skor_total)
      
      th_L1 <- score_theta_group(resp, grp_level$Level1, item_params); skor_L1 <- th_L1*12.5 + 50; kat_L1 <- kategori_0_100(skor_L1)
      th_L2 <- score_theta_group(resp, grp_level$Level2, item_params); skor_L2 <- th_L2*12.5 + 50; kat_L2 <- kategori_0_100(skor_L2)
      th_L3 <- score_theta_group(resp, grp_level$Level3, item_params); skor_L3 <- th_L3*12.5 + 50; kat_L3 <- kategori_0_100(skor_L3)
      th_L4 <- score_theta_group(resp, grp_level$Level4, item_params); skor_L4 <- th_L4*12.5 + 50; kat_L4 <- kategori_0_100(skor_L4)
      
      th_C4 <- score_theta_group(resp, grp_bloom_rev$C4, item_params); skor_C4 <- th_C4*12.5 + 50; kat_C4 <- kategori_0_100(skor_C4)
      th_C5 <- score_theta_group(resp, grp_bloom_rev$C5, item_params); skor_C5 <- th_C5*12.5 + 50; kat_C5 <- kategori_0_100(skor_C5)
      th_C6 <- score_theta_group(resp, grp_bloom_rev$C6, item_params); skor_C6 <- th_C6*12.5 + 50; kat_C6 <- kategori_0_100(skor_C6)
      
      incProgress(0.70, detail = "Susun hasil, deskripsi, rekap")
      
      out <- data.frame(
        ID = ID,
        
        Theta_Total = round(th_total, 3),
        Skor_Total_0_100 = round(skor_total, 2),
        Kategori_Total = kat_total,
        
        Theta_Level1 = round(th_L1, 3), Skor_Level1_0_100 = round(skor_L1, 2), Kategori_Level1 = kat_L1,
        Theta_Level2 = round(th_L2, 3), Skor_Level2_0_100 = round(skor_L2, 2), Kategori_Level2 = kat_L2,
        Theta_Level3 = round(th_L3, 3), Skor_Level3_0_100 = round(skor_L3, 2), Kategori_Level3 = kat_L3,
        Theta_Level4 = round(th_L4, 3), Skor_Level4_0_100 = round(skor_L4, 2), Kategori_Level4 = kat_L4,
        
        Theta_BloomRev_C4 = round(th_C4, 3), Skor_BloomRev_C4_0_100 = round(skor_C4, 2), Kategori_BloomRev_C4 = kat_C4,
        Theta_BloomRev_C5 = round(th_C5, 3), Skor_BloomRev_C5_0_100 = round(skor_C5, 2), Kategori_BloomRev_C5 = kat_C5,
        Theta_BloomRev_C6 = round(th_C6, 3), Skor_BloomRev_C6_0_100 = round(skor_C6, 2), Kategori_BloomRev_C6 = kat_C6
      )
      
      rv$desc <- list(
        total = deskripsi_skor(out$Skor_Total_0_100),
        L1 = deskripsi_skor(out$Skor_Level1_0_100),
        L2 = deskripsi_skor(out$Skor_Level2_0_100),
        L3 = deskripsi_skor(out$Skor_Level3_0_100),
        L4 = deskripsi_skor(out$Skor_Level4_0_100),
        C4 = deskripsi_skor(out$Skor_BloomRev_C4_0_100),
        C5 = deskripsi_skor(out$Skor_BloomRev_C5_0_100),
        C6 = deskripsi_skor(out$Skor_BloomRev_C6_0_100)
      )
      
      rv$rekap <- list(
        total = rekap_kategori(out$Kategori_Total),
        L1 = rekap_kategori(out$Kategori_Level1),
        L2 = rekap_kategori(out$Kategori_Level2),
        L3 = rekap_kategori(out$Kategori_Level3),
        L4 = rekap_kategori(out$Kategori_Level4),
        C4 = rekap_kategori(out$Kategori_BloomRev_C4),
        C5 = rekap_kategori(out$Kategori_BloomRev_C5),
        C6 = rekap_kategori(out$Kategori_BloomRev_C6)
      )
      
      good_levels <- data.frame(
        Kelompok = c("Level1","Level2","Level3","Level4"),
        Jumlah_Baik_SangatBaik = c(
          sum(out$Kategori_Level1 %in% c("Baik","Sangat Baik")),
          sum(out$Kategori_Level2 %in% c("Baik","Sangat Baik")),
          sum(out$Kategori_Level3 %in% c("Baik","Sangat Baik")),
          sum(out$Kategori_Level4 %in% c("Baik","Sangat Baik"))
        )
      )
      good_levels$Persen_Baik_SangatBaik <- paste0(round(good_levels$Jumlah_Baik_SangatBaik / nrow(out) * 100, 1), "%")
      rv$profil_level <- good_levels
      
      good_bloom_rev <- data.frame(
        Kelompok = c("Bloom Rev C4","Bloom Rev C5","Bloom Rev C6"),
        Jumlah_Baik_SangatBaik = c(
          sum(out$Kategori_BloomRev_C4 %in% c("Baik","Sangat Baik")),
          sum(out$Kategori_BloomRev_C5 %in% c("Baik","Sangat Baik")),
          sum(out$Kategori_BloomRev_C6 %in% c("Baik","Sangat Baik"))
        )
      )
      good_bloom_rev$Persen_Baik_SangatBaik <- paste0(round(good_bloom_rev$Jumlah_Baik_SangatBaik / nrow(out) * 100, 1), "%")
      rv$profil_bloom_rev <- good_bloom_rev
      
      rv$hasil <- out
      
      incProgress(1, detail = "Selesai")
      showNotification("Perhitungan selesai ✅", type = "message", duration = 4)
    })
  })
  
  # OUTPUTS
  output$hasil_table <- renderTable({ rv$hasil })
  
  output$desc_total <- renderTable({ rv$desc$total })
  output$desc_L1 <- renderTable({ rv$desc$L1 })
  output$desc_L2 <- renderTable({ rv$desc$L2 })
  output$desc_L3 <- renderTable({ rv$desc$L3 })
  output$desc_L4 <- renderTable({ rv$desc$L4 })
  output$desc_C4 <- renderTable({ rv$desc$C4 })
  output$desc_C5 <- renderTable({ rv$desc$C5 })
  output$desc_C6 <- renderTable({ rv$desc$C6 })
  
  output$rek_total <- renderTable({ rv$rekap$total })
  output$rek_L1 <- renderTable({ rv$rekap$L1 })
  output$rek_L2 <- renderTable({ rv$rekap$L2 })
  output$rek_L3 <- renderTable({ rv$rekap$L3 })
  output$rek_L4 <- renderTable({ rv$rekap$L4 })
  output$rek_C4 <- renderTable({ rv$rekap$C4 })
  output$rek_C5 <- renderTable({ rv$rekap$C5 })
  output$rek_C6 <- renderTable({ rv$rekap$C6 })
  
  output$profil_level_good <- renderTable({ rv$profil_level })
  output$profil_bloom_good <- renderTable({ rv$profil_bloom_rev })
  
  # DOWNLOADS
  output$download_hasil <- downloadHandler(
    filename = function() paste0("hasil_lengkap_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$hasil)
      write.xlsx(rv$hasil, file)
    }
  )
  
  output$download_rekap_all <- downloadHandler(
    filename = function() paste0("rekap_semua_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$hasil)
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Deskripsi_Total"); writeData(wb, "Deskripsi_Total", rv$desc$total)
      addWorksheet(wb, "Kategori_Total");  writeData(wb, "Kategori_Total",  rv$rekap$total)
      
      addWorksheet(wb, "Deskripsi_L1"); writeData(wb, "Deskripsi_L1", rv$desc$L1)
      addWorksheet(wb, "Kategori_L1");  writeData(wb, "Kategori_L1",  rv$rekap$L1)
      
      addWorksheet(wb, "Deskripsi_L2"); writeData(wb, "Deskripsi_L2", rv$desc$L2)
      addWorksheet(wb, "Kategori_L2");  writeData(wb, "Kategori_L2",  rv$rekap$L2)
      
      addWorksheet(wb, "Deskripsi_L3"); writeData(wb, "Deskripsi_L3", rv$desc$L3)
      addWorksheet(wb, "Kategori_L3");  writeData(wb, "Kategori_L3",  rv$rekap$L3)
      
      addWorksheet(wb, "Deskripsi_L4"); writeData(wb, "Deskripsi_L4", rv$desc$L4)
      addWorksheet(wb, "Kategori_L4");  writeData(wb, "Kategori_L4",  rv$rekap$L4)
      
      addWorksheet(wb, "Deskripsi_BloomRev_C4"); writeData(wb, "Deskripsi_BloomRev_C4", rv$desc$C4)
      addWorksheet(wb, "Kategori_BloomRev_C4");  writeData(wb, "Kategori_BloomRev_C4",  rv$rekap$C4)
      
      addWorksheet(wb, "Deskripsi_BloomRev_C5"); writeData(wb, "Deskripsi_BloomRev_C5", rv$desc$C5)
      addWorksheet(wb, "Kategori_BloomRev_C5");  writeData(wb, "Kategori_BloomRev_C5",  rv$rekap$C5)
      
      addWorksheet(wb, "Deskripsi_BloomRev_C6"); writeData(wb, "Deskripsi_BloomRev_C6", rv$desc$C6)
      addWorksheet(wb, "Kategori_BloomRev_C6");  writeData(wb, "Kategori_BloomRev_C6",  rv$rekap$C6)
      
      addWorksheet(wb, "Profil_Level_Good");      writeData(wb, "Profil_Level_Good", rv$profil_level)
      addWorksheet(wb, "Profil_BloomRev_Good");   writeData(wb, "Profil_BloomRev_Good", rv$profil_bloom_rev)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
