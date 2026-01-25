library(shiny)
library(shinythemes)
library(mirt)
library(dplyr)
library(openxlsx)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Pengolahan Data Kemampuan dan Nilai Tes Pengukuran Penalaran Kombinatorial"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Data Respons Siswa (.csv)", accept = ".csv"),
      actionButton("proses", "Hitung Theta"),
      downloadButton("download_theta", "⬇️ Download Hasil"),
      downloadButton("download_rekap", "⬇️ Download Rekap")
    ),
    mainPanel(
      h4("Deskripsi Skor 0–100"),
      verbatimTextOutput("deskripsi"),
      h4("Ringkasan Jumlah & Persentase per Kategori"),
      tableOutput("ringkasan"),
      h4("Hasil Theta & Skor Siswa"),
      tableOutput("theta_table")
    )
  )
)

server <- function(input, output, session) {
  dataInput <- reactiveVal()
  hasil_theta <- reactiveVal()

  observeEvent(input$proses, {
    req(input$datafile)
    data <- read.csv(input$datafile$datapath, sep = ";")

    response_matrix <- as.matrix(data[,-1])
    model <- mirt(response_matrix, 1, itemtype = "gpcm", verbose = FALSE)
    fscores <- fscores(model)

    theta_est_df <- data.frame(
      ID = data[[1]],
      Theta = round(fscores[,1], 2)
    )
    theta_est_df$Skor_0_100 <- theta_est_df$Theta * 12.5 + 50
    theta_est_df$Kategori_Penilaian <- cut(
      theta_est_df$Skor_0_100,
      breaks = c(-Inf, 45, 55, 65, 80, Inf),
      labels = c("Sangat Kurang", "Kurang", "Cukup", "Baik", "Sangat Baik"),
      right = TRUE
    )
    hasil_theta(theta_est_df)
  })

  output$theta_table <- renderTable({
    hasil_theta()
  })

  output$deskripsi <- renderPrint({
    df <- hasil_theta()
    if (is.null(df)) return()
    cat("Mean     :", round(mean(df$Skor_0_100), 2), "\n")
    cat("SD       :", round(sd(df$Skor_0_100), 2), "\n")
    cat("Var      :", round(var(df$Skor_0_100), 2), "\n")
    cat("Min      :", round(min(df$Skor_0_100), 2), "\n")
    cat("Max      :", round(max(df$Skor_0_100), 2), "\n")
  })

  output$ringkasan <- renderTable({
    df <- hasil_theta()
    if (is.null(df)) return()
    df %>%
      group_by(Kategori_Penilaian) %>%
      summarise(Jumlah_Responden = n()) %>%
      mutate(Persen = round((Jumlah_Responden / sum(Jumlah_Responden)) * 100, 1),
             Label = paste0(Persen, "%"))
  })

  output$download_theta <- downloadHandler(
    filename = function() {"hasil_theta.xlsx"},
    content = function(file) {
      write.xlsx(hasil_theta(), file)
    }
  )

  output$download_rekap <- downloadHandler(
    filename = function() {"rekap_kategori.xlsx"},
    content = function(file) {
      rekap <- hasil_theta() %>%
        group_by(Kategori_Penilaian) %>%
        summarise(Jumlah_Responden = n()) %>%
        mutate(Persen = round((Jumlah_Responden / sum(Jumlah_Responden)) * 100, 1),
               Label = paste0(Persen, "%"))
      write.xlsx(rekap, file)
    }
  )
}

shinyApp(ui, server)
