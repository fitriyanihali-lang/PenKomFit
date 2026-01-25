#' Jalankan Aplikasi Pengolahan Skor Kombinatorial
#'
#' Fungsi ini akan membuka aplikasi Shiny untuk mengolah data siswa berdasarkan model GPCM.
#' @export
run_kombinatorial_app <- function() {
  appDir <- system.file("app", package = "PenKomFit")
  if (appDir == "") {
    stop("App tidak ditemukan dalam package.")
  }
  shiny::runApp(appDir, display.mode = "normal")
}
