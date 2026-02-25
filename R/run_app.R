#' Jalankan Aplikasi Pengolahan Skor Kombinatorial
#'
#' Fungsi ini membuka aplikasi Shiny untuk mengolah data siswa berdasarkan model IRT GPCM.
#' @export
run_kombinatorial_app <- function() {
  appDir <- system.file("app", package = "PenKomFit")
  if (appDir == "" || !dir.exists(appDir)) {
    stop("Folder app tidak ditemukan. Pastikan file ada di inst/app/ dalam package.")
  }
  shiny::runApp(appDir, display.mode = "normal")
}
