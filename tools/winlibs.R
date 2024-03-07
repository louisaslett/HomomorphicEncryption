if(!file.exists("../windows/include/flint/fmpz_polyxx.h") || !file.exists("../config.h")) {
  # Windows does not have malloc_stats() in malloc.h, so create config.h accordingly
  file.create("../config.h")
  #writeLines("#define HAVE_MALLOC_H", "../config.h")
  # Grab the missing static library
  unlink("../windows", recursive = TRUE)
  url <- "https://www.louisaslett.com/fhe/flint-2.9.0-win-x86.tar.gz"
  download.file(url, basename(url), quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  untar(basename(url), exdir = "../windows", tar = 'internal')
  unlink(basename(url))
  setwd("../windows")
  #file.rename(list.files(), 'flint')
}
