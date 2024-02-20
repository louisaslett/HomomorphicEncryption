if(!file.exists("../windows/flint/include/flint/fmpz_polyxx.h")){
  unlink("../windows", recursive = TRUE)
  url <- "https://www.louisaslett.com/HomomorphicEncryption/deps/libflint-win-x86.tar.gz"
  download.file(url, basename(url), quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  untar(basename(url), exdir = "../windows", tar = 'internal')
  unlink(basename(url))
  setwd("../windows")
  file.rename(list.files(), 'flint')
}
