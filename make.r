require(rmarkdown)
path <- "Users\\chanb\\Documents\\GitHub Repositories\\WSERLottery\\"
if (Sys.info()["nodename"] == "CHSE") {
  path <- paste0("\\\\tsclient\\C\\", path)
} else if (Sys.info()["nodename"] == "GHBA299") {
  path <- paste0("C:\\", path)
} else if (Sys.info()["nodename"] == "FAMILYPC") {
  path <- "C:\\Users\\Ben\\Documents\\GitHub repositories\\WSERLottery\\"
} else if (Sys.info()["nodename"] == "MAMAYAGA") {
  path <- "C:\\Users\\Cat Buckley\\Documents\\GitHub\\WSERLottery\\"
}
render(paste0(path, "WSERLottery.Rmd"), output_format="ioslides_presentation")
file.rename(paste0(path, "WSERLottery.html"), paste0(path, "index.html"))
