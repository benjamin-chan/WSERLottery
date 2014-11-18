require(rmarkdown)
path <- "Users\\chanb\\Documents\\GitHub Repositories\\WSERLottery\\"
if (Sys.info()["nodename"] == "CHSE") {
  path <- paste0("\\\\tsclient\\C\\", path)
} else {
  path <- paste0("C:\\", path)
}
render(paste0(path, "WSERLottery.Rmd"), output_format="ioslides_presentation")
file.rename(paste0(path, "WSERLottery.html"), paste0(path, "index.html"))
