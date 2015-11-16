if (!require(parallel)) {install.packages("parallel", dependencies=TRUE)}
if (!require(doParallel)) {install.packages("doParallel", dependencies=TRUE)}
if (!require(data.table)) {install.packages("data.table", dependencies=TRUE)}
if (!require(reshape2)) {install.packages("reshape2", dependencies=TRUE)}
if (!require(ggplot2)) {install.packages("ggplot2", dependencies=TRUE)}
if (!require(xtable)) {install.packages("xtable", dependencies=TRUE)}
if (!require(rmarkdown)) {install.packages("rmarkdown", dependencies=TRUE)}
require(rmarkdown)
if (Sys.info()["nodename"] == "CHSE") {
  path <- paste0("\\\\tsclient\\C\\", path)
} else if (Sys.info()["nodename"] == "GHBA299") {
  path <- paste0("C:\\", path)
} else if (Sys.info()["nodename"] == "FAMILYPC") {
  path <- "C:\\Users\\Ben\\Documents\\GitHub repositories\\WSERLottery\\"
} else if (Sys.info()["nodename"] == "MAMAYAGA") {
  path <- "C:\\Users\\Cat Buckley\\Documents\\GitHub\\WSERLottery\\"
} else {
  path <- "Dropbox/"
}
render(paste0(path, "WSERLottery.Rmd"), output_format="ioslides_presentation")
file.rename(paste0(path, "WSERLottery.html"), paste0(path, "index.html"))
