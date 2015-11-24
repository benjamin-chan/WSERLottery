if (!require(parallel)) {install.packages("parallel", dependencies=TRUE)}
if (!require(doParallel)) {install.packages("doParallel", dependencies=TRUE)}
if (!require(data.table)) {install.packages("data.table", dependencies=TRUE)}
if (!require(reshape2)) {install.packages("reshape2", dependencies=TRUE)}
if (!require(ggplot2)) {install.packages("ggplot2", dependencies=TRUE)}
if (!require(xtable)) {install.packages("xtable", dependencies=TRUE)}
if (!require(knitr)) {install.packages("knitr", dependencies=TRUE)}
if (!require(rmarkdown)) {install.packages("rmarkdown", dependencies=TRUE)}
require(rmarkdown)
render(paste(getwd(), "WSERLottery.Rmd", sep="/"), output_format="ioslides_presentation")
file.rename(paste(getwd(), "WSERLottery.html", sep="/"), paste(getwd(), "index.html", sep="/"))
