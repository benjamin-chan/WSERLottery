setwd("~/Projects/WSERLottery")
library(checkpoint)
checkpoint("2017-11-01", use.knitr = TRUE)
library(knitr)
library(rmarkdown)
knit("WSERLottery.Rmd", output = "README.md")
