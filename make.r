library(checkpoint)
checkpoint("2016-10-01", use.knitr = TRUE)
library(knitr)
library(rmarkdown)
knit("WSERLottery.Rmd", output = "README.md")
