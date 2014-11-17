require(rmarkdown)
render(file.path(getwd(), "WSERLottery.Rmd"), output_format="ioslides_presentation")
file.rename(file.path(getwd(), "WSERLottery.html"), file.path(getwd(), "index.html"))
