#! /usr/bin/Rscript --vanilla --default-packages=base,stats,utils
library(knitr)
library(rmarkdown)
file <- list.files(pattern='.Rmd')
rmarkdown::render(file)


