library(stringr)
library(tidyverse)
knitr::opts_chunk$set(error=FALSE)
df=read.csv("MPCE_data.csv",header = T)
df$Programme=as.factor(df$Programme)
Programme <- df$Programme
render_one <- function(Programme) {
  # assuming the output format of input.Rmd is PDF
  rmarkdown::render(
    input ='Target_Fixation_template.Rmd',
    output_file = str_c('Analysis_summary',Programme,'.html'),
    output_dir = "output/",
    params = list(Programme=Programme),
    envir = parent.frame()
  )
}
Pname=unique(unlist(Programme, use.names = FALSE))
for (prg in Pname) {
  render_one(prg)
}

