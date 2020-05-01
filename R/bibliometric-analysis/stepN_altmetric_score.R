#! R --vanilla --args ~/proyectos/IVIC/the-big-picture


## need to set up three variables
## read command arguments
args <- commandArgs(TRUE)
if (!exists("script.dir")) {
  if (!is.na(args[1])) {
    script.dir <- args[1]
  } else {
    script.dir <- readline(prompt="Enter path to script directory: ")
  }
}
##https://github.com/ropensci/rcrossref
##~/.Renviron con la línea crossref_email= "mi.correo@electroni.co"
##https://github.com/ropensci/rAltmetric
require(rAltmetric)


library(bibliometrix) #the library for bibliometrics
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes
library(tidytext)

## Set up working environment (customize accordingly...)
work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

## Load data from previous step:
(load(file=sprintf("%s/ISI-CT-corpus.rda",Rdata.dir)))
(load(file=sprintf("%s/ISI-CP-corpus.rda",Rdata.dir)))
(load(file=sprintf("%s/ISI-search-df.rda",Rdata.dir)))

library(magrittr)
library(purrr)
ids <- unique(ISI.search.df$DI)
ids <- list(c(subset(ids,!is.na(ids))))

alm <- function(x)  {
  m <- try(altmetrics(doi = x))
  if (!any(class(m) %in% "try-error"))
  m %>% altmetric_data()
}

results <- pmap_df(ids, alm)

qry <- try(altmetrics(doi=midoi,apikey=mi.key))





require(textcat)
require(RJSONIO)
require(rcrossref)
require(xml2)
##require(dplyr)
require(tidyverse)
require(gdata)


mi.key <- ""
