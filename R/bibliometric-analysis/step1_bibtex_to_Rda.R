#! R --vanilla

## read command arguments
args <- commandArgs(TRUE)
script.dir <- args[1]
target.dir <- args[2]

## Load required packages
library(bibliometrix) #the library for bibliometrics

## Set up working environment (customize accordingly...)
work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

data.dir <- sprintf("%s/%s", script.dir,target.dir)
rda.arch <- sprintf("%s/%s.rda",Rdata.dir,basename(target.dir))

if (file.exists(rda.arch)) {
  cat(sprintf("File %s already exists, ",rda.arch))
} else {
  cat(sprintf("Loop through bibtex files in directory %s\n",target.dir))
  for (arch in dir(data.dir,pattern=".bib",full.names=T)) {
    # read file and transform to data frame
    A0 <- readFiles(arch)
    M0 <- convert2df(A0, dbsource = "isi", format = "bibtex")
    # add a column with the topic (taken from file name)
    M0$search.group <- gsub("savedrecs_|[0-9].bib", "", basename(arch))
    # bind data frames by rows
    if (!exists("ISI.search.df")) {
      ISI.search.df <- M0
    } else {
      ISI.search.df <- rbind(ISI.search.df,M0)
    }
    # clean-up
    rm(M0)
  }
  cat(sprintf("Check resulting data frame...\n "))
  dim(ISI.search.df)
  table(ISI.search.df$search.group)

  cat(sprintf("Saving to rda file %s\n",rda.arch))

  save(file=rda.arch,ISI.search.df)
}

cat(sprintf("my job here is done!\n"))

## That's it!, we are ready for the next step.
