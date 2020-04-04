#! R --vanilla

## Load required packages
library(bibliometrix) #the library for bibliometrics

## Set up working environment (customize accordingly...)
## FOR ADA:
script.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture"
## FOR JR:
script.dir <- "~/proyectos/IVIC/the-big-picture"

work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

#Use saved search results from Web of Science database

# data dir: results from search on 2019-12-11
# cameratrap: search term "camera trap"
# consplan: search terms "conservation planning" and "terrestrial"
data.dir <- sprintf("%s/data/ISI-20191211", script.dir)

# loop bib-files in directory
for (arch in dir(data.dir,pattern=".bib",full.names=T)) {
  # read file and transform to data frame
  A0 <- readFiles(arch)
  M0 <- convert2df(A0, dbsource = "isi", format = "bibtex")
  # add a column with the topic (taken from file name)
  M0$my.topic <- gsub("savedrecs_|[0-9].bib", "", basename(arch))
  # bind data frames by rows
  if (!exists("ISI.search.df")) {
    ISI.search.df <- M0
  } else {
    ISI.search.df <- rbind(ISI.search.df,M0)
  }
  # clean-up
  rm(M0)
}

# Check resulting object
dim(ISI.search.df)
table(ISI.search.df$my.topic)
#2469   42

results_M <- biblioAnalysis(ISI.search.df, sep = ";")
summary(object=results_M,k=20,pause=FALSE)

## save to a Rdata object:
save(file=sprintf("%s/ISI-20191211.rda",Rdata.dir),ISI.search.df)

## That's it!, we are ready for the next step.
