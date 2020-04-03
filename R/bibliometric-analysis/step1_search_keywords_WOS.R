#! R --vanilla

## Load required packages
library(bibliometrix) #the library for bibliometrics

## Set up working environment
## FOR ADA:
script.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture"
## FOR JR:
script.dir <- "~/proyectos/IVIC/the-big-picture"

work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)

# data dir: results from search on 2019-12-11
# cameratrap: search term "camera trap"
# consplan: search terms "conservation planning" and "terrestrial"
data.dir <- sprintf("%s/data/ISI-20191211", script.dir)

setwd(work.dir)

#Use saved search results from Web of Science database

# loop bib-files in directory
for (arch in dir(data.dir,pattern=".bib",full.names=T)) {
  # read file and transform to data frame
  A0 <- readFiles(arch)
  M0 <- convert2df(A0, dbsource = "isi", format = "bibtex")
  # add a column with the topic (taken from file name)
  M0$my.topic <- gsub("savedrecs_|[0-9].bib", "", basename(arch))
  # bind data frames by rows
  if (!exists("my.data")) {
    my.data <- M0
  } else {
    my.data <- rbind(my.data,M0)
  }
  # clean-up
  rm(M0)
}

# Check resulting object
dim(my.data)
table(my.data$my.topic)
#2469   42

results_M <- biblioAnalysis(my.data, sep = ";")
summary(object=results_M,k=20,pause=FALSE)



## Source code to run web of science searches through the clarivate APIs
## https://www.programmableweb.com/api/clarivate-web-science-expanded
## https://clarivate.com/webofsciencegroup/solutions/xml-and-apis/

## to do...
