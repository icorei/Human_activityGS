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

## Load required packages

library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes
library(tidytext)
library("ldatuning")

## Set up working environment (customize accordingly...)
work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

## Load data from previous step:
(load(file=sprintf("%s/ISI-CT-corpus.rda",Rdata.dir)))
(load(file=sprintf("%s/ISI-CP-corpus.rda",Rdata.dir)))
(load(file=sprintf("%s/ISI-search-df.rda",Rdata.dir)))


## direct citations
cts <- data.frame()
for (k in 1:nrow(ISI.search.df)) {
  l <- grep(ISI.search.df$SR[k],ISI.search.df$CR)
  if (length(l)>0)
    cts <- rbind(cts,data.frame(k,l))
}
table(ISI.search.df[cts$l,"search.group"],
ISI.search.df[cts$k,"search.group"])

ISI.search.cts <- cts
save(file=sprintf("%s/ISI-citation-pairs.rda",Rdata.dir), ISI.search.cts)


slc <- subset(cts,(ISI.search.df[cts$l,"search.group"] %in% "CT") & (ISI.search.df[cts$k,"search.group"] %in% "CP"))
oslc <- rev(sort(table(slc$l)))

output.arch <- sprintf("%s/R/manual-anotations/README.md",script.dir)
cat(file=output.arch,
  sprintf("|Camera trap studies|cited in Cons. Plan.|Assigned to|\n|---|---|---|\n"))

#paste(rep(c("ADA","ADA",NA),45), #
#  rep(c(NA,"JR","JR"),45)[1:nrow(oslc)],
# rep(c("IZZA",NA,"IZZA"),45)[1:nrow(oslc)])
quien <- rep(c("ADA / JR","ADA / IZZA","IZZA / JR"),45)[1:nrow(oslc)]
names(quien) <- names(oslc)

for (k in names(oslc)) {
  cat(file=output.arch,append=T,
sprintf("|[%s](http://doi.org/%s) | %s | %s |\n", ISI.search.df[as.numeric(k),"TI"], ISI.search.df[as.numeric(k),"DI"],oslc[k],quien[k]) )

}



ISI.search.df$CP_nref <- NA
for (k in seq(along=ISI.search.df$SR)[ISI.search.df$search.group %in% "CT"]) {
  ISI.search.df$CP_nref[k] <- sum(grepl(ISI.search.df$SR[k],subset(ISI.search.df,search.group %in% "CP")$CR))
}
