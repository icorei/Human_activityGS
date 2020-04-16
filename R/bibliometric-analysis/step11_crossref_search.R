require(rcrossref)
require(readxl)
setwd("~/proyectos/IVIC/the-big-picture")
load("Rdata/ISI-20191211.rda")
load("Rdata/ISI-20200409.rda")

## remember to set this in ~/.Renviron
## crossref_email = valid@email
readRenviron("~/.Renviron")

search.term.1 <- c("camera trap","remote photography","remote camera","phototrap","automatic photography")

search.term.2 <- c("conservation plan","recovery plan")

search.term.3 <- c("biodiversity", "habitat", "species")
for (k in search.term.1) {
   for (j in search.term.2) {
      for (g in search.term.3) {
         cat(sprintf("%s %s %s:",k,j,g))
         qry <- cr_works(query=paste(k,j,g,sep="+"), filter=c(has_abstract = TRUE), limit = 1000, sort = "score")
         assign(gsub(" ","_",sprintf("qry.%s.%s.%s",k,j,g)),qry)
         rm(qry)
         cat(sprintf("... done!\n"))
      }
   }
}

summary(as.numeric(subset(qry1$data,doi %in% tolower(ISI20191211.df$DI))$score))

## Check if we can use this to track cameratrap references from conservation planning references:
dois <- tolower(ISI20200409.df$DI)
dois <- subset(dois,!is.na(dois))
qry <- cr_works(doi=dois,.progress="text")
