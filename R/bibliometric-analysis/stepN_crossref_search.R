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
##save(file="Rdata/CR-20200416.rda",list=ls(pattern="qry"))

summary(as.numeric(subset(qry1$data,doi %in% tolower(ISI20191211.df$DI))$score))

## Check if we can use this to track cameratrap references from conservation planning references:
dois <- unique(c(tolower(ISI20191211.df$DI),tolower(ISI20200409.df$DI)))
dois <- subset(dois,!is.na(dois))

lks <- data.frame()

q1 <- cr_works(doi=dois[3],.progress="text")
l1 <- unique(q1$data$reference[[1]]$DOI)
lks <- rbind(lks, data.frame(l=dois[3], k=subset(l1,!is.na(l1))))
l1 <- subset(l1,!is.na(l1) & !(l1 %in% dois) & !(l1 %in% lks$l))

q2 <- cr_works(doi=l1,.progress="text")

l2 <- unique(unlist(lapply(q2$data$reference,function(x) x$DOI)))
lks <- rbind(lks, data.frame(l=dois[3], k=subset(l1,!is.na(l1))))



q1 <- cr_works(doi=dois[1],.progress="text")



qry <- cr_works(doi=dois,.progress="text")

ref.dois <- unique(unlist(lapply(qry$data$reference,function(x) x$DOI)))
ref.dois <- subset(ref.dois,!is.na(ref.dois) & !(ref.dois %in% dois))
qry <- cr_works(doi=ref.dois,.progress="text")
