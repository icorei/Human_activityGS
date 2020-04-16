require(rcrossref)
require(readxl)
setwd("~/proyectos/IVIC/the-big-picture")
load("Rdata/ISI-20191211.rda")
load("Rdata/ISI-20200409.rda")

## remember to set this in ~/.Renviron
## crossref_email = valid@email
readRenviron("~/.Renviron")


## track cameratrap references from conservation planning references:

## start with all dois
dois <- unique(c(tolower(ISI20191211.df$DI),tolower(ISI20200409.df$DI)))
dois <- subset(dois,!is.na(dois))

lks <- data.frame()
doi.search <- data.frame()

for (origin in dois) {
   if (!origin %in% doi.search$doi) {
      q1 <- cr_works(doi=origin)
      doi.search <- rbind(doi.search,data.frame(
            doi=origin,cited.refs=as.numeric(q1$data$reference.count)))

      if ("reference" %in% colnames(q1$data)) {
         dref <- unique(q1$data$reference[[1]]$DOI)
         lks <- rbind(lks, data.frame(l=origin, k=subset(dref,!is.na(dref))))

         search.refs <- subset(dref,!is.na(dref) & !(dref %in% dois) & !(dref %in% lks$l) & !(dref %in% doi.search$doi))

         if(length(search.refs)>0) {
            q2 <- cr_works(doi=search.refs,.progress="text")
            dts <- subset(q2$data,!is.na(doi))

            for (k in 1:nrow(dts)) {
               dref <- dts$doi[k]
               doi.search <- rbind(doi.search,data.frame(
               doi=dref,cited.refs=as.numeric(dts$reference.count[k])))

               if (!is.null(dts$reference[[k]])) {
                  if ("DOI" %in% colnames( dts$reference[[k]])) {
                     iref <- dts$reference[[k]]$DOI
                     lks <- rbind(lks, data.frame(l=dref, k=subset(iref,!is.na(iref))))
                  }
               }
            }
         }
      }
      lks <- unique(lks)
      doi.search <- unique(doi.search)
       save(file="Rdata/CR-20200416.rda",lks ,doi.search)
   }
}

 (load(file="Rdata/CR-20200416.rda"))
 table(unique(doi.search$doi) %in% lks$l)
dim(unique(lks))
length(unique(lks$l))
length(unique(lks$k))

q1 <- cr_works(doi=dois[3],.progress="text")

l2 <- unique(unlist(lapply(q2$data$reference,function(x) x$DOI)))
lks <- rbind(lks, data.frame(l=dois[3], k=subset(l1,!is.na(l1))))



q1 <- cr_works(doi=dois[1],.progress="text")



qry <- cr_works(doi=dois,.progress="text")

ref.dois <- unique(unlist(lapply(qry$data$reference,function(x) x$DOI)))
ref.dois <- subset(ref.dois,!is.na(ref.dois) & !(ref.dois %in% dois))
qry <- cr_works(doi=ref.dois,.progress="text")
