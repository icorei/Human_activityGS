require(readxl)
setwd("~/proyectos/IVIC/the-big-picture")
load("Rdata/ISI-20191211.rda")
load("Rdata/ISI-20200409.rda")

 table(unique(ISI20200409.df$UT) %in% ISI20191211.df$UT)
 table(unique(ISI20191211.df$UT) %in% ISI20200409.df$UT)

search.keys <- rbind(with(ISI20191211.df,data.frame(UT,search.group)), with(ISI20200409.df,data.frame(UT,search.group)))


ISI.search.df <- duplicatedMatching(rbind(ISI20200409.df,ISI20191211.df), Field = "UT", tol = 0.95)


M <- biblioAnalysis(ISI.search.df, sep = ";")
summary(object=M,k=20,pause=FALSE)

tail(rev(sort(table(unlist(strsplit(ISI.search.df$DE,";"))))),50)


CT.review.Izza <- read_xlsx(sprintf("%s/documents/CTresearch_60s-2016.xlsx",script.dir),sheet=3)

table(is.na(CT.review.Izza$doi) ,CT.review.Izza$doi %in% ISI.search.df$DI)

table(is.na(CT.review.Izza$TI) ,CT.review.Izza$TI %in% ISI.search.df$TI)
