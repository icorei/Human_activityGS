require(readxl)
## Load required packages
library(bibliometrix) #the library for bibliometrics

## Set up working environment (customize accordingly...)
script.dir <- "~/proyectos/IVIC/the-big-picture"
work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)



setwd(script.dir)
load(sprintf("%s/ISI-20191211.rda"))
load(sprintf("%s/ISI-20200409.rda"))

 table(unique(ISI20200409.df$UT) %in% ISI20191211.df$UT)
 table(unique(ISI20191211.df$UT) %in% ISI20200409.df$UT)

search.keys <- rbind(with(ISI20191211.df,data.frame(UT,search.group)), with(ISI20200409.df,data.frame(UT,search.group)))


ISI.search.df <- duplicatedMatching(rbind(ISI20200409.df,ISI20191211.df), Field = "UT", tol = 0.95)



tt <- table(search.keys$UT,search.keys$search.group)

tt <- tt[match(ISI.search.df$UT,row.names(tt)),]

head(tt)
head(ISI.search.df$UT)


ISI.search.df$search.group <- "CP"
ISI.search.df$search.group[tt[,"cameratrap"]>0] <- "CT"
ISI.search.df$search.group[rowSums(tt[,grep("^ct",colnames(tt))])>0] <- "CT"

ISI.search.df$cons.kwd <- "NO"
ISI.search.df$cons.kwd[rowSums(tt[,!colnames(tt)  %in% "cameratrap"])>0] <- "YES"

table(ISI.search.df$cons.kwd,ISI.search.df$search.group)

rda.arch <- sprintf("%s/ISI-search-df.rda",Rdata.dir)

  save(file=rda.arch,ISI.search.df,search.keys,tt)





M <- biblioAnalysis(ISI.search.df, sep = ";")
summary(object=M,k=20,pause=FALSE)

tail(rev(sort(table(unlist(strsplit(ISI.search.df$DE,";"))))),50)


CT.review.Izza <- read_xlsx(sprintf("%s/documents/CTresearch_60s-2016.xlsx",script.dir),sheet=3)

table(is.na(CT.review.Izza$doi) ,CT.review.Izza$doi %in% ISI.search.df$DI)

table(is.na(CT.review.Izza$TI) ,CT.review.Izza$TI %in% ISI.search.df$TI)
