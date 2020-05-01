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


## load dictionaries:

exclude.words <- read.table(file=sprintf("%s/dict/terms/exclude.txt",script.dir),as.is=T)$V1
species.words <- read.table(file=sprintf("%s/dict/terms/species_terms.txt",script.dir),as.is=T)$V1
regions.words <- read.table(file=sprintf("%s/dict/terms/region_terms.txt",script.dir),as.is=T)$V1

# Dictionay of related terms in LIWC format
status_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/status.liwc",script.dir), format = "LIWC")
threats_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/threats.liwc",script.dir), format = "LIWC")
conservation_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/conservation.liwc",script.dir), format = "LIWC")
interaction_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/interaction.liwc",script.dir), format = "LIWC")
habitat_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/habitat.liwc",script.dir), format = "LIWC")
##region_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/region.liwc",script.dir), format = "LIWC")
##taxonomic_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/taxonomic.liwc",script.dir), format = "LIWC")


## use single tokens instead of bigram

CT.dfm <- dfm(CT.tokens, thesaurus = conservation_thesaurus)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = status_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = threats_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = habitat_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = interaction_thesaurus,exclusive=FALSE)

CP.dfm <- dfm(CP.tokens, thesaurus = conservation_thesaurus)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = status_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = threats_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = habitat_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = interaction_thesaurus,exclusive=FALSE)

##tmp.dfm <- dfm_remove(tmp.dfm, pattern = taxonomic_thesaurus)
##tmp.dfm <- dfm_remove(tmp.dfm, pattern = region_thesaurus)

CT.dfm

CT.dfm <- dfm_trim(CT.dfm, min_termfreq = 20)

CP.dfm <- dfm_trim(CP.dfm, min_termfreq = 20)


CT.dtm <- convert(CT.dfm, to = "topicmodels")

CP.dtm <- convert(CP.dfm, to = "topicmodels")


CT.lda <- LDA(CT.dtm, control=list(seed=0), k = 15)

CP.lda <- LDA(CP.dtm, control=list(seed=0), k = 30)

tt <- topics(CT.lda)
docvars(CT.dfm, 'topic') <- tt[match(row.names(CT.dfm),names(tt))]

CT.topics <- tidy(CT.lda, matrix = "beta")

CP.topics <- tidy(CP.lda, matrix = "beta")
CT.topics

CT.top_terms <- CT.topics %>%
group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)

CT.top_terms %>%
mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(term, beta, fill = factor(topic))) +
geom_col(show.legend = FALSE) +
facet_wrap(~ topic, scales = "free") +
coord_flip() +
scale_x_reordered()

topic.names <- c("Frst cons","Bio inter","Pop mngmnt","Behaviour","Diversity","PAs","Occu","Density","Hum imp")

tt <- topics(CT.lda)

aggregate(ISI.search.df$cons.kwd[match(names(tt),ISI.search.df$UT)] %in% "YES",list(tt),mean)

CT.top_terms %>% filter(topic %in% c(11))
CT.top_terms %>% filter(topic %in% c(11,15))
CT.top_terms %>% filter(topic %in% c(17,21))

boxplot(log1p(ISI.search.df$TC[match(names(tt),ISI.search.df$UT)])~tt)

ISI.search.df$CP_nref <- NA
for (k in seq(along=ISI.search.df$SR)[ISI.search.df$search.group %in% "CT"]) {
  ISI.search.df$CP_nref[k] <- sum(grepl(ISI.search.df$SR[k],subset(ISI.search.df,search.group %in% "CP")$CR))
}

CT.beta <- CT.lda@beta
rownames(CT.beta) <- sprintf("CT%02d",1:nrow(CT.beta))

CT.dist <- dist(CT.beta, method="euclidean")
CT.clus <- hclust(CT.dist)

CP.beta <- CP.lda@beta
rownames(CP.beta) <- sprintf("CP%02d",1:nrow(CP.beta))

CP.dist <- dist(CP.beta, method="euclidean")
CP.clus <- hclust(CP.dist)

plot(CT.clus, cex = 1)


tt <- topics(CT.lda)
ISI.search.df[match(names(tt),ISI.search.df$UT),"topic"] <- sprintf("CT%02d",tt)
tt <- topics(CP.lda)
ISI.search.df[match(names(tt),ISI.search.df$UT),"topic"] <- sprintf("CP%02d",tt)

mtz <- table(ISI.search.df[cts$l,"topic"], ISI.search.df[cts$k,"topic"])
mtz <- cbind(mtz,CP04=rep(0,nrow(mtz)))

## in opposite direction, fewer references...
#mtz <- table(ISI.search.df[cts$k,"topic"], ISI.search.df[cts$l,"topic"])
#mtz <- rbind(mtz,CP04=rep(0,ncol(mtz)))

grph <- graph_from_adjacency_matrix(mtz, mode = "directed", weighted = TRUE,  diag = FALSE)

# use heatmap() with a matrix of links and dendrograms of the topics on each group
require(viridis)
clrs <- c(NA,brewer.pal(9,"PuRd"))
#clrs <- viridis(10)

mt2 <- as.matrix(mtz[grep("CT",rownames(mtz)),grep("CP",colnames(mtz))])
mt2[sort(rownames(mt2)),sort(colnames(mt2))]

heatmap(mt2, Rowv=as.dendrogram(CT.clus), Colv=as.dendrogram(CP.clus), col=clrs)

aggregate(ISI.search.df$CP_nref[match(names(tt),ISI.search.df$UT)] > 0 ,list(tt),sum)

lda_gamma <- tidy(CT.lda, matrix = "gamma")
lda_gamma

tt <- with(lda_gamma,tapply(gamma,list(document,topic),mean))

require(vegan)
mi.pca1<- rda(tt)

mtz <- aggregate(tt, by=list(ISI.search.df[match(rownames(tt),ISI.search.df$UT),"PY"]),sum)

mt2 <- decostand(mtz[,-1],1,method="total")
mt2 <- mtz[,-1]
colnames(mt2) <- topic.names

clrs <- c("grey","grey","green","green","green","orange","orange","orange","orange")
mosaicplot(mt2[,c(7,8,2,4,5,1,3,6,9)],main="mosaicplot",las=2,col=clrs)


boxplot(tt[,1]~ISI.search.df[match(rownames(tt),ISI.search.df$UT),"cons.kwd"])


sort(table(unlist(with(ISI.search.df,
  sapply(ID,function(x) trim(strsplit(x,";")[[1]]))))))

  ISI.search.df$TC

  citas <-  ISI.search.df[match(rownames(tt),ISI.search.df$UT),"TC"]


  for (k in seq(along=ISI.search.df$SR)[ISI.search.df$search.group %in% "CT"]) {
    ISI.search.df$CP_nref[k] <- sum(grepl(ISI.search.df$SR[k],subset(ISI.search.df,search.group %in% "CP")$CR))
  }

stem(ISI.search.df$CP_nref)

  #SELECTION of references:
  ##conservation search
  s1 <- ISI.search.df$cons.kwd %in% "YES"
  ##nr citations
  s2 <- ISI.search.df$TC > 20
  ##altmetric score
  s3 <- TRUE
  ## conservation topics
  s4 <- ISI.search.df$UT %in% subset(rownames(tt),rowSums(tt[,c(1,3,6,9)])>1/3)
  ##links to conservation planning literature
  s5 <- ISI.search.df$CP_nref>0
  ##author keywords

  table(s1 & s4,s2 & s3 & s5)

  subset(ISI.search.df,s1 & s2 & s3 & s4 & s5)[,c("TI","DI")]
