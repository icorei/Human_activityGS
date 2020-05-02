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
region_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/region.liwc",script.dir), format = "LIWC")
taxonomic_thesaurus <- dictionary(file = sprintf("%s/dict/liwc/taxonomic.liwc",script.dir), format = "LIWC")


## Apply dictionaries to clean up the vocabulary
CT.bigram <- tokens_select(CT.bigram, exclude.words, selection = 'remove')
##CT.bigram <- tokens_select(CT.bigram, species.words, selection = 'remove')
##CT.bigram <- tokens_select(CT.bigram, regions.words, selection = 'remove')

CT.dfm <- dfm(CT.bigram, thesaurus = conservation_thesaurus)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = status_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = threats_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = habitat_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = interaction_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = taxonomic_thesaurus,exclusive=FALSE)
CT.dfm <- dfm_lookup(CT.dfm,dictionary = region_thesaurus,exclusive=FALSE)


CP.bigram <- tokens_select(CP.bigram, exclude.words, selection = 'remove')

CP.dfm <- dfm(CP.bigram, thesaurus = conservation_thesaurus)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = status_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = threats_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = habitat_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = interaction_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = taxonomic_thesaurus,exclusive=FALSE)
CP.dfm <- dfm_lookup(CP.dfm,dictionary = region_thesaurus,exclusive=FALSE)

##tmp.dfm <- dfm_remove(tmp.dfm, pattern = taxonomic_thesaurus)
##tmp.dfm <- dfm_remove(tmp.dfm, pattern = region_thesaurus)

CT.dfm

## create document term matrix for topic model analysis using topics with at least 20 occurrences
CT.dfm <- dfm_trim(CT.dfm, min_termfreq = 20)

CP.dfm <- dfm_trim(CP.dfm, min_termfreq = 20)


CT.dtm <- convert(CT.dfm, to = "topicmodels")

CP.dtm <- convert(CP.dfm, to = "topicmodels")


## Search for an optimal number of topics for the models

if (file.exists(sprintf("%s/Topic-number-results.rda",Rdata.dir))) {
  load(file=sprintf("%s/Topic-number-results.rda",Rdata.dir))
} else {
  ## how many cores can we use?
  ## on mac osx use: sysctl -n hw.ncpu; on Linux: nproc
  optimTopic.CT <- FindTopicsNumber(
    CT.dtm,
    topics = seq(from = 5, to = 150, by = 5),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 6,
    verbose = TRUE
  )
  optimTopic.CP <- FindTopicsNumber(
    CP.dtm,
    topics = seq(from = 5, to = 150, by = 5),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 5,
    verbose = TRUE
  )
  save(file=sprintf("%s/Topic-number-results.rda",Rdata.dir), optimTopic.CT, optimTopic.CP)
}

## the answer to the question of the universe, life, etc is...
FindTopicsNumber_plot(optimTopic.CP)
FindTopicsNumber_plot(optimTopic.CT)

## Now fit the models
CT.lda <- LDA(CT.dtm, control=list(seed=0), k = 45)

CP.lda <- LDA(CP.dtm, control=list(seed=0), k = 45)

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


CP.top_terms <- CP.topics %>%
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

## This is a table for the Appendix...
## some topics do not have any term above 0.1
## maybe topics can be classified as "dominated by one term", "codominated by n terms", "heterogeneous topics"
CT.top_terms %>% filter(beta>.05) %>% print.AsIs()
CP.top_terms %>% filter(beta>.05) %>% print.AsIs()


save(file=sprintf("%s/ISI-lda.rda",Rdata.dir),CT.lda,CP.lda)







####
## wvwvwvwv old code below wvwvwvwv
####






##topic.names <- c("Frst cons","Bio inter","Pop mngmnt","Behaviour","Diversity","PAs","Occu","Density","Hum imp")

tt <- topics(CT.lda)

aggregate(ISI.search.df$cons.kwd[match(names(tt),ISI.search.df$UT)] %in% "YES",list(tt),mean)
hist(aggregate(ISI.search.df$cons.kwd[match(names(tt),ISI.search.df$UT)] %in% "YES",list(tt),mean)$x)
subset(aggregate(ISI.search.df$cons.kwd[match(names(tt),ISI.search.df$UT)] %in% "YES",list(tt),mean),x>.4)

CT.top_terms %>% filter(topic %in% c(11) & beta>.1)
CT.top_terms %>% filter(topic %in% c(36) & beta>.1)
CT.top_terms %>% filter(topic %in% c(37,41) & beta>.1)


## times cited per topic
boxplot(log1p(ISI.search.df$TC[match(names(tt),ISI.search.df$UT)])~tt)

CT.beta <- CT.lda@beta
rownames(CT.beta) <- sprintf("CT%02d",1:nrow(CT.beta))

CT.dist <- dist(CT.beta, method="euclidean")
CT.clus <- hclust(CT.dist)

CP.beta <- CP.lda@beta
rownames(CP.beta) <- sprintf("CP%02d",1:nrow(CP.beta))

CP.dist <- dist(CP.beta, method="euclidean")
CP.clus <- hclust(CP.dist)

## cluster
plot(CT.clus, cex = 1)

## main topic
tt <- topics(CT.lda,k=5)

## but more than one topic per paper is likely...
tt <- topics(CT.lda,threshold=2/45)
## we can also consider here papers "focused on one topic", "connecting n topics", and "multi-topic or heterogeneous" papers

## this show how many topics per paper...
hist(unlist(lapply(tt,length)))



lda_gamma <- tidy(CT.lda, matrix = "gamma")
lda_gamma

tt <- with(lda_gamma,tapply(gamma,list(document,topic),mean))
d0 <- dist(tt)
h0 <- hclust(d0)

## image showing clusters of papers per topic
image(tt[h0$order,CT.clus$order])
image(tt[h0$order,CT.clus$order]>2/45)

## weight of topic in the whole group, this could be partitioned by year or other subgrouping to show differences...
barplot(colSums(tt))


## this is not working well since
require(vegan)
mi.pca1<- rda(tt)
mi.pca2 <- capscale(d0~1)





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
