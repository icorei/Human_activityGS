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
require(lubridate)
library(stringr)
library(caret)
library(sna)
library(igraph)
library(dplyr) #for data munging

library(ggplot2)
library(plyr)
library(scales)
library(tidyr)
library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library("RColorBrewer") # user friendly color palettes
require(viridis)
library(tidytext)



## Set up working environment (customize accordingly...)
work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

## Load data from previous step:
(load(file=sprintf("%s/ISI-lda.rda",Rdata.dir)))
(load(file=sprintf("%s/ISI-citation-pairs.rda",Rdata.dir)))
(load(file=sprintf("%s/ISI-search-df.rda",Rdata.dir)))


#######
## which topics are more related...
#######


CT.beta <- CT.lda@beta
rownames(CT.beta) <- sprintf("CT%02d",1:nrow(CT.beta))

CT.dist <- dist(CT.beta, method="euclidean")
CT.clus <- hclust(CT.dist)

CP.beta <- CP.lda@beta
rownames(CP.beta) <- sprintf("CP%02d",1:nrow(CP.beta))

CP.dist <- dist(CP.beta, method="euclidean")
CP.clus <- hclust(CP.dist)

plot(CT.clus)
abline(h=1600)
CT.grp <- cutree(CT.clus,h=1600)
plot(CP.clus)
abline(h=1400)
CP.grp <- cutree(CP.clus,h=1400)

## cluster
plot(CT.clus, cex = 1)


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
CT.top_terms %>% filter(beta>.05) %>% print.AsIs()
CP.top_terms %>% filter(beta>.05) %>% print.AsIs()

  ###########
  ## Direct citations between topics
  ##########

  CT.prd <- topics(CT.lda)
  CP.prd <- topics(CP.lda)


  ISI.search.df[match(names(CT.prd),ISI.search.df$UT),"topic"] <- sprintf("CT%02d",CT.prd)
  ISI.search.df[match(names(CP.prd),ISI.search.df$UT),"topic"] <- sprintf("CP%02d",CP.prd)

  ISI.search.df[ISI.search.df$DT %in% "REVIEW" & ISI.search.df$search.group %in% "CT","topic"] <- "CTRV"
  ISI.search.df[ISI.search.df$DT %in% "REVIEW" & ISI.search.df$search.group %in% "CP","topic"] <- "CPRV"

  ISI.search.df[is.na(ISI.search.df$topic) & ISI.search.df$search.group %in% "CP","topic"] <- "CPNN"
  ISI.search.df[is.na(ISI.search.df$topic) & ISI.search.df$search.group %in% "CT","topic"] <- "CTNN"

  table(ISI.search.df$search.group,ISI.search.df$topic,useNA="always")

  my.topic <- as.factor(ISI.search.df$topic)

  ## direct citations

  (load(file=sprintf("%s/ISI-citation-pairs.rda",Rdata.dir)))
  mtz <- table(my.topic[ISI.search.cts$l], my.topic[ISI.search.cts$k])
  ## direction is l cites k

  grph <- graph_from_adjacency_matrix(mtz, mode = "directed", weighted = TRUE,  diag = FALSE)

## ‘layout_as_bipartite’, ‘layout_as_star’, ‘layout_as_tree’,     ‘layout_in_circle’, ‘layout_nicely’, ‘layout_on_grid’,     ‘layout_on_sphere’, ‘layout_randomly’, ‘layout_with_dh’,     ‘layout_with_fr’, ‘layout_with_gem’, ‘layout_with_graphopt’,     ‘layout_with_kk’, ‘layout_with_lgl’, ‘layout_with_mds’,     ‘layout_with_sugiyama’, ‘merge_coords’, ‘norm_coords’, ‘normalize’

plot(grph,layout=layout_nicely)

V(grph)$size  <- sqrt(table(ISI.search.df$topic)[V(grph)$name])
E(grph)$color <- brewer.pal(6,"Reds")[ceiling(log(E(grph)$weight))]

E(grph)$width <- ifelse(E(grph)$weight>10,2,1)

l <- layout_with_fr(grph,weights=sqrt(E(grph)$weight))
l <- layout_on_grid(grph)

plot(grph,layout=l,edge.arrow.size=.25, vertex.color="gold",  vertex.frame.color="orangered", vertex.label.color="black", vertex.label.cex=0.6)##, vertex.size=15, vertex.label.dist=2)


slc.mtz <- mtz[grep("CP",rownames(mtz)),grep("CT",colnames(mtz))]
slc.mtz <- slc.mtz[rowSums(slc.mtz)>0,colSums(slc.mtz)>0]

bprt <- graph_from_incidence_matrix(slc.mtz, directed = FALSE,
       mode = "all", multiple = FALSE,
       weighted = TRUE)
V(bprt)$color <- c("lightblue","pink")[grepl("CT",V(bprt)$name)+1]
E(bprt)$color <- brewer.pal(4,"Reds")[E(bprt)$weight]

E(bprt)$width <- ifelse(E(bprt)$weight>1,2,1)
E(bprt)$width <- E(bprt)$weight
l <- layout_as_bipartite(bprt)
l <- layout_in_circle(bprt)

l[,1] <- CP.clus$order[match(V(bprt)$name,CP.clus$label)]
l[,1] <- ifelse(is.na(l[,1]),46-CT.clus$order[match(V(bprt)$name,CT.clus$label)],l[,1])
l[grep("RV",V(bprt)$name),1] <- 0
l[grep("NN",V(bprt)$name),1] <- 46
l[,2] <- grepl("CP",V(bprt)$name)+0

## distribute along a circle...
a2 <- (l[,1]-(max(l[,1])/2))^2
c2 <- (max(l[,1])/2)^2
b2 <-  c2 - a2
l[,2] <- sqrt(b2) * ifelse(grepl("CP",V(bprt)$name),-1,1)

V(bprt)$size  <- sqrt(table(ISI.search.df$topic)[V(bprt)$name])*1.2

## figure X: not all CT topics are connected to CP topics, and only few have more than one link
plot(bprt,layout=l,edge.arrow.size=.25,  vertex.frame.color="orangered", vertex.label.color="black", vertex.label.cex=0.5)##, vertex.color="gold", vertex.size=15, vertex.label.dist=2)

## there are apparently few connections between CT and CP, maybe focus on this small subset to find out why?, specific terms? impact factor of journal, altmetric score or citation number of article (is cited because of widespread media attention or high number of citation?)


###########
## Degrees of separation between topics
##########

# remember: direction is l cites k
el <- cbind(ISI.search.df$UT[ISI.search.cts$l],
  ISI.search.df$UT[ISI.search.cts$k])
cts.graph <- graph_from_edgelist(el, directed = TRUE)

d0 <- distances(cts.graph, mode="all")

degSep <- data.frame()
for (k in levels(my.topic)) {
  for (l in levels(my.topic)) {
    ss <- d0[rownames(d0) %in% ISI.search.df$UT[my.topic %in% k],colnames(d0) %in% ISI.search.df$UT[my.topic %in% l]]
    ss <- ss[lower.tri(ss) & is.finite(ss)]
      degSep <- rbind(degSep,
        data.frame(k,l, mu=mean(1/(ss)), sg=sd(1/ss), d1=sum(ss %in% 1), d2=sum(ss %in% 2), d3=sum(ss %in% 3),total=length(ss)))
  }
}
subset(degSep,k %in% "CT20" & grepl("CP2",l))

degSep$dS <- with(degSep,d1+(d2/10)+(d3/100))
mtz <- with(degSep,tapply(dS,list(l,k),sum))
##mtz <- with(degSep,tapply(mu,list(l,k),sum))
slc.mtz <- mtz[grep("CP",rownames(mtz)),grep("CT",colnames(mtz))]
##slc.mtz[slc.mtz<.15] <- 0
slc.mtz <- slc.mtz[rowSums(slc.mtz,na.rm=T)>0,colSums(slc.mtz,na.rm=T)>0]
slc.mtz <- slc.mtz[order(rowSums(slc.mtz)),order(colSums(slc.mtz))]
bprt <- graph_from_incidence_matrix(slc.mtz, directed = FALSE,
       mode = "all", multiple = FALSE,
       weighted = TRUE)
##bprt <- permute(bprt,rank(c(rowSums(slc.mtz),colSums(slc.mtz))))

clrs1 <- cividis(15)
clrs1 <- brewer.pal(6,"Dark2")
clrs2 <- brewer.pal(6,"Accent")
c1 <- clrs1[CT.grp[match(V(bprt)$name,names(CT.grp))]]
c2 <- clrs2[CP.grp[match(V(bprt)$name,names(CP.grp))]]
V(bprt)$color <- ifelse(is.na(c1),ifelse(is.na(c2),"#aaaaaaaa",c2),c1)
 ##c("darkgreen","slateblue")[grepl("CT",V(bprt)$name)+1]
V(bprt)$size  <- sqrt(table(ISI.search.df$topic)[V(bprt)$name])*1.0

E(bprt)$color <- brewer.pal(8,"Reds")[
  as.numeric(cut(E(bprt)$weight,breaks=9))]
##E(bprt)$width <- ifelse(E(bprt)$weight>1,2,1)
E(bprt)$width <- E(bprt)$weight

#l <- layout_as_bipartite(bprt)
l <- layout_in_circle(bprt)
#l <- layout_as_tree(bprt)

x.CP <- l[grep("CP",V(bprt)$name),]
n.CP <- grep("CP",V(bprt)$name,value=T)
o.CP <- CP.clus$order[match(n.CP,CP.clus$label)]
o.CP <- CP.grp[match(n.CP,names(CP.grp))]

o.CP[is.na(o.CP)] <- c(min(o.CP,na.rm=T)-1,max(o.CP,na.rm=T)+1)


x.CT <- l[grep("CT",V(bprt)$name),]
n.CT <- grep("CT",V(bprt)$name,value=T)
o.CT <- CT.clus$order[match(n.CT,CT.clus$label)]
o.CT <- CT.grp[match(n.CT,names(CT.grp))]
o.CT[is.na(o.CT)] <- c(min(o.CT,na.rm=T)-1,max(o.CT,na.rm=T)+1)

l[,] <- rbind(x.CP[rank(o.CP,ties.method='first'),],x.CT[rank(o.CT,ties.method='first'),])

l[grepl("CP",V(bprt)$name),2] <- l[grepl("CP",V(bprt)$name),2]+.1
## distribute along a circle...
#a2 <- (l[,1]-(max(l[,1])/2))^2
#c2 <- (max(l[,1])/2)^2
#b2 <-  c2 - a2
#l[,2] <- sqrt(b2) * ifelse(grepl("CP",V(bprt)$name),-1,1)


## figure X: not all CT topics are connected to CP topics, and only few have more than one link

plot(bprt,layout=l,edge.arrow.size=.25,  vertex.frame.color=NA, vertex.label.color="black", vertex.label.cex=0.5, margin=c(0,0,0,0),
  ##mark.groups=list(CT1=names(CT.grp)[CT.grp==1],CT2=names(CT.grp)[CT.grp==2],CT3=names(CT.grp)[CT.grp==3],CT4=names(CT.grp)[CT.grp==4],CT5=names(CT.grp)[CT.grp==5],CT6=names(CT.grp)[CT.grp==6]),
  vertex.label.dist=0)##, vertex.color="gold", vertex.size=15)

CP.top_terms %>% filter(topic %in% c(3,9,26,27,22) & beta>.1)

CT.top_terms %>% filter(topic %in% c(15,29,21) & beta>.1)
















##############
## vwvwvwvvwvw old code below wvwvwvwvwvwv
###############

dts <- data.frame(from.topic=ISI.search.df[cts$l,"CP_topic"],
  to.topic=ISI.search.df[cts$k,"CT_topic"],
  from.year=ISI.search.df[cts$l,"PY"],
  to.year=ISI.search.df[cts$k,"PY"])


ss <- subset(dts,!is.na(from.topic) & !is.na(to.topic))
dim(ss)

tt <- table(ss$from.year,ss$to.year)
image(as.numeric(rownames(tt)),as.numeric(colnames(tt)),tt)


doS <- data.frame()
m.doS <- c()
for (Y in 2001:2020) {
  fromUT <- subset(ISI.search.df,PY %in% Y & !is.na(CT_topic))$UT
  toUT <- subset(ISI.search.df,PY >= Y & !is.na(CP_topic))$UT

  qry  <- as.vector(mtz[rownames(mtz) %in% fromUT,colnames(mtz) %in% toUT])
  qry <- subset(qry,!is.infinite(qry))
  ##doS <- rbind(doS,quantile(qry,prob=c(0,.05,.25,.5,.75,1)))
  doS <- rbind(doS,data.frame(d1=sum(qry==1),d2=sum(qry==2),d3=sum(qry==3)))
 ##m.doS <- c(m.doS,mean(qry))
 ##m.doS <-  c(m.doS,sum(qry==1))

}
## not much difference in degree of separation
matplot(2001:2020,sqrt(doS),type="l")
##lines(2001:2020,m.doS)


# frequency of conservation keywords per topic...
with(subset(ISI.search.df,!is.na(CT_topic)),aggregate(cons.kwd %in% "YES",by=list(CT_topic),mean))


layout(matrix(1:16,ncol=4,nrow=4))
l <- c(sprintf("CP%02d",1:14),"CPRV","CPNN")
for (k in c(sprintf("CT%02d",1:14),"CTRV","CTNN")) {
    ss <- mtz[rownames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% k],colnames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% l]]
    par(mar=c(2,2,4,0))
    hist(ss,xlim=c(0,12),breaks=seq(-.5,16.5,by=1),main=k)
}


##
layout(matrix(1:16,ncol=4,nrow=4))
l <- c(sprintf("CT%02d",1:14),"CTRV","CTNN")
for (k in c(sprintf("CP%02d",1:14),"CPRV","CPNN")) {
    ss <- mtz[rownames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% k],colnames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% l]]
    par(mar=c(2,2,4,0))
    hist(ss,xlim=c(0,12),breaks=seq(-.5,16.5,by=1),main=k)
}


tt <- table(ISI.search.df$CT_topic,ISI.search.df$PY)

tt[6,]/colSums(tt)
plot(tt[6,]/colSums(tt))
abline(h=1/14)


## conservation planning literature per year: list of references: how many directly related to CT research: how many indirectly related to camera trap: is this growing ?, year with maximum and minimum influence?

## reference to old or new camera trap research? lag or gap?

## how this differs by cons plan and camera trap topic?
