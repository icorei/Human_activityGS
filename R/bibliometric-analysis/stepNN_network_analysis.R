setwd("~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/R/bibliometric-analysis")
data.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/documents/ISI-20191211"
require(lubridate)
library(stringr)
library(caret)
library(sna)
library(igraph)
library(ggplot2)
library(plyr)
library(scales)
library(tidyr)


  ######
  #STEP 4: NETWORK ANALYSIS
  ######

  CT.prd <- topics(CT.lda)
  CP.prd <- topics(CP.lda)


  ISI.search.df[match(names(CT.prd),ISI.search.df$UT),"CT_topic"] <- sprintf("CT%02d",CT.prd)
  ISI.search.df[match(names(CP.prd),ISI.search.df$UT),"CP_topic"] <- sprintf("CP%02d",CP.prd)

  ISI.search.df[ISI.search.df$DT %in% "REVIEW" & ISI.search.df$search.group %in% "CT","CT_topic"] <- "CTRV"
  ISI.search.df[ISI.search.df$DT %in% "REVIEW" & ISI.search.df$search.group %in% "CP","CP_topic"] <- "CPRV"

  ISI.search.df[is.na(ISI.search.df$CP_topic) & ISI.search.df$search.group %in% "CP","CP_topic"] <- "CPNN"
  ISI.search.df[is.na(ISI.search.df$CT_topic) & ISI.search.df$search.group %in% "CT","CT_topic"] <- "CTNN"
  ISI.search.df$topic <- ifelse(ISI.search.df$search.group %in% "CT",ISI.search.df$CT_topic,ISI.search.df$CP_topic)

table(ISI.search.df$search.group,ISI.search.df$topic,useNA="always")


## direct citations
cts <- data.frame()
for (k in 1:nrow(ISI.search.df)) {
  l <- grep(ISI.search.df$SR[k],ISI.search.df$CR)
  if (length(l)>0)
    cts <- rbind(cts,data.frame(k,l))
}

mtz <- table(ISI.search.df[cts$l,"topic"], ISI.search.df[cts$k,"topic"])
grph <- graph_from_adjacency_matrix(mtz, mode = "directed", weighted = TRUE,  diag = FALSE)

## ‘layout_as_bipartite’, ‘layout_as_star’, ‘layout_as_tree’,     ‘layout_in_circle’, ‘layout_nicely’, ‘layout_on_grid’,     ‘layout_on_sphere’, ‘layout_randomly’, ‘layout_with_dh’,     ‘layout_with_fr’, ‘layout_with_gem’, ‘layout_with_graphopt’,     ‘layout_with_kk’, ‘layout_with_lgl’, ‘layout_with_mds’,     ‘layout_with_sugiyama’, ‘merge_coords’, ‘norm_coords’, ‘normalize’

plot(grph,layout=layout_nicely)

V(grph)$size  <- sqrt(table(ISI.search.df$topic)[V(grph)$name])
E(grph)$color <- brewer.pal(6,"Reds")[ceiling(log(E(grph)$weight))]

E(grph)$width <- ifelse(E(grph)$weight>10,2,1)

l <- layout_with_fr(grph,weights=sqrt(E(grph)$weight))
l <- layout_on_grid(grph)

plot(grph,layout=l,edge.arrow.size=.25, vertex.color="gold",  vertex.frame.color="orangered", vertex.label.color="black", vertex.label.cex=0.6)##, vertex.size=15, vertex.label.dist=2)



bprt <- graph_from_incidence_matrix(mtz[grep("CT",rownames(mtz)),grep("CP",colnames(mtz))], directed = FALSE,
       mode = "all", multiple = FALSE,
       weighted = TRUE)
V(bprt)$color <- c("lightblue","pink")[grepl("CT",V(bprt)$name)+1]
E(bprt)$color <- brewer.pal(4,"Reds")[round(sqrt(E(bprt)$weight))]

E(bprt)$width <- ifelse(E(bprt)$weight>3,2,1)
l <- layout_as_bipartite(bprt)
l <- layout_in_circle(bprt)

V(bprt)$size  <- sqrt(table(ISI.search.df$topic)[V(bprt)$name])*1.2

plot(bprt,layout=l,edge.arrow.size=.25,  vertex.frame.color="orangered", vertex.label.color="black", vertex.label.cex=0.5)##, vertex.color="gold", vertex.size=15, vertex.label.dist=2)



table(ISI.search.df[cts$l,"CP_topic"],
ISI.search.df[cts$k,"CT_topic"])


dts <- data.frame(from.topic=ISI.search.df[cts$l,"CP_topic"],
  to.topic=ISI.search.df[cts$k,"CT_topic"],
  from.year=ISI.search.df[cts$l,"PY"],
  to.year=ISI.search.df[cts$k,"PY"])


  ## there are apparently few connections between CT and CP, maybe focus on this small subset to find out why?, specific terms? impact factor of journal, altmetric score or citation number of article (is cited because of widespread media attention or high number of citation?)

ss <- subset(dts,!is.na(from.topic) & !is.na(to.topic))
dim(ss)

tt <- table(ss$from.year,ss$to.year)
image(as.numeric(rownames(tt)),as.numeric(colnames(tt)),tt)


el <- cbind(ISI.search.df$UT[cts$l],
  ISI.search.df$UT[cts$k])
cts.graph <- graph_from_edgelist(el, directed = TRUE)

mtz <- distances(cts.graph, mode="all")

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


degSep <- data.frame()
for (k in sort(unique(ISI.search.df$topic))) {
  for (l in sort(unique(ISI.search.df$topic))) {
    ss <- mtz[rownames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% k],colnames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% l]]
      degSep <- rbind(degSep,data.frame(k,l,    median(ss),    mad(ss)))
  }
}

layout(matrix(1:16,ncol=4,nrow=4))
for (k in "CT10") {
  for (l in c(sprintf("CP%02d",1:14),"CPRV","CPNN")) {
    ss <- mtz[rownames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% k],colnames(mtz) %in% ISI.search.df$UT[ISI.search.df$topic %in% l]]
    par(mar=c(2,2,4,0))
    hist(ss,xlim=c(1,12),breaks=seq(.5,16.5,by=1),main=l)
  }
}


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
