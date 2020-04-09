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


  ISI.topic.df <- merge(ISI.search.df, prd.topic, by = "UT", all.x = T)

  dim(ISI.topic.df)
  str(ISI.topic.df)
  table(ISI.topic.df$search.group, ISI.topic.df$lda_topic)

  #Unify lda topic for CP papers, lad-topics = NA should be "cp"
  ISI.topic.df$lda_topic[ISI.topic.df$search.group %in% "consplan"] <- "CP"
  table(ISI.topic.df$search.group, ISI.topic.df$lda_topic)


  #There are 283 document of CT without LDA classification

  #####
  ISI.hist.nw <- histNetwork(ISI.topic.df, min.citations = 3, sep = ";")

  #Extract the network matrix from the object so we can have more control
  #of plotting parameters
  net <- ISI.hist.nw$NetMatrix
  str(net)

## direct citations
cts <- data.frame()
for (k in 1:nrow(ISI.topic.df)) {
  l <- grep(ISI.topic.df$SR[k],ISI.topic.df$CR)
  if (length(l)>0)
    cts <- rbind(cts,data.frame(k,l))
}

table(ISI.topic.df[cts$l,"lda_topic"],
ISI.topic.df[cts$k,"lda_topic"])
