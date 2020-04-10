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

  cameratrap.prd <- topics(cameratrap.lda)
  consplan.prd <- topics(consplan.lda)


  ISI.search.df[match(names(cameratrap.prd),ISI.search.df$UT),"CT_topic"] <- as.character(cameratrap.prd)
  ISI.search.df[match(names(consplan.prd),ISI.search.df$UT),"CP_topic"] <- as.character(consplan.prd)

  ISI.search.df[ISI.search.df$DT %in% "REVIEW" & ISI.search.df$search.group %in% "cameratrap","CT_topic"] <- "RV"
  ISI.search.df[ISI.search.df$DT %in% "REVIEW" & ISI.search.df$search.group %in% "consplan","CP_topic"] <- "RV"




## direct citations
cts <- data.frame()
for (k in 1:nrow(ISI.search.df)) {
  l <- grep(ISI.search.df$SR[k],ISI.search.df$CR)
  if (length(l)>0)
    cts <- rbind(cts,data.frame(k,l))
}

table(ISI.topic.df[cts$l,"lda_topic"],
ISI.topic.df[cts$k,"lda_topic"])
