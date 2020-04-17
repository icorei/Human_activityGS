library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes
library(tidytext)
library("ldatuning")

load(file=sprintf("%s/ISI-CT-corpus.rda",Rdata.dir))

CT.dfm <- dfm(CT.bigram, thesaurus = camera_thesaurus)
CT.dfm

CT.dfm <- dfm_trim(CT.dfm, min_termfreq = 20)


CT.dtm <- convert(CT.dfm, to = "topicmodels")
CT.lda <- LDA(CT.dtm, control=list(seed=0), k = 14)

tt <- topics(CT.lda)
docvars(CT.dfm, 'topic') <- tt[match(row.names(CT.dfm),names(tt))]

CT.topics <- tidy(CT.lda, matrix = "beta")
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
