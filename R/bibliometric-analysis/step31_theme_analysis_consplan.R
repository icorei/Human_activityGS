library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes
library(tidytext)
library("ldatuning")

load(file=sprintf("%s/ISI-20191211-cameratrap-corpus.rda",Rdata.dir))

consplan.dfm <- dfm(consplan.bigram, thesaurus = camera_thesaurus)
consplan.dfm

consplan.dfm <- dfm_trim(consplan.dfm, min_termfreq = 20)


consplan.dtm <- convert(consplan.dfm, to = "topicmodels")
consplan.lda <- LDA(consplan.dtm, control=list(seed=0), k = 14)

tt <- topics(consplan.lda)
docvars(consplan.dfm, 'topic') <- tt[match(row.names(consplan.dfm),names(tt))]

consplan.topics <- tidy(consplan.lda, matrix = "beta")
consplan.topics

consplan.top_terms <- consplan.topics %>%
group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)

consplan.top_terms %>%
mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(term, beta, fill = factor(topic))) +
geom_col(show.legend = FALSE) +
facet_wrap(~ topic, scales = "free") +
coord_flip() +
scale_x_reordered()
