library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes
library(tidytext)
library("ldatuning")

load(file=sprintf("%s/ISI-20191211-cameratrap-corpus.rda",Rdata.dir))

cameratrap.dfm <- dfm(cameratrap.bigram, thesaurus = camera_thesaurus)
cameratrap.dfm

cameratrap.dfm <- dfm_trim(cameratrap.dfm, min_termfreq = 20)


cameratrap.dtm <- convert(cameratrap.dfm, to = "topicmodels")
cameratrap.lda <- LDA(cameratrap.dtm, control=list(seed=0), k = 14)

tt <- topics(cameratrap.lda)
docvars(cameratrap.dfm, 'topic') <- tt[match(row.names(cameratrap.dfm),names(tt))]

cameratrap.topics <- tidy(cameratrap.lda, matrix = "beta")
cameratrap.topics

cameratrap.top_terms <- cameratrap.topics %>%
group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)

cameratrap.top_terms %>%
mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(term, beta, fill = factor(topic))) +
geom_col(show.legend = FALSE) +
facet_wrap(~ topic, scales = "free") +
coord_flip() +
scale_x_reordered()
