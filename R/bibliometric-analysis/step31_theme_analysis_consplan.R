library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes
library(tidytext)
library("ldatuning")

load(file=sprintf("%s/ISI-CP-corpus.rda",Rdata.dir))

CP.dfm <- dfm(CP.bigram, thesaurus = camera_thesaurus)
CP.dfm

CP.dfm <- dfm_trim(CP.dfm, min_termfreq = 20)


CP.dtm <- convert(CP.dfm, to = "topicmodels")
CP.lda <- LDA(CP.dtm, control=list(seed=0), k = 14)

tt <- topics(CP.lda)
docvars(CP.dfm, 'topic') <- tt[match(row.names(CP.dfm),names(tt))]

CP.topics <- tidy(CP.lda, matrix = "beta")
CP.topics

CP.top_terms <- CP.topics %>%
group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)

CP.top_terms %>%
mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(term, beta, fill = factor(topic))) +
geom_col(show.legend = FALSE) +
facet_wrap(~ topic, scales = "free") +
coord_flip() +
scale_x_reordered()
