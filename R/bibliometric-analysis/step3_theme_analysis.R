#! R --vanilla

## Load required packages
library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes
library(tidytext)

## Set up working environment (customize accordingly...)
## FOR ADA:
script.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture"
## FOR JR:
script.dir <- "~/proyectos/IVIC/the-big-picture"

work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

## Load data from previous step:
load(file=sprintf("%s/ISI-camera-corpus.rda",Rdata.dir))

######
# THEME ANALYSIS FOR CAMERA TRAPS PAPERS
######


#LDA model

ISI.camera.dtm <- convert(ISI.camera.dfm, to = "topicmodels")

library("ldatuning")
result <- FindTopicsNumber(
ISI.camera.dtm,
topics = seq(from = 7, to = 37, by = 3),
metrics = c("CaoJuan2009"),
method = "Gibbs",
control = list(seed = 77),
mc.cores = 2L,
verbose = TRUE
)

#Now, we can fit our LDA model
ISI.camera.lda <- LDA(ISI.camera.dtm, control=list(seed=0), k = 10) # set the number of topics to 10.
terms(ISI.camera.lda, 20) #show top 10 words pertaining to each topic

#Obtain the most likely topics for each document
docvars(ISI.camera.dfm, 'topic') <- topics(ISI.camera.lda)
head(topics(ISI.camera.lda), 5) #show topic allocation for the first docucments

#lists the document to (primary) topic assignments:
my.rslt <- topics(ISI.camera.lda)
table(my.rslt)
#    1   2   3   4   5   6   7   8   9  10
# 191 299 289 181 225 229 221 232 183 135

#The tidytext package provides this method for extracting the per-topic-per-word
#probabilities, called  β(“beta”), from the model.
ap_topics <- tidy(ISI.camera.lda, matrix = "beta")
ap_topics

#Now we can build the tidy data frame for the keywords.
#For this one, we need to use unnest() from tidyr, because they are in a
#list-column.
#Visualization
ap_top_terms <- ap_topics %>%
group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)

ap_top_terms %>%
mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(term, beta, fill = factor(topic))) +
geom_col(show.legend = FALSE) +
facet_wrap(~ topic, scales = "free") +
coord_flip() +
scale_x_reordered()

as.data.frame(ap_top_terms)
#Dendogram to evaluate how similar are the topics
str(ISI.camera.lda)
my.model <- ISI.camera.lda@beta
distance_matrix <- dist(my.model, method="euclidean")
plot(hclust(distance_matrix), cex = 1)

#Document-topic probabilities
#Besides estimating each topic as a mixture of words,
#LDA also models each document as a mixture of topics
#We can examine the per-document-per-topic probabilities, called
#γ (“gamma”), with the matrix = "gamma" argument to tidy()
lda_gamma <- tidy(ISI.camera.lda, matrix = "gamma")
lda_gamma

#How are the probabilities distributed? Let’s visualize them
boxplot(lda_gamma$gamma ~ lda_gamma$topic)
#Hmm, few documents are classified in a given topic
#with high γ...seems like documents are asigned randomly
#to a given topic

#Another way to see this is:
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))
#There are many values near zero, which means there are many documents that
#do not belong in each topic. Also, there are few values near γ= 1
#these are the documents that do belong in those topics.
#This distribution shows that documents are being not well discriminated as
#belonging to a topic or not. We can also look at how the probabilities#
#are distributed within each topic

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))
#We can use this information to decide how many topics for
#our topic modeling procedure
#When we tried options higher than 6, the distributions for
#γ started to look very flat toward  γ= 1;
#documents were not getting sorted into topics very well.

#Convert to a data frame
my.rslt <- as.data.frame(as.table(my.rslt))
colnames(my.rslt)<- c("UT", "lda_topic")
str(my.rslt)

#Now get back to the complete collection of literature review
#and combine it
M3 <- merge(M2, my.rslt, by = "UT", all.x = T)
summary(M3$lda_topic)
dim(M3)
#2415   44

#Topic temporal trends
#We can do this by:
#!) Calculating the total number of articles that have been published on a topic
#over a particular period. This will provides provides information on total research
#effort within a corpus.
#2) by investigating changes in topic popularity over that period. This allow us
#evaluate which topics are hot (i.e., show positive growth) versus cold (negative growth)
#within a given research community.

#Lets see the first approach

#Topic 1
M3$topic_1 <- 0
M3$topic_2 <- 0
M3$topic_3 <- 0
M3$topic_4 <- 0
M3$topic_5 <- 0
M3$topic_6 <- 0
M3$topic_7 <- 0
M3$topic_8 <- 0
M3$topic_9 <- 0
M3$topic_10 <- 0

    M3$topic_1 <- grepl(1, M3$lda_topic)
    M3$topic_1[M3$lda_topic==c("TRUE")]<-1
    M3$topic_1[M3$lda_topic==c("FALSE")]<-0

    M3$topic_2 <- grepl(2, M3$lda_topic)
    M3$topic_2[M3$lda_topic==c("TRUE")]<-1
    M3$topic_2[M3$lda_topic==c("FALSE")]<-0

    M3$topic_3 <- grepl(3, M3$lda_topic)
    M3$topic_3[M3$lda_topic==c("TRUE")]<-1
    M3$topic_3[M3$lda_topic==c("FALSE")]<-0

    M3$topic_4 <- grepl(4, M3$lda_topic)
    M3$topic_4[M3$lda_topic==c("TRUE")]<-1
    M3$topic_4[M3$lda_topic==c("FALSE")]<-0

    M3$topic_5 <- grepl(5, M3$lda_topic)
    M3$topic_5[M3$lda_topic==c("TRUE")]<-1
    M3$topic_5[M3$lda_topic==c("FALSE")]<-0

    M3$topic_6 <- grepl(6, M3$lda_topic)
    M3$topic_6[M3$lda_topic==c("TRUE")]<-1
    M3$topic_6[M3$lda_topic==c("FALSE")]<-0

    M3$topic_7 <- grepl(7, M3$lda_topic)
    M3$topic_7[M3$lda_topic==c("TRUE")]<-1
    M3$topic_7[M3$lda_topic==c("FALSE")]<-0

    M3$topic_8 <- grepl(8, M3$lda_topic)
    M3$topic_8[M3$lda_topic==c("TRUE")]<-1
    M3$topic_8[M3$lda_topic==c("FALSE")]<-0

    M3$topic_9 <- grepl(9, M3$lda_topic)
    M3$topic_9[M3$lda_topic==c("TRUE")]<-1
    M3$topic_9[M3$lda_topic==c("FALSE")]<-0

    M3$topic_10 <- grepl(10, M3$lda_topic)
    M3$topic_10[M3$lda_topic==c("TRUE")]<-1
    M3$topic_10[M3$lda_topic==c("FALSE")]<-0

    ###
    #How many years cover our review?
    M3$PY <- as.character(M3$PY)
    tmp <- table(unlist(strsplit(M3$PY," :: ")))
    length(tmp)
    #23 years from 1984 to 2020

    topic_1<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_1 <- rbind(topic_1, data.frame(my.topic_1 = sum(M3$topic_1[grep(aa, M3$PY)])))

    }

    topic_2<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_2 <- rbind(topic_2, data.frame(my.topic_2 = sum(M3$topic_2[grep(aa, M3$PY)])))

    }

    topic_3<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_3 <- rbind(topic_3, data.frame(my.topic_3 = sum(M3$topic_3[grep(aa, M3$PY)])))

    }

    topic_4<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_4 <- rbind(topic_4, data.frame(my.topic_4 = sum(M3$topic_4[grep(aa, M3$PY)])))

    }

    topic_5<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_5 <- rbind(topic_5, data.frame(my.topic_5 = sum(M3$topic_5[grep(aa, M3$PY)])))

    }


    topic_6<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_6 <- rbind(topic_6, data.frame(my.topic_6 = sum(M3$topic_6[grep(aa, M3$PY)])))

    }

    topic_7<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_7 <- rbind(topic_7, data.frame(my.topic_7 = sum(M3$topic_7[grep(aa, M3$PY)])))

    }

    topic_8<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_8 <- rbind(topic_8, data.frame(my.topic_8 = sum(M3$topic_8[grep(aa, M3$PY)])))

    }

    topic_9<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_9 <- rbind(topic_9, data.frame(my.topic_9 = sum(M3$topic_9[grep(aa, M3$PY)])))

    }

    topic_10<- data.frame()
    tt <- c()
    for (aa in names(tmp)[1:23]) {
      topic_10 <- rbind(topic_10, data.frame(my.topic_10 = sum(M3$topic_10[grep(aa, M3$PY)])))

    }


    tmp1<-as.data.frame(tmp)
    fch1<-as.numeric(levels(tmp1$Var1))[tmp1$Var1]
    total.reg<-tmp1$Freq
    t.ints<-data.frame(cbind(fch1,total.reg, topic_1, topic_2, topic_3, topic_4,
                             topic_5, topic_6, topic_7, topic_8, topic_9, topic_10))
    #Accumulated number of publications
    for (k in 2:12) {
      t.ints[,k] <- cumsum(t.ints[,k])
    }

    #Proportion of total
    t.ints$p.topic_1 <- c((t.ints$my.topic_1*100)/t.ints$total.reg)
    t.ints$p.topic_2 <- c((t.ints$my.topic_2*100)/t.ints$total.reg)
    t.ints$p.topic_3 <- c((t.ints$my.topic_3*100)/t.ints$total.reg)
    t.ints$p.topic_4 <- c((t.ints$my.topic_4*100)/t.ints$total.reg)
    t.ints$p.topic_5 <- c((t.ints$my.topic_5*100)/t.ints$total.reg)
    t.ints$p.topic_6 <- c((t.ints$my.topic_6*100)/t.ints$total.reg)
    t.ints$p.topic_7 <- c((t.ints$my.topic_7*100)/t.ints$total.reg)
    t.ints$p.topic_8 <- c((t.ints$my.topic_8*100)/t.ints$total.reg)
    t.ints$p.topic_9 <- c((t.ints$my.topic_9*100)/t.ints$total.reg)
    t.ints$p.topic_10<- c((t.ints$my.topic_10*100)/t.ints$total.reg)

    colSums(t.ints)

    #Plot to vizualize temporal publication pattern
      plot(t.ints$fch1, t.ints$total.reg, pch=NA, xlim=c(1994,2020), ylim=c(0.0,100),
           ylab= "% of publications", xlab= "Year")
      lines(t.ints$fch1, t.ints$p.topic_1, col= "black", lwd= 2, lty = 1, pch = 3, type="b", cex = 1) #Mammals
      lines(t.ints$fch1, t.ints$p.topic_2, col= "grey20", lwd= 2, pch = 15, type="b", cex = 1) #Home range & abundance
      lines(t.ints$fch1, t.ints$p.topic_3, col= "grey50", lwd= 2, pch = 17, type="b") #Protected area & threatened spp
      lines(t.ints$fch1, t.ints$p.topic_4, col= "black", lwd= 2, pch = 18, type="b") #Use trap & Prey
      lines(t.ints$fch1, t.ints$p.topic_5, col= "grey70", lwd= 2, pch= 19, type="b", cex = 1) #Density estimation & Methods
      lines(t.ints$fch1, t.ints$p.topic_6, col= "red", pch = 3,type="b") #Richness of mammals spp
      lines(t.ints$fch1, t.ints$p.topic_7, col= "blue", pch =15, type="b",  cex = 1, lwd= 3)
      lines(t.ints$fch1, t.ints$p.topic_8, col= "green", pch = 17, type="b", ,  cex = 1, lwd= 3)
      lines(t.ints$fch1, t.ints$p.topic_9, col= "orange", pch = 18, type="b", ,  cex = 1, lwd= 3)
      lines(t.ints$fch1, t.ints$p.topic_10, col= "pink", pch = 19,  type="b",  cex = 1, lwd= 3)

      legend(2000, 90,c("topic 1 (Nature reserve for large mammals)","topic 2 (Diversity)",
                        "topic 3 (Density estimates)", "topic 4 (Conservation & Threatened spp)",
                        "topic 5 (Protected area)", "topic 6 (Prey – Predator)","topic 7 (Human activities)",
                        "topic 8 (Activity pattern)", "topic 9 (Occupancy models)",
                        "topic 10 (Wildlife management & Monitoring)")
             ,pch =c (3,15,17,18,19, 3,15,17,18,19),
             col=c("black","grey20", "grey50", "black", "grey70", "red","blue", "green", "orange", "pink"), cex=1 )


    #Now for the 2nd approach we will use lme4
    #using a Poisson model with a log link
    #But firts we will extract the colums of number of cite by paper from
    #the biblioAnalysis function and add to the data frame t.ints.
    #We will use this colunm as predictive variable
    tmp002 <- biblioAnalysis(M3, sep = ";")
    tmp003 <- data.frame(tmp002$MostCitedPapers$`Paper         `, tmp002$TCperYear, tmp002$TotalCitation)
    colnames(tmp003)<- c("SR", "TC_year", "TC")

    #Now, merge with M3
    tmp004 <- data.frame(SR = M3$SR, fch1 = M3$PY, topic = M3$lda_topic)
    t.ints2 <- merge(tmp004, tmp003, by ="SR")
    str(t.ints2)
    t.ints2$fch1 <- as.numeric(as.character(t.ints2$fch1))

    #Now we can fit a mixed modelsto fit a unique intercept
    #(i.e., mean number of publications if the predictor variable is centered)
    #and slope (i.e., rate of change in number of publications) for each topic.
    #the predictor variable is time
    library(lme4)
      my.model1 <- glmer(TC_year ~ fch1 + (fch1|topic),
                        data = t.ints2, family = poisson)
      tt <-as.data.frame(ranef(my.model1)$topic)
      colnames(tt) <- c("intercept", "slope")
      tt$topic <- c(1:10)
      #Plot
    p <- ggplot(tt, aes(intercept, slope, label = topic))
    p + geom_point(cex = 7)+ geom_text(hjust = 0, nudge_x = 0.01, cex = 7)
        #In such a model, topics with positive random intercepts (i.e., u > 0)
    #can be interpreted as having higher- than-average numbers of articles written
  #about them in the period. Similarly, topics with positive random slopes
  #have higher-than-average growth in publications during the same period.



  ## save to a Rdata object:
#  save(file=sprintf("%s/....rda",Rdata.dir), ...)

  ## That's it!, we are ready for the next step.
