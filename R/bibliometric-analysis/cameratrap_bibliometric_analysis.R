setwd("~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/R/bibliometric-analysis")
data.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/documents/ISI-20191211"
library(quanteda) #a library for quantitative text analysis
library(dplyr) #for data munging
require(ggplot2) #visualization
require(lubridate)
require(topicmodels) #for topic modeling
library("RColorBrewer")
library(tidytext)
library(tidyr)
library(stringr)
library(caret)
library(sna)
library(igraph)
library(ggplot2)
library(plyr)
library(scales)



  ######
  #STEP 2: THEME ANALYSIS FOR CAMERA TRAPS PAPERS
  ######
  #Lets exclude all "review" type studies
  table (M$DT)
  #Choose only "review" articles
  tmp001 <- M[grep("REVIEW", M$DT), ]
  dim(tmp001)
  #93 43
  #Now delete this subset from the original data frame
  M2<- M[!(M$DT %in% tmp001$DT),]
  dim(M2)
  #2415   43

  #Now,apply Natural Language Processing and Topic Modeling to abstracts
  #to identify the topics published in conservation planning
  #First, clean the text and create a corpus

  txt1 <- M2[,c("UT","AB", "my.topic")] #take two columns from the dataframe N2.
  #Column UT is for Unique Article Identifier, and column AB is for abstracts.
  #txt1$UT <- tolower(txt1$TI)
  #convert to lower case
  txt1$AB <- tolower(txt1$AB)
  txt1$my.topic <- tolower(txt1$my.topic)

  #Create a corpus in which each abstract is a document
  #keep the UT as document identifier
  txt1_corpus <- corpus(txt1, docid_field = "UT", text_field = "AB")

  txt1_corpus #txt1_corpus is the name of the corpus created.
  head(docvars(txt1_corpus))
  #Tokenization (lexical analysis)
  #Tokenization is the process of splitting a text into tokens
  #(i.e. convert the text into smaller, more
  #specific text features, such as words or word combinations)
  #There are many ways to tokenize text
  #(by sentence, by word, or by line).
  #For our data, we tokenize by words.
  #Notice that we remove punctuation and numbers along the way
  toks <- tokens(txt1_corpus, remove_punct = TRUE, remove_numbers = TRUE)
  head(toks[[2]], 5) #show the first 5 tokens in the second document.

  #Remove stop words and a customized list of filter words.
  nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')
  nostop_toks <- tokens_select(nostop_toks, c("abstract", "study","the", "therefore",
                                                  "elsevier", "often", "based", "new", "due",
                                                  "two", "use", "used", "km", "2", "24",
                                                  "also", "may", "one", "within", "results",
                                                  "found", "however", "many", "elsewhere",
                                                  "n", "can", "camera", "trap", "camera-trap",
                                              "deutsch", "gesellschaft", "saugetierkund"),
                                 selection = 'remove')
  head(nostop_toks[[3]], 5)
  #Simplify words to avoid confussion with deriv and plural terms
  nostop_toks <- tokens_wordstem(nostop_toks,
                                 language = quanteda_options("language_stemmer"))
  head(nostop_toks[[3]], 5)
  #"fragment" "natur"    "environ"  "import"   "threat"
  #Create n-gram. (tokens in sequence)
  nostop_toks<- tokens_ngrams(nostop_toks, n=2) #for bigram
  head(nostop_toks[[1]], 10) #show the first 10 bigram
  nostop_toks <- tokens_select(nostop_toks, c("c_deutsch", "saugetierkund_publish",
                                              "c_ltd", "publish_gmbh", "gmbh_right",
                                              "ltd_right"),
                               selection = 'remove')


  #Unify similar concepts
  my_thesaurus1 <- dictionary(list(protect_area = c("nation_park", "protect_area"),
                                   large_mammals = c("large_carnivor", "large_mammal", "red_fox", "larg_mammal", "larg_carnivor",
                                                     "snow_leopard", "wild_boar"),
                                   densiti_estim = c("densiti_estim", "popul_densiti", "popul_densiti", "estim_densiti"),
                                   occup_model = c("detect_probabl", "occup_model")))


  #Create DTM (Document Term Matrix).
  #Commun format for text analysis. A DTM is a matrix in which rows are
  #documents, columns are terms, and cells indicate how often
  #each term occurred in each document.
  my_dfm <- dfm(nostop_toks, thesaurus = my_thesaurus1)
  my_dfm
  #Document-feature matrix of: 2,415 documents, 179,972 features (99.9% sparse).
  #There are too much features
  #Lets simplify it by
  #create a new dfm to include words that have appeared at least 25 times in the corpus.
  new_dfm <- dfm_trim(my_dfm, min_termfreq = 50)
  #2,415 documents, 567 features
  #Better
  #create a feature co-occurrence matrix
  new_fcm <- fcm(new_dfm)

  #extract top 50 keywords based on abstracts and create a feature co-occurrence matrix
  #based on the top 50
  feat <- names(topfeatures(new_fcm, 50))
  new_fcm <- fcm_select(new_fcm, feat)

  size <- log(colSums(dfm_select(new_dfm, feat)))
  textplot_network(new_fcm, min_freq = 0.5, vertex_size = size/max(size) * 3)

  topfeatures(my_dfm, 10)

  #Plot top keywords and produce a wordcloud of top keywords.
  freq <- textstat_frequency(new_dfm, n = 50)
  new_dfm %>%
    textstat_frequency(n = 20) %>%
    ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
    geom_point() +
    coord_flip() +
    labs(x = NULL, y = "Frequency") +
    theme_minimal()

  textplot_wordcloud(new_dfm, max_words = 100,
                     random.order=FALSE, rot.per=0.35,
                     colors=brewer.pal(8, "Dark2"))
  #LDA model
  #Now we'll use topic modeling to clasiffy the articles
  #But first we need determine what is the optimal number of topics
  #we should specify in the LDA model
  #Package ldatuning realizes 4 metrics to select perfect number of topics for LDA model.
  dtm <- convert(new_dfm, to = "topicmodels")

  #library("ldatuning")
  #result <- FindTopicsNumber(
   # dtm,
    #topics = seq(from = 2, to = 15, by = 1),
    #metrics = c("CaoJuan2009"),
    #method = "Gibbs",
    #control = list(seed = 77),
    #mc.cores = 2L,
    #verbose = TRUE
  #)

  #Now, we can fit our LDA model
  lda <- LDA(dtm, control=list(seed=0), k = 10) # set the number of topics to 10.
  terms(lda, 20) #show top 10 words pertaining to each topic

  #Obtain the most likely topics for each document
  docvars(new_dfm, 'topic') <- topics(lda)
  head(topics(lda), 5) #show topic allocation for the first docucments

  #lists the document to (primary) topic assignments:
  my.rslt <- topics(lda)
  table(my.rslt)
  #    1   2   3   4   5   6   7   8   9  10
  # 191 299 289 181 225 229 221 232 183 135

  #The tidytext package provides this method for extracting the per-topic-per-word
  #probabilities, called  β(“beta”), from the model.
  ap_topics <- tidy(lda, matrix = "beta")
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
    str(lda)
    my.model <- lda@beta
    distance_matrix <- dist(my.model, method="euclidean")
    plot(hclust(distance_matrix), cex = 1)

    #Document-topic probabilities
    #Besides estimating each topic as a mixture of words,
    #LDA also models each document as a mixture of topics
    #We can examine the per-document-per-topic probabilities, called
    #γ (“gamma”), with the matrix = "gamma" argument to tidy()
    lda_gamma <- tidy(lda, matrix = "gamma")
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

  ######
  #STEP 4: NETWORK ANALYSIS
  ######

  #Network analysis
  #We will analyse the complete data set to evaluate
  #how frequently are camera trap cited in conservation planning papers
  #We will use "histNetwork" fuction creates a historical citation network
  #from a bibliographic data frame.
  #Historiograph - Direct citation linkages

  #Crate the data frame n3 with all literature review (CT and CP)
  #plus the vector with LDA topics
  str(my.rslt)
  my.rslt$lda_topic <- as.character(my.rslt$lda_topic)
  n3 <- merge(my.data, my.rslt, by = "UT", all.x = T)
  dim(n3)
  str(n3)
  table(n3$my.topic, n3$lda_topic)

  #Unify lda topic for CP papers, lad-topics = NA should be "cp"
  n3$lda_topic2 <-n3$lda_topic
  str(n3)
  n3$lda_topic2[is.na(n3$lda_topic)] <- "cp"
  table(n3$my.topic, n3$lda_topic2)

  n3 <- within(n3, lda_topic2[lda_topic2 == 'cp' & my.topic == 'camera_trap'] <- 'sin_lda')
  table(n3$my.topic, n3$lda_topic2)
  #There are 283 document of CT without LDA classification

  #####
  histResults2 <- histNetwork(n3, min.citations = 3, sep = ";")

  #Extract the network matrix from the object so we can have more control
  #of plotting parameters
  net <- histResults2$NetMatrix
  str(net)

  #Or we should create a Reference co-citation network?
  #NetMatrix <- biblioNetwork(n3, analysis = "co-citation",
   #                          network = "references", sep = ";")
  #networkPlot(NetMatrix, normalize = NULL,
   #               weighted= TRUE, n = 100, label = "",
    #          labelsize=0.7,
     #             edgesize = 5,
      #            curved=TRUE, Title = "A Co-citation Network of references",
       #           type = "mds", size= TRUE, remove.multiple=TRUE)

  #net <- NetMatrix

  #Now convert to igraph object
  my.net <-graph_from_adjacency_matrix(net, mode = "directed")
  #Examine attributes
  E(my.net)$weight
  edge_attr(my.net)
  #vertex_attr(my.net)
  vertex_attr_names(my.net)

  #To add a new attribute: topics
  #1. Iterate through the vector of vertices V(g)
  #You can't iterate the vector of vertices, you'll need to
  #iterate through a vertex attribute that all the vertices have.
  #2.Filter the data frame down to the row that matches each vector
  #3.Return the value of interest
  #But first create new table of attributes
  tmp001 <- data.frame(id = n3$SR,
                       my.topic = as.character(n3$my.topic),
                       lda.topic = as.character(n3$lda_topic2))

  str(tmp001)

my.net2 <- my.net %>%
  set_vertex_attr(.,
                  name = 'my.topic',
                  index = V(my.net),
                  value = sapply(V(my.net)$name, function(x){
                    tmp001 %>%
                      filter(id == x) %>%
                      .$my.topic
                   }))

#This result in a list of topics, we need transform this list
#in a vector and add as vertex attribute in the matrix
#Asign the new atribute as a vector
my.vector1 <- unlist(V(my.net2)$my.topic)
vertex_attr(my.net2, "my.topic2", index = V(my.net2)) <-sapply(my.vector1, paste0, collapse="")
str(V(my.net2)$my.topic2)

my.net2 <- my.net %>%
  set_vertex_attr(.,
                  name = 'lda.topic',
                  index = V(my.net),
                  value = sapply(V(my.net)$name, function(x){
                    tmp001 %>%
                      filter(id == x) %>%
                      .$lda.topic
                  }))

my.vector2 <- unlist(V(my.net2)$lda.topic)
vertex_attr(my.net2, "lda.topic2", index = V(my.net2)) <-sapply(my.vector2, paste0, collapse="")#unlist(c(my.vector2))
str(V(my.net2)$lda.topic2)

#Now calculate network metrix
#Degree by vertex
tmp002 <- centr_degree(my.net2, mode="all", normalized=T, loops = FALSE)
my.degree <- tmp002$res
#Betw by vertex
tmp003 <- centr_betw(my.net2, directed=F, normalized=T)
my.betw <- tmp003$res

#Asign the new atribute as a vector
vertex_attr(my.net2, "degree", index = V(my.net2)) <-unlist(c(my.degree))
str(V(my.net2)$degree)

vertex_attr(my.net2, "betw", index = V(my.net2)) <-unlist(c(my.betw))
str(V(my.net2)$betw)

#Plot the network
# Remove loops
my.net2 <- simplify(my.net2, remove.multiple = T, remove.loops = T)
#2263 vertex
#Delete isolated vertices
table(degree(my.net2))
Isolated = which(degree(my.net2)<10)
#1688 isolated vertex!!!!
my.net3 = delete.vertices(my.net2, Isolated)
#575 vertex only

# Set vertex color based on topic research:
igraph :: V(my.net3)$color = igraph :: V(my.net3)$my.topic2
igraph :: V(my.net3)$color = gsub("camera_trap", "black", igraph :: V(my.net3)$color)
igraph :: V(my.net3)$color = gsub("cons_plan", "grey70",igraph :: V(my.net3)$color)

# Set vertex color frame based on topic research:
igraph :: V(my.net3)$frame.color = igraph :: V(my.net3)$my.topic2
igraph :: V(my.net3)$frame.color = gsub("camera_trap", "black", igraph :: V(my.net3)$frame.color)
igraph :: V(my.net3)$frame.color = gsub("cons_plan", "grey70",igraph :: V(my.net3)$frame.color)

# Set vertex shape based on my.topic research:
#igraph :: V(my.net3)$shape = igraph :: V(my.net3)$lda.topic2

#change arrow size and edge color:
igraph :: E(my.net3)$arrow.size <- .3
igraph :: E(my.net3)$edge.color <- "blue"
igraph :: E(my.net3)$width <-  1

#Label characteristics
igraph :: V(my.net3)$label = igraph :: V(my.net3)$lda.topic2
V(my.net3)$label.cex = .6 #Label size
V(my.net3)$size = 4

#The best option
plot(my.net3, edge.curved=.1,
       layout=layout_nicely, edge.color = "lightblue",
       display.isolates= TRUE,
       vertex.label = ifelse(degree(my.net3) < 10, NA, V(my.net3)$name))
#ifelse(V(my.net3)$lda.topic2 == "cp" ,NA, V(my.net3)$name)
#vertex.label = V(my.net3)$lda.topic2
#vertex.label = ifelse(degree(my.net3) > 10, V(my.net3)$label, NA)

#Community detection
#Hierarchical agglomerative method that is designed to run well in large networks.
my.ceb <- edge.betweenness.community(my.net3, modularity = TRUE)
modularity(my.ceb)
#0.01862102
#the modularity score Q, which ranged from -1 to 1 and measured how strong
#the division was: the more positive the value of Q, the more significant
#the grouping. By definition, the entire network (as one community) had Q = 0

#How many paper by groups?
membership(my.ceb)
str(my.ceb)
#How many nodes by group
with(my.ceb, tapply(names, membership, function(x) length(unique(x))))

plot(my.ceb, my.net3,
     vertex.label = NA,
     layout = layout_nicely)
#As histogtram
plot_dendrogram(my.ceb, use.modularity = T,
                mode="hclust",
                cex = 0.15,
                lwd= 0.3)

######
#STEP 4: ADDITIONAL ANALYSIS
######
my.way <-data.frame(lda.topic = V(my.net2)$lda.topic2,
                    betw = V(my.net2)$betw,
                    degree = V(my.net2)$degree,
                    my.topic = V(my.net2)$my.topic2,
                    my.name = V(my.net2)$name)

str(my.way)
table(my.way$my.topic, my.way$lda.topic)

#Create a data frame with memberships
my.memb <- as.data.frame(as.table(membership(my.ceb)))
str(my.memb)
colnames(my.memb) <- c("my.name", "membership")

#Combine both data frames
my.way2 <- merge(my.way, my.memb, by = "my.name", all.x = T)
dim(my.way2)
str(my.way2)
table(my.way2$my.topic)

table(my.way2$membership, my.way2$my.topic)
table(my.way2$membership, my.way2$lda.topic)

boxplot(my.way2$degree ~ my.way2$lda.topic)
####
sp <- ggplot(my.way2, aes(degree, betw, fill = my.topic, size = 3))
sp <- sp + geom_point(aes(color = my.topic)) +
  theme(legend.position = "top") +
  facet_grid(membership ~ .) +
  scale_color_grey()+
  geom_text(
    label= my.way2$my.name,
    nudge_x = -0.1, nudge_y = -0.1, cex = 3,
    check_overlap = T
  )
sp

####
#END
####
