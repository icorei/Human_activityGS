setwd("~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/R/bibliometric-analysis")
data.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture/documents/ISI-20191211"
library(bibliometrix) #the library for bibliometrics
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
      #STEP 1: DATA SETS
      #####
      #Use saved search results from Web of Science database
      #keyword "camera trap"
      #The library bibliometrix can convert bibtex files into data frames
      #There are >2000 results returned and they are saved into 5 BibTex files
      A1 <- readFiles(sprintf("%s/savedrecs_cameratrap1.bib",data.dir))
      A2 <- readFiles(sprintf("%s/savedrecs_cameratrap2.bib",data.dir))
      A3 <- readFiles(sprintf("%s/savedrecs_cameratrap3.bib",data.dir))
      A4 <- readFiles(sprintf("%s/savedrecs_cameratrap4.bib",data.dir))
      A5 <- readFiles(sprintf("%s/savedrecs_cameratrap5.bib",data.dir))
      
      #Conver .bib files into a data frame
      M1 <- convert2df(A1, dbsource = "isi", format = "bibtex")
      M2 <- convert2df(A2, dbsource = "isi", format = "bibtex")
      M3 <- convert2df(A3, dbsource = "isi", format = "bibtex")
      M4 <- convert2df(A4, dbsource = "isi", format = "bibtex")
      M5 <- convert2df(A5, dbsource = "isi", format = "bibtex")
        
      #Data set with camera traps literature  
      M<- rbind(M1,M2, M3, M4, M5) #merge the three files
      dim(M)
      #2469   42
      #Add a colum with our topic "camera trap"
      M$my.topic <- "camera_trap"
      results_M <- biblioAnalysis(M, sep = ";")
      summary(object=results_M,k=20,pause=FALSE)
        
      #Now we will import the result for the search using the keywords: 
      #"conservation planning" and "terrestrial"
      B1 <- readFiles(sprintf("%s/savedrecs_consplan1.bib",data.dir))
      B2 <- readFiles(sprintf("%s/savedrecs_consplan2.bib",data.dir))
      B3 <- readFiles(sprintf("%s/savedrecs_consplan3.bib",data.dir))
          
      N1 <- convert2df(B1, dbsource = "isi", format = "bibtex")
      N2 <- convert2df(B2, dbsource = "isi", format = "bibtex")
      N3 <- convert2df(B3, dbsource = "isi", format = "bibtex")
        
      #Data set for conservation planning literature 
      N<- rbind(N1,N2,N3)
      N$my.topic <- "cons_plan"
      dim(N)
      #1075   43
      
      results_N <- biblioAnalysis(N, sep = ";")
      summary(object=results_N,k=20,pause=FALSE)
      
      #Combine both data frames
      my.data<- rbind(M,N)
      dim(my.data)
      
      ##
      #Tenporal pattern in publication
      ###
      #How many publications by year?
      #Create binomial variables for topics
      my.data$camera<-0
      my.data$planning<-0
      #Camera trap
      my.data$camera <- grepl("camera_trap", my.data$my.topic)
      my.data$camera[my.data$my.topic==c("TRUE")]<-1
      my.data$camera[my.data$my.topic==c("FALSE")]<-0
      #Conservation planning
      my.data$planning <- grepl("cons_plan", my.data$my.topic)
      my.data$planning[my.data$my.topic==c("TRUE")]<-1
      my.data$planning[my.data$my.topic==c("FALSE")]<-0
      
      ###
      #How many years cover our review?
      my.data$PY <- as.character(my.data$PY)
      tmp <- table(unlist(strsplit(my.data$PY," :: ")))
      length(tmp)
      #31 years from 1984 to 2020
      
      camera<- data.frame()
      tt <- c()
      for (aa in names(tmp)[1:31]) {
        camera <- rbind(camera, data.frame(camera1 = sum(my.data$camera[grep(aa, my.data$PY)])))
        
      }
      
      my.plan<- data.frame()
      tt <- c()
      for (aa in names(tmp)[1:31]) {
        my.plan <- rbind(my.plan, data.frame(my.plan1 = sum(my.data$planning[grep(aa, my.data$PY)])))
        
      }
      
      tmp1<-as.data.frame(tmp)
      fch1<-as.numeric(levels(tmp1$Var1))[tmp1$Var1]
      total.reg<-tmp1$Freq
      t.ints<-data.frame(cbind(fch1,total.reg, camera, my.plan))
      #Accumulated number of publications
      for (k in 2:4) {
        t.ints[,k] <- cumsum(t.ints[,k])
      }
      #Plot to vizualize temporal publication pattern
      plot(t.ints$fch1, t.ints$total.reg, pch=NA, xlim=c(1984,2019), ylim=c(0.0,3500),
           ylab= "Accumaleted publications", xlab= "Year")
      lines(t.ints$fch1, t.ints$camera1, col= "black", lwd= 1, lty = 1)
      lines(t.ints$fch1, t.ints$my.plan1, col= "blue", lwd= 1, lty = 2)
      legend(1984,2500,c("Camera traps","Conservation planning"),lty=c(1,2),
             col=c("black","blue"), cex=.9)
      
######
#STEP 2: THEME ANALYSIS FOR CONSERVATION PLANNING PAPERS
######
#Lets exclude all "review" type studies
table (N$DT)
#Choose only "review" articles
tmp001 <- N[grep("REVIEW", N$DT), ]
dim(tmp001)
#93 43
#Now delete this subset from the original data frame
N2<- N[!(N$DT %in% tmp001$DT),]
dim(N2)
#982  43

#Now,apply Natural Language Processing and Topic Modeling to abstracts
#to identify the topics published in conservation planning
#First, clean the text and create a corpus

txt1 <- N2[,c("UT","AB", "my.topic")] #take two columns from the dataframe N2. 
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
                                             "n", "can", "conservation", "planning" ), 
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

#Remove "conserv_plan" from the corpus because is reduntant
#nostop_toks <- tokens_select(nostop_toks, c("conserv_plan"), 
 #                            selection = 'remove')

#Create DTM (Document Term Matrix).
#Commun format for text analysis. A DTM is a matrix in which rows are
#documents, columns are terms, and cells indicate how often 
#each term occurred in each document.
my_dfm <- dfm(nostop_toks)
my_dfm
#Document-feature matrix of: 982 documents, 101,106 features (99.9% sparse).
#There are too much features
#Lets simplify it by 
#create a new dfm to include words that have appeared at least 25 times in the corpus.
new_dfm <- dfm_trim(my_dfm, min_termfreq = 25)
#982 documents, 131 features (96.6% sparse).
#Better, we have now ~100 features
#create a feature co-occurrence matrix
new_fcm <- fcm(new_dfm) 

#extract top 50 keywords based on abstracts and create a feature co-occurrence matrix 
#based on the top 50
feat <- names(topfeatures(new_fcm, 25))
new_fcm <- fcm_select(new_fcm, feat)

size <- log(colSums(dfm_select(new_dfm, feat)))
textplot_network(new_fcm, min_freq = 0.5, vertex_size = size/max(size) * 3)

topfeatures(my_dfm, 10)

#Plot top keywords and produce a wordcloud of top keywords.
freq <- textstat_frequency(new_dfm, n = 25)
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
lda <- LDA(dtm, k = 8) # set the number of topics to 6.
terms(lda, 20) #show top 10 words pertaining to each topic

#Obtain the most likely topics for each document
docvars(new_dfm, 'topic') <- topics(lda) 
head(topics(lda), 5) #show topic allocation for the first docucments

#lists the document to (primary) topic assignments:
my.rslt <- topics(lda)
table(my.rslt)
# 1   2   3   4   5   6   7   8 
# 99 150 111 133 130  97  63 144

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

#As an alternative, we could consider the terms that had the 
#greatest difference in β between topics
#To constrain it to a set of especially relevant words, we can filter for 
#relatively common words, such as those that have a  β
#greater than 1/100 in at least one topic

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .01 | topic2 > .01) %>%
  mutate(log_ratio = log2(topic2 / topic1))

ggplot(beta_spread, aes(term, log_ratio)) +
  geom_bar(stat = "identity") +
  geom_col(show.legend = TRUE)+
  ylab("log odds ratio (topic2 / topic1)")+
  coord_flip()

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

#Thematic map according to author keywords
Map.n = thematicMap(N2, field = "DE", n = 200, minfreq = 5,
                stemming = FALSE, 
                size = .5, n.labels=2, 
                repel = FALSE)

plot(Map.n$map)
#Thematic map is a very intuitive plot and we can analyze themes according
#to the quadrant in which they are placed: 
#(1) upper-right quadrant: motor-themes; 
#(2) lower-right quadrant: basic themes; 
#(3) lower-left quadrant: emerging or disappearing themes; 
#(4) upper-left quadrant: very specialized/niche themes.
#Cobo, M. J., L?pez-Herrera, A. G., Herrera-Viedma, E., & Herrera, F. 
#(2011). An approach for detecting, quantifying, and visualizing the 
#evolution of a research field: A practical application to the fuzzy 
#sets theory field. Journal of Informetrics, 5(1), 146-166.

#It performs a Thematic Evolution Analysis based on co-word network analysis
#and clustering. The methodology is inspired by the proposal of Cobo et al. (2011).
nexus <- thematicEvolution(N2, field="DE", years=c(2000, 2010), n = 250, minFreq=2)
plotThematicEvolution(nexus$Nodes, nexus$Edges)
      #The thickness of the edge is proportional to the inclusion index, 
#and the volume of the spheres is proportional to the number of published documents of each theme.

######
#STEP 3: THEME ANALYSIS FOR CAMERA TRAP
######
#Lets exclude all "review" type studies
table (M$DT)
#Choose only "review" articles
tmp001.m <- M[grep("REVIEW", M$DT), ]
dim(tmp001.m)
#54 43
#Now delete this subset from the original data frame
M2<- M[!(M$DT %in% tmp001.m$DT),]
dim(M2)
#24152  43
txt1.m <- M2[,c("UT","AB", "my.topic")] 
txt1.m$AB <- tolower(txt1.m$AB)
txt1.m$my.topic <- tolower(txt1.m$my.topic)
txt1.m_corpus <- corpus(txt1.m, docid_field = "UT", text_field = "AB")

txt1.m_corpus #txt1_corpus is the name of the corpus created.
head(docvars(txt1.m_corpus))
toks.m <- tokens(txt1.m_corpus, remove_punct = TRUE, remove_numbers = TRUE)
head(toks.m[[2]], 5) #show the first 5 tokens in the second document.

nostop_toks.m <- tokens_select(toks.m, stopwords('en'), selection = 'remove')
nostop_toks.m <- tokens_select(nostop_toks.m, c("abstract", "study","the", "therefore",
                                            "elsevier", "often", "based", "new", "due", 
                                            "two", "use", "used", "km", "2", "24",
                                            "also", "may", "one", "within", "results",
                                            "found", "however", "many", "elsewhere",
                                            "n", "can", "camera", "trap", "camera-trap" ), 
                             selection = 'remove')
head(nostop_toks.m[[3]], 5)
nostop_toks.m <- tokens_wordstem(nostop_toks.m, 
                               language = quanteda_options("language_stemmer"))
head(nostop_toks.m[[3]], 5)
nostop_toks.m<- tokens_ngrams(nostop_toks.m, n=2) #for bigram
head(nostop_toks.m[[1]], 10) #show the first 10 bigram

#Unify similar concepts
my_thesaurus1 <- dictionary(list(protect_area = c("nation_park", "protect_area"),
                                 large_mammals = c("large_carnivor", "large_mammal", "red_fox", "larg_mammal", "larg_carnivor",
                                                   "snow_leopard", "wild_boar"),
                                 densiti_estim = c("densiti_estim", "popul_densiti", "popul_densiti", "estim_densiti"),
                                 occup_model = c("detect_probabl", "occup_model")))

#DFM
my_dfm.m <- dfm(nostop_toks.m, thesaurus = my_thesaurus1)
my_dfm.m
new_dfm.m <- dfm_trim(my_dfm.m, min_termfreq = 50)
new_fcm.m <- fcm(new_dfm.m) 

feat.m <- names(topfeatures(new_fcm.m, 50))
new_fcm.m <- fcm_select(new_fcm.m, feat.m)

size.m <- log(colSums(dfm_select(new_dfm.m, feat.m)))
textplot_network(new_fcm.m, min_freq = 0.5, vertex_size = size/max(size) * 3)
topfeatures(my_dfm.m, 10)
#Plot top keywords and produce a wordcloud of top keywords.
freq <- textstat_frequency(new_dfm.m, n = 25)
new_dfm.m %>% 
  textstat_frequency(n = 20) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

textplot_wordcloud(new_dfm.m, max_words = 100,
                   random.order=FALSE, rot.per=0.35, 
                   colors=brewer.pal(8, "Dark2"))
#LDA model
dtm.m <- convert(new_dfm.m, to = "topicmodels")
lda.m <- LDA(dtm.m, k = 8) # set the number of topics to 6.
terms(lda.m, 20) #show top 10 words pertaining to each topic
#Obtain the most likely topics for each document
docvars(new_dfm.m, 'topic') <- topics(lda.m) 
head(topics(lda.m), 5) #show topic allocation for the first docucments
#lists the document to (primary) topic assignments:
my.rslt.m <- topics(lda.m)
table(my.rslt.m)
#  1   2   3   4   5   6   7   8 
# 369 203 233 372 267 252 224 266 

m_topics <- tidy(lda.m, matrix = "beta")
m_topics

m_top_terms <- m_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

m_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
   scale_x_reordered()

as.data.frame(m_top_terms)

str(lda.m)
my.model.m <- lda.m@beta
distance_matrix.m <- dist(my.model.m, method="euclidean")
plot(hclust(distance_matrix.m), cex = 1)

#Document-topic probabilities
lda_gamma.m <- tidy(lda.m, matrix = "gamma")
lda_gamma.m
#How are the probabilities distributed? Let’s visualize them
boxplot(lda_gamma.m$gamma ~ lda_gamma.m$topic)
#Another way to see this is:
ggplot(lda_gamma.m, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

ggplot(lda_gamma.m, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

#Thematic map according to author keywords
Map.m = thematicMap(M2, field = "DE", n = 300, minfreq = 5,
                    stemming = FALSE, 
                    size = .5, n.labels=2, 
                    repel = FALSE)

plot(Map.m$map)
#It performs a Thematic Evolution Analysis based on co-word network analysis
#and clustering. The methodology is inspired by the proposal of Cobo et al. (2011).
nexus.m <- thematicEvolution(M2, field="DE", years=c(2005, 2010), n = 200, minFreq=2)
plotThematicEvolution(nexus.m$Nodes, nexus.m$Edges)




######
#STEP 4: NETWORK ANALYSIS
######
#Convert to a data frame
my.rslt2 <- as.data.frame(as.table(my.rslt))
colnames(my.rslt2)<- c("UT", "lda_topic")
str(my.rslt2)

#Now get back to the complete collection of literature review
#and combine it
n3 <- merge(my.data, my.rslt2, by = "UT", all.x = T)
summary(n3$lda_topic)
dim(n3)
#3544   46
#######
#Network analysis
#We will analyse the complete data set to evaluate
#how frequently are camera trap cited in conservation planning papers
#We will use "histNetwork" fuction creates a historical citation network 
#from a bibliographic data frame.
#Historiograph - Direct citation linkages
histResults2 <- histNetwork(n3, min.citations = 1, sep = ";")
options(width = 300)
histPlot(histResults2, n=20, size = 5, labelsize = 2)

#Extract the network matrix from the object so we can have more control
#of plotting parameters
net <- histResults2$NetMatrix
str(net)

#Or we should create a Reference co-citation network?
NetMatrix <- biblioNetwork(n3, analysis = "co-citation", 
                           network = "references", sep = ";")
networkPlot(NetMatrix, normalize = NULL, 
                weighted= TRUE, n = 100, label = "",
            labelsize=0.7,
                edgesize = 5,
                curved=TRUE, Title = "A Co-citation Network of references", 
                type = "mds", size= TRUE, remove.multiple=TRUE)

net <- NetMatrix

#Now convert to igraph object
my.net <-graph_from_adjacency_matrix(net, mode = "directed")
E(my.net)$weight

#Examine attributes
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
                     lda.topic = as.character(n3$lda_topic))

my.net2 <- my.net %>%
  set_vertex_attr(., 
                  name = 'my.topic', 
                  index = V(my.net), 
                  value = sapply(V(my.net)$name, function(x){
                    tmp001 %>%
                      filter(id == x) %>%
                      .$my.topic
                  })) %>%
  set_vertex_attr(., 
                  name = 'lda.topic', 
                  index = V(my.net), 
                  value = sapply(V(my.net)$name, function(x){
                    tmp001 %>%
                      filter(id == x) %>%
                      .$lda.topic
                  })) 
#vertex_attr(my.net2)
vertex_attr_names(my.net2)
#This result in a list of topics, we need transform this list
#in a vector and add as vertex attribute in the matrix
#Asign the new atribute as a vector
my.vector1 <- unlist(V(my.net2)$my.topic)
my.vector2 <- unlist(V(my.net2)$lda.topic)

vertex_attr(my.net2, "my.topic2", index = V(my.net2)) <-sapply(my.vector1, paste0, collapse="")
str(V(my.net2)$my.topic2)

vertex_attr(my.net2, "lda.topic2", index = V(my.net2)) <-sapply(my.vector2, paste0, collapse="")#unlist(c(my.vector2))
str(V(my.net2)$lda.topic2)

#Now calculate network metrix
#Densidity
edge_density(my.net2, loops=F)
#Reciprocity by vertex
reciprocity(my.net2)
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

edge_attr(my.net2)

#Plot the network
# Remove loops
my.net2 <- simplify(my.net2, remove.multiple = T, remove.loops = T)
#Delete isolated vertices
table(degree(my.net2))
Isolated = which(degree(my.net2)<10)
my.net3 = delete.vertices(my.net2, Isolated)

# Set vertex color based on topic research:
igraph :: V(my.net3)$color = igraph :: V(my.net3)$my.topic2 
igraph :: V(my.net3)$color = gsub("camera_trap", "green", igraph :: V(my.net3)$color)
igraph :: V(my.net3)$color = gsub("cons_plan", "pink",igraph :: V(my.net3)$color)

# Set vertex color frame based on topic research:
igraph :: V(my.net3)$frame.color = igraph :: V(my.net3)$my.topic2 
igraph :: V(my.net3)$frame.color = gsub("camera_trap", "green", igraph :: V(my.net3)$frame.color)
igraph :: V(my.net3)$frame.color = gsub("cons_plan", "pink",igraph :: V(my.net3)$frame.color)

# Set vertex shape based on my.topic research:
#igraph :: V(my.net3)$shape = igraph :: V(my.net3)$lda.topic2  

#change arrow size and edge color:
igraph :: E(my.net3)$arrow.size <- .1
igraph :: E(my.net3)$edge.color <- "gray80"
igraph :: E(my.net3)$width <-  0.7

#Label characteristics
igraph :: V(my.net3)$label = igraph :: V(my.net3)$lda.topic2
V(my.net3)$label.cex = .6 #Label size
#Label color
#igraph :: V(my.net3)$label.color = igraph :: V(my.net3)$my.topic2
#igraph :: V(my.net3)$label.color = gsub("camera_trap", "green", igraph :: V(my.net3)$label.color)
#igraph :: V(my.net3)$label.color = gsub("cons_plan", "grey",igraph :: V(my.net3)$label.color)

V(my.net3)$size = 4

#The best option
plot(my.net3, edge.curved=.1,
     layout = igraph :: layout_nicely,
     display.isolates= FALSE,
     vertex.label = V(my.net3)$lda.topic2)
#vertex.label=ifelse(page_rank(my.net3)$vector > 0.01 , V(my.net3)$label, NA)
#vertex.label = V(my.net3)$lda.topic2
#vertex.label = ifelse(degree(my.net3) > 10, V(my.net3)$label, NA)

#Community detection
#Hierarchical agglomerative method that is designed to run well in large networks.
my.ceb <- cluster_fast_greedy(my.net3)
#How many paper by groups?
membership(my.ceb)
str(my.ceb)
with(my.ceb, tapply(names, membership, function(x) length(unique(x))))

plot(my.ceb, my.net3, 
     vertex.label = NA, 
     layout = layout_nicely)
########
colors=brewer.pal(length(my.ceb),'Set1') #make a color palette
V(my.net3)$color=colors[membership(my.ceb)] #assign each vertex a color based on the community assignment

set.seed(2)
plot(my.net3, vertex.label="", vertex.label = V(my.net3)$lda.topic2)
#####
plot_dendrogram(my.ceb, use.modularity = T, 
                mode="hclust",
                cex = 0.15,
                lwd= 0.3)

#Another option
hcd <- as.dendrogram(my.ceb)

plot(cut(hcd, h = 100)$upper, 
     main="Upper tree of cut at h=1500",
     lab.cex = 0.1)

par(mfrow=c(3,1), cex = 0.5)

plot(cut(hcd, h = 400)$lower[[2]], 
     main="First branch of lower tree with cut at h=500")


######
#STEP 4: ADDITIONAL ANALYSIS
######
my.way <-data.frame(lda.topic = V(my.net2)$lda.topic2,
                    betw = V(my.net2)$betw,
                    degree = V(my.net2)$degree,
                    my.topic = V(my.net2)$my.topic2,
                    my.name = V(my.net2)$name)

#Create a data frame with memberships
my.memb <- as.data.frame(as.table(membership(my.ceb)))
str(my.memb)
colnames(my.memb) <- c("my.name", "membership")

#Combine both data frames
my.way2 <- merge(my.way, my.memb, by = "my.name")
dim(my.way2)

#Drop "NA" level in LDA topics
table(my.way2$lda.topic)

my.way3 <- subset(my.way2, lda.topic != "NA")
droplevels(my.way3$lda.topic)
str(my.way3)
table(my.way3$lda.topic, my.way3$my.topic)

plot(my.way3$my.topic, my.way3$membership)

p<-ggplot(my.way3, aes(membership, my.topic, fill = my.topic)) +
  geom_bar(stat="identity") +
  facet_wrap(~ lda.topic)
p

sp <- ggplot(my.way3, aes(degree, betw, fill = my.topic))
sp <- sp + geom_point(aes(color = my.topic)) +
  theme(legend.position = "top")+
  scale_color_grey(start = 0.2, end = 0.7)+
  geom_text(
    label= my.way3$my.name, 
    nudge_x = 0.25, nudge_y = 0.25, cex = 2,
    check_overlap = T
  )
sp

#############################
#Create a co-citation network of authors.
NetMatrix <- biblioNetwork(M, analysis = "collaboration", 
                           network = "authors", sep = ";")

net=networkPlot(NetMatrix, normalize = NULL, 
                weighted= TRUE, n = 30, labelsize=0.7,
                edgesize = 10,
                curved=TRUE, Title = "A Co-citation Network of Authors", 
                type = "fruchterman", size= TRUE, remove.multiple=TRUE)

######
#Create a collaboration network of universities.
NetMatrix1 <- biblioNetwork(M, analysis = "collaboration", 
                            network = "universities", sep = ";")

net=networkPlot(NetMatrix1, normalize = "salton", weighted=T, 
                n = 20, labelsize=0.9,
                curved=TRUE, edgesize = 10,
                Title = "A Collaboration Network of Universities", 
                type = "fruchterman", size=TRUE,remove.multiple=TRUE)

#Create a collaboration network of countries.
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

NetMatrix2 <- biblioNetwork(M, analysis = "collaboration", 
                            network = "countries", sep = ";")

net=networkPlot(NetMatrix2, normalize = "salton", weighted=T, 
                n = 30, edgesize = 9,
                labelsize=0.9, curved=TRUE,
                Title = "A Collaboration Network of Countries",
                type = "circle", size=TRUE, remove.multiple=TRUE)

#Here is the keyword co-occurence network.
#Based on top 30
NetMatrix3 <- biblioNetwork(M, analysis = "co-occurrences", 
                            network = "keywords", sep = ";")

net=networkPlot(NetMatrix3, normalize="association", weighted= T,
                n = 30, curved=TRUE, remove.multiple = T,
                Title = "A Keyword Co-occurrence Network", 
                type = "circle", size=T, edgesize = 9, edges.min = 0.9,
                labelsize=0.9)


########
#CONTENT ANALYSIS
########
#Co-word Analysis through Correspondence Analysis
CS <- conceptualStructure(M, method="MCA", field="DE", 
                          minDegree=10, clust=5, 
                          stemming=FALSE, labelsize=8,documents=20)

#Thematic map according to author keywords
Map=thematicMap(M, field = "DE", n = 600, minfreq = 5,
                stemming = FALSE, size = .5, n.labels=2, repel = FALSE)
plot(Map$map)

#Thematic map is a very intuitive plot and we can analyze themes according
#to the quadrant in which they are placed: 
#(1) upper-right quadrant: motor-themes; 
#(2) lower-right quadrant: basic themes; 
#(3) lower-left quadrant: emerging or disappearing themes; 
#(4) upper-left quadrant: very specialized/niche themes.
#Cobo, M. J., L?pez-Herrera, A. G., Herrera-Viedma, E., & Herrera, F. 
#(2011). An approach for detecting, quantifying, and visualizing the 
#evolution of a research field: A practical application to the fuzzy 
#sets theory field. Journal of Informetrics, 5(1), 146-166.

