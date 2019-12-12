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
#Use saved search results from Web of Science database
#keyword "illegal wildlife trade"
#The library bibliometrix can convert bibtex files into data frames
#There are over 900 results returned and they are saved into two BibTex files

A1 <- readFiles(sprintf("%s/savedrecs_cameratrap1.bib",data.dir))
A2 <- readFiles(sprintf("%s/savedrecs_cameratrap2.bib",data.dir))
A3 <- readFiles("savedrecs_cameratrap3.bib")
A4 <- readFiles("savedrecs_cameratrap4.bib")
A5 <- readFiles("savedrecs_cameratrap5.bib")

M1 <- convert2df(A1, dbsource = "isi", format = "bibtex")
M2 <- convert2df(A2, dbsource = "isi", format = "bibtex")
M3 <- convert2df(A3, dbsource = "isi", format = "bibtex")
M4 <- convert2df(A4, dbsource = "isi", format = "bibtex")
M5 <- convert2df(A5, dbsource = "isi", format = "bibtex")

M<- rbind(M1,M2, M3, M4, M5)
#M<- dplyr::bind_rows(M1,M2,M3) #merge the three files
#we used bind_rows because M3 have 41 colunms, while M1 and M2 have 42
dim(M)
#2469   42

#Generate a descriptive analysis of the references.
results <- biblioAnalysis(M, sep = ";")
S<- summary(object=results,k=20,pause=FALSE)

#Plot results
plot(x = results, k = 20, pause = FALSE)

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

#######################

#Now,apply Natural Language Processing and Topic Modeling to abstracts.
#First, clean the text.
txt1 <- M[,c("UT","AB")] #take two columns from the dataframe M. 
#Column TI is for doi, and column AB is for abstracts.
#txt1$TI <- tolower(txt1$TI) #convert to lower case
txt1$AB <- tolower(txt1$AB)

#Create a corpus in which each abstract is a document.
txt1_corpus <- corpus(txt1, docid_field = "UT", text_field = "AB")

txt1_corpus #txt1_corpus is the name of the corpus created.

#Tokenization (lexical analysis). There are many ways to tokenize text
#(by sentence, by word, or by line). 
#For our data, we tokenize by words.
#notice that we remove punctuation and numbers along the way
toks <- tokens(txt1_corpus, remove_punct = TRUE)

head(toks[[2]], 5) #show the first 5 tokens in the second document.

#Remove stop words and a customized list of filter words.
nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')
nostop_toks <- tokens_select(nostop_toks, c("abstract", "study","the", "therefore",
                                            "elsevier", "often", "based", "new", "due", 
                                            "two", "use", "used", "km", "2", "24",
                                            "also", "may", "one", "within", "results",
                                            "found", "however", "many", "elsewhere",
                                            "can"), 
                             selection = 'remove')
head(nostop_toks[[2]], 5)

#Create n-gram. (tokens in sequence)
ngram <- tokens_ngrams(nostop_toks, n=2) #for bigram
head(ngram[[1]], 10) #show the first 10 bigram

#Create DTM (Document Term Matrix).
nostop_toks_dfm <- dfm(nostop_toks)
nostop_toks_dfm

####
#Create a semantic co-occurrence network.
#create a new dfm to include words that have appeared at least 50 times in the corpus.
new_dfm <- dfm_trim(nostop_toks_dfm, min_termfreq = 50)
new_fcm <- fcm(new_dfm) #create a feature co-occurrence matrix

#extract top 50 keywords based on abstracts and create a feature co-occurrence matrix 
#based on the top 50
feat <- names(topfeatures(new_fcm, 50))
new_fcm <- fcm_select(new_fcm, feat)

size <- log(colSums(dfm_select(new_dfm, feat)))
textplot_network(new_fcm, min_freq = 0.5, vertex_size = size/max(size) * 3)

topfeatures(nostop_toks_dfm, 10)

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

######
#Try topic modeling.
dtm <- convert(nostop_toks_dfm, to = "topicmodels")
lda <- LDA(dtm, k = 6) # set the number of topics to 6.
terms(lda, 20) #show top 10 words pertaining to each topic

library(scales)

#Obtain the most likely topics for each document
docvars(nostop_toks_dfm, 'topic') <- topics(lda) 
head(topics(lda), 5) #show topic allocation for the first docucments

#The tidytext package provides this method for extracting the per-topic-per-word 
#probabilities, called  β(“beta”), from the model.
ap_topics <- tidy(lda, matrix = "beta")
ap_topics

#Now we can build the tidy data frame for the keywords. 
#For this one, we need to use unnest() from tidyr, because they are in a 
#list-column.
library(tidyr)

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
  filter(topic1 > .007 | topic2 > .007) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#library(plyr)
#library(ggplot2)
#library(scales)
ggplot(beta_spread, aes(term, log_ratio)) +
  geom_bar(stat = "identity") +
  geom_col(show.legend = TRUE)+
  ylab("log odds ratio (topic3 / topic2)")+
  coord_flip()

#Document-topic probabilities
#Besides estimating each topic as a mixture of words, 
#LDA also models each document as a mixture of topics
#We can examine the per-document-per-topic probabilities, called  
#γ (“gamma”), with the matrix = "gamma" argument to tidy()

lda_gamma <- tidy(lda, matrix = "gamma")
lda_gamma

#How are the probabilities distributed? Let’s visualize them
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))
#There are many values near zero, which means there are many documents that
#do not belong in each topic. Also, there are many values near  γ= 1
#these are the documents that do belong in those topics.
#This distribution shows that documents are being well discriminated as 
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
#When we tried options higher than 24 (such as 32 or 64), the distributions for  
#γ started to look very flat toward  γ= 1;
#documents were not getting sorted into topics very well.

#Build a dataset with authors keywords
ct_keyword <- tibble(id = M$UT, keyword = M$DE) %>%
  unnest(keyword)
ct_keyword

#to change all of the keywords to either lower or upper case to get rid of duplicates
ct_keyword <- ct_keyword %>% 
  mutate(keyword = toupper(keyword))

#Change the format with one token (word, in this case) per row
ct_keyword <- ct_keyword %>% 
  unnest_tokens(word, keyword) %>% 
  anti_join(stop_words)

#Connecting topic modeling with keywords
lda_gamma <- full_join(lda_gamma, ct_keyword, by = c("document" = "id"))

lda_gamma

#Now we can use filter() to keep only the document-topic entries 
#that have probabilities gamma greater than some cut-off value; let’s use 0.9.
top_keywords <- lda_gamma %>% 
  filter(gamma > 0.01) %>% 
  count(topic, word)

top_keywords

top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  ungroup %>%
  mutate(keyword = reorder_within(keyword, n, topic)) %>%
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ topic, ncol = 4, scales = "free")