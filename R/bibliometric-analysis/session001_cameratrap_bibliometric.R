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
#Use saved search results from Web of Science database
#keyword "illegal wildlife trade"
#The library bibliometrix can convert bibtex files into data frames
#There are over 900 results returned and they are saved into two BibTex files

A1 <- readFiles(sprintf("%s/savedrecs_cameratrap1.bib",data.dir))
A2 <- readFiles(sprintf("%s/savedrecs_cameratrap2.bib",data.dir))
A3 <- readFiles(sprintf("%s/savedrecs_cameratrap3.bib",data.dir))
A4 <- readFiles(sprintf("%s/savedrecs_cameratrap4.bib",data.dir))
A5 <- readFiles(sprintf("%s/savedrecs_cameratrap5.bib",data.dir))

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

#Now we will import the data frame revised by Izzabella and Alex Diment
my.rw<- read.csv(sprintf("%s/CTresearch_60s-2016.csv",data.dir))
dim(my.rw)
#1002   27
#Lets now define our categories of The Conservation Planing Circle
#based on the objective description asignated by Izza and Alex
#Define firts the categories. 
table(my.rw$objective_principal)

for (tt in c("abundance", "demography ", "density", "distribution",
            "diversity", "occupancy", "presence", "richness", "inventory", "survey")) {
  my.rw$objective_principal<-gsub(tt, "status", my.rw$objective_principal)
}

for (tt in c("hunting", "invasion", "land_use_change","nest_predation",
             "poisoning", "predation", "disease")) {
  my.rw$objective_principal<-gsub(tt, "threats", my.rw$objective_principal)
}

for (tt in c("activity_pattern", "age_ratio ", "dispersal", "mortality_rates",
             "popultion dynamic", "reproduction", "sex_ratio")) {
  my.rw$objective_principal<-gsub(tt, "pop_dynamic", my.rw$objective_principal)
}

for (tt in c("community_structure", "ecology", "feeding_ecology", "fruit_production",
             "marine", "migration", "pollination", "seed_pop_dynamic", "seed_threats")) {
  my.rw$objective_principal<-gsub(tt, "ecology", my.rw$objective_principal)
}

for (tt in c("re-establishing", "conservation", "recovery strategy")) {
  my.rw$objective_principal<-gsub(tt, "conservation", my.rw$objective_principal)
}

#Now create binomial variables for each category
my.rw$status<-0
my.rw$conservation<-0
my.rw$ecology<-0
my.rw$pop_dynamic<-0
my.rw$threats<-0

#Status
my.rw$status <- grepl("status", my.rw$objective_principal)
my.rw$status[my.rw$objective_principal==c("TRUE")]<-1
my.rw$status[my.rw$objective_principal==c("FALSE")]<-0

#Conservation
my.rw$conservation <- grepl("conservation", my.rw$objective_principal)
my.rw$conservation[my.rw$objective_principal==c("TRUE")]<-1
my.rw$conservation[my.rw$objective_principal==c("FALSE")]<-0

#Ecology
my.rw$ecology <- grepl("ecology", my.rw$objective_principal)
my.rw$ecology[my.rw$objective_principal==c("TRUE")]<-1
my.rw$ecology[my.rw$objective_principal==c("FALSE")]<-0

#Ecology
my.rw$pop_dynamic <- grepl("pop_dynamic", my.rw$objective_principal)
my.rw$pop_dynamic[my.rw$objective_principal==c("TRUE")]<-1
my.rw$pop_dynamic[my.rw$objective_principal==c("FALSE")]<-0

#Threats
my.rw$threats <- grepl("threats", my.rw$objective_principal)
my.rw$threats[my.rw$objective_principal==c("TRUE")]<-1
my.rw$threats[my.rw$objective_principal==c("FALSE")]<-0


#Combine both dataframe: our automatic search with the validation 
#table developed by Izza and Alex Diment
my.rw2 <- merge.data.frame(M,my.rw, by = "TI", all.x = TRUE)
dim(my.rw2)
#2480  73

#How many publications by year?
my.table<-as.data.frame(rowSums(table(my.rw2$PY, as.numeric(my.rw2$PY))))
#Temporal patterns of publications
my.rw2$PY <- as.character(my.rw2$PY)
tmp <- table(unlist(strsplit(my.rw2$PY," :: ")))
length(tmp)
#23

cites<- data.frame()
tt <- c()
for (aa in names(tmp)[1:23]) {
  cites <- rbind(cites, data.frame(cites1 = sum(my.rw2$TC[grep(aa, my.rw2$PY)])))
  
}

tmp1<-as.data.frame(tmp)

fch1<-as.numeric(levels(tmp1$Var1))[tmp1$Var1]
total.reg<-tmp1$Freq

t.ints<-data.frame(cbind(fch1,total.reg, cites))

#Accumulated number of publications
for (k in 2:2) {
  t.ints[,k] <- cumsum(t.ints[,k])
}
#Omit NAs
t.ints<-na.omit(t.ints)

#Calculate proportions
t.ints$prop<-t.ints$cites1/t.ints$total.reg

#Plot to vizualize temporal publication pattern
plot(t.ints$fch1, t.ints$total.reg, pch=NA, xlim=c(1994,2019), ylim=c(0.0,3000),
     ylab= "Accumaleted publications", xlab= "Year")
lines(t.ints$fch1, t.ints$total.reg, col= "black", lwd= 1)
lines(t.ints$fch1, t.ints$cites1, col= "black", lwd= 1, lty = 2)
legend(1994,2500,c("Total publications","Cites"),lty=c(1,2),
       col=c("black","grey50"), cex=.9)

#Lets apply exclude all "review" type studies
table (my.rw2$DT)
#Choose only "review" articles
tmp001 <- my.rw2[grep("REVIEW", my.rw2$DT), ]
dim(tmp001)
#54 68
#Now delete this subset from the original data frame
my.M<- my.rw2[!(my.rw2$DT %in% tmp001$DT),]
dim(my.M)
#2426   68

#Now we are going to discard those publications
#focused on methodological comparisons and assessment of equipment efficiency
list(my.M$TI)
tmp002 <- my.M %>%
  filter(str_detect(AB, "METHOD|COMPARISON"))

tmp002$TI
dim(tmp002)
#1726  68

#This type of publications should be frequent
#at the first beginning of a discipline. Let see the temporal pattern of methodological
#papers
my.table<-as.data.frame(rowSums(table(tmp002$PY, as.numeric(tmp002$PY$PY))))
#Temporal patterns of publications
tmp002$PY <- as.character(tmp002$PY)
tmp2 <- table(unlist(tmp002$PY))
length(tmp2)
#21
tmp3<-as.data.frame(tmp2)
#Seems that the "methods" topic is yet an interesting issue in CT research

fch1<-as.numeric(levels(tmp3$Var1))[tmp3$Var1]
reg.methods<-tmp3$Freq

t.ints2<-data.frame(cbind(fch1, reg.methods))

#Registros acumulados
for (k in 2:2) {
  t.ints2[,k] <- cumsum(t.ints2[,k])
}

#Plot to visualize
plot(t.ints$fch1, t.ints$total.reg, pch=NA, xlim=c(1994, 2019), ylim=c(0.0, 2500), 
     ylab= "Accumulated publications", xlab= "Year")
lines(t.ints$fch1, t.ints$total.reg, col= "black", lwd= 2)
lines(t.ints2$fch1, t.ints2$reg.methods, col= "blue", lwd= 2)

#Now we'll deleted those methodological publications from our dataframe
Msub<- my.M[!(my.M$TI %in% tmp002$TI),]
dim(Msub)
#1700   68

####################

#Now,apply Natural Language Processing and Topic Modeling to abstracts.
#First, clean the text and create a corpus
txt1 <- Msub[,c("UT","AB", "objective_principal", "status", "conservation")] #take two columns from the dataframe M. 
#Column UT is for Unique Article Identifier, and column AB is for abstracts.
#txt1$UT <- tolower(txt1$TI) #convert to lower case
txt1$AB <- tolower(txt1$AB)
txt1$objective_principal <- tolower(txt1$objective_principal)

#Create a corpus in which each abstract is a document.
#keep the UT as document identifier
txt1_corpus <- corpus(txt1, docid_field = "UT", text_field = "AB")

txt1_corpus #txt1_corpus is the name of the corpus created.
head(docvars(txt1_corpus))
#Tokenization (lexical analysis). 
#Tokenization is the process of splitting a text into tokens
#(i.e. convert the text into smaller, more
#specific text features, such as words or word combinations)
#There are many ways to tokenize text
#(by sentence, by word, or by line). 
#For our data, we tokenize by words.
#notice that we remove punctuation and numbers along the way
toks <- tokens(txt1_corpus, remove_punct = TRUE, remove_numbers = TRUE)

head(toks[[3]], 5) #show the first 5 tokens in the second document.

#Remove stop words and a customized list of filter words.
nostop_toks <- tokens_select(toks, stopwords('en'), selection = 'remove')
nostop_toks <- tokens_select(nostop_toks, c("abstract", "study","the", "therefore",
                                            "elsevier", "often", "based", "new", "due", 
                                            "two", "use", "used", "km", "2", "24",
                                            "also", "may", "one", "within", "results",
                                            "found", "however", "many", "elsewhere",
                                            "camera", "trap", "camera-trap", "n",
                                            "can"), 
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

#Create DTM (Document Term Matrix).
#Commun format for text analysis. A DTM is a matrix in which rows are
#documents, columns are terms, and cells indicate how often 
#each term occurred in each document.
my_dfm <- dfm(nostop_toks)
my_dfm

#Document-feature matrix of: 1,700 documents, 129,255 features features (99.9% sparse).
#There are too much features
#Lets simplify it by 
#create a new dfm to include words that have appeared at least 50 times in the corpus.
new_dfm <- dfm_trim(my_dfm, min_termfreq = 50)
#Better, we have now 91 features

#create a feature co-occurrence matrix
new_fcm <- fcm(new_dfm) 

#extract top 100 keywords based on abstracts and create a feature co-occurrence matrix 
#based on the top 50
feat <- names(topfeatures(new_fcm, 50))
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
######
#Given we know the categories we have 2 options to do
#text classification: 1) dictionary and 2) machine learning
#Lets try with machine learning given we have a set of 
#publication manualy checket and clasified in the "objetive_principal"

#Create a subset with publication focus on species status evaluation
#this will be our training set
train.subet <- dfm_subset(new_dfm, objective_principal != "NA")
#Document-feature matrix of: 416 documents, 91 features (95.9% sparse)
head(docvars(train.subet))

test.subset <- dfm_subset(new_dfm, objective_principal = "NA")
#Document-feature matrix of: 1,700 documents, 91 features (96.3% sparse).
head(docvars(test.subset))

#Fit a Naive Bayes classifier for texts
tmod_nb<- textmodel_nb(train.subet, 
                                  y = docvars(train.subet, "objective_principal"),
                                  prior = "docfreq")
summary(tmod_nb)
predict(tmod_nb)


#Predict to the entire dataset
pred_nb <- predict(tmod_nb, newdata = test.subset, type = "class")
summary(pred_nb)

#Let see how well is the prediction
actual_class <- docvars(test.subset, "objective_principal")
tab_class <- table(actual_class, pred_nb)
tab_class

confusionMatrix(tab_class, mode = "everything")
#We are looking for high values of 
#Sensitivity (true positive rate, the proportion of actual positives that are correctly 
#identified as such), and 
#Specificity (true negative rate, the proportion of actual negatives that are correctly
#identified as such)
#In general we had high values of specificity for all categories, less for "status"
#
#######
#Now get back to our data frame with all the bibliography, and
#add these predicted classes


#####################
######
#Now we'll use topic modeling to clasiffy the articles
dtm <- convert(dict_dfm, to = "topicmodels")
lda <- LDA(dtm, k = 3) # set the number of topics to .
terms(lda, 20) #show top 10 words pertaining to each topic

library(scales)

#Obtain the most likely topics for each document
docvars(dict_dfm, 'topic') <- topics(lda) 
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
  filter(topic1 > .007 | topic3 > .007) %>%
  mutate(log_ratio = log2(topic2 / topic1))

library(plyr)
library(ggplot2)
library(scales)
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

