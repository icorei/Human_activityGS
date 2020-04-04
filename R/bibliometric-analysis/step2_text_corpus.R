#! R --vanilla

## Load required packages
library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes

## Set up working environment (customize accordingly...)
## FOR ADA:
script.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture"
## FOR JR:
script.dir <- "~/proyectos/IVIC/the-big-picture"

work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

## Load data from previous step:
load(file=sprintf("%s/ISI-20191211.rda",Rdata.dir))


#Lets exclude all "review" type studies
table (ISI.search.df$DT)

# # filters:
# * exclude reviews and
# * include all from camera trap search

M2 <- subset(ISI.search.df, !grepl("REVIEW",DT) & my.topic %in% "cameratrap")

dim(M2)
#2415   43

#First, clean the text and create a corpus

txt1 <- M2[,c("UT","AB", "my.topic")] #take two columns from the dataframe N2.
#Column UT is for Unique Article Identifier, and column AB is for abstracts.
#txt1$UT <- tolower(txt1$TI)
#convert to lower case

txt1$AB <- tolower(txt1$AB)

## remove copyright text
txt1$AB <- gsub("\\(c\\) [0-9 A-Za-z.-]+","",txt1$AB)
## remove some mathematical notation
txt1$AB <- gsub("-|\\+|<|_|=|`","",txt1$AB)

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
nostop_toks <- tokens_select(nostop_toks, c("abstract", "study","the", "therefore",  "elsevier", "often", "based", "new", "due", "two", "use", "used", "km", "2", "24", "also", "may", "one", "within", "results", "found", "however", "many", "elsewhere",  "n", "can", "camera", "trap", "camera-trap", "deutsch", "gesellschaft", "saugetierkund"), selection = 'remove')

head(nostop_toks[[3]], 5)
#Simplify words to avoid confussion with deriv and plural terms
nostop_toks <- tokens_wordstem(nostop_toks, language = quanteda_options("language_stemmer"))
head(nostop_toks[[3]], 5)

#"fragment" "natur"    "environ"  "import"   "threat"
#Create n-gram. (tokens in sequence)
nostop_toks<- tokens_ngrams(nostop_toks, n=2) #for bigram
head(nostop_toks[[1]], 10) #show the first 10 bigram
nostop_toks <- tokens_select(nostop_toks, c("c_deutsch", "saugetierkund_publish", "c_ltd", "publish_gmbh", "gmbh_right", "ltd_right"), selection = 'remove')

## This section will be reviewed by Izza!
head(rev(sort(table(unlist(nostop_toks)))),100)

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

## save to a Rdata object:
#save(file=sprintf("%s/???.rda",Rdata.dir),...)

## That's it!, we are ready for the next step.
