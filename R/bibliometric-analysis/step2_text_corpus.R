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


#Look at document types
table (ISI.search.df$DT)

# Subset of the dataframe with following filters:
# * exclude reviews and
# * include all from camera trap search

ISI.camera.ss <- subset(ISI.search.df, !grepl("REVIEW",DT) & my.topic %in% "cameratrap")

dim(ISI.camera.ss)
#2415   43

#First, extract and clean the abstract text and create a corpus

ISI.camera.txt <- ISI.camera.ss[,c("UT","AB", "my.topic")] #take two columns from the dataframe N2.
#Column UT is for Unique Article Identifier, and column AB is for abstracts.

#convert to lower case
ISI.camera.txt$AB <- tolower(ISI.camera.txt$AB)

## remove copyright text
ISI.camera.txt$AB <- gsub("\\(c\\) [0-9 A-Za-z.-]+", "", ISI.camera.txt$AB)
## remove some mathematical notation
ISI.camera.txt$AB <- gsub("-|\\+|<|_|=|`", "", ISI.camera.txt$AB)

ISI.camera.txt$my.topic <- tolower(ISI.camera.txt$my.topic)

#Create a corpus in which each abstract is a document
#keep the UT as document identifier
ISI.camera.corpus <- corpus(ISI.camera.txt, docid_field = "UT", text_field = "AB")

ISI.camera.corpus
head(docvars(ISI.camera.corpus))


#Tokenization by word
#Notice that we remove any remaining punctuation and numbers along the way
temp_toks <- tokens(ISI.camera.corpus, remove_punct = TRUE, remove_numbers = TRUE)
head(temp_toks[[2]], 5) #show the first 5 tokens in the second document.

#Remove stop words and a customized list of filter words.
nostop_toks <- tokens_select(temp_toks, stopwords('en'), selection = 'remove')
nostop_toks <- tokens_select(nostop_toks, c("abstract", "study","the", "therefore",  "elsevier", "often", "based", "new", "due", "two", "use", "used", "km", "2", "24", "also", "may", "one", "within", "results", "found", "however", "many", "elsewhere",  "n", "can", "camera", "trap", "camera-trap", "deutsch", "gesellschaft", "saugetierkund"), selection = 'remove')

head(nostop_toks[[3]], 5)
#Finally > simplify words to avoid confussion with deriv and plural terms
ISI.camera.toks <- tokens_wordstem(nostop_toks, language = quanteda_options("language_stemmer"))
head(ISI.camera.toks[[3]], 5)

#"fragment" "natur"    "environ"  "import"   "threat"


#Create n-gram. (tokens in sequence)
ISI.camera.bigram <- tokens_ngrams(ISI.camera.toks, n=2) #for bigram
head(ISI.camera.bigram[[1]], 10) #show the first 10 bigram



## Word list to be reviewed by @icorei
word.list <- table(unlist(ISI.camera.bigram))
## we compress this using gzip
zz <- gzfile(sprintf("%s/dict/wordlist.csv.gz",script.dir), "w")
write.csv(file=zz,rev(sort(word.list[word.list>1])))
close(zz)


# compressed file
     cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
     close(zz)
     readLines(zz <- gzfile(zzfil))
     close(zz)
     unlink(zzfil)

write.csv(file=sprintf("%s/dict/wordlist.csv",script.dir),rev(sort(word.list[word.list>1])))

# Read dictionay of related terms
camera_thesaurus <- dictionary(file = sprintf("%s/dict/camera_trap.liwc",script.dir), format = "LIWC")

#Create DTM (Document Term Matrix).

my_dfm <- dfm(ISI.camera.bigram, thesaurus = camera_thesaurus)
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
