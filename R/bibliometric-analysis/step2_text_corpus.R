#! R --vanilla

## read command arguments
args <- commandArgs(TRUE)
script.dir <- args[1]
target.dir <- args[2]
target.grp <- args[3]

## Load required packages
library(bibliometrix) #the library for bibliometrics
require(topicmodels) #for topic modeling
library(quanteda) #a library for quantitative text analysis
require(ggplot2) #visualization
library(dplyr) #for data munging
library("RColorBrewer") # user friendly color palettes

## Set up working environment (customize accordingly...)
work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

## Load data from previous step:
load(file=sprintf("%s/%s.rda",Rdata.dir,target.dir))


#Look at document types
table (ISI.search.df$DT)

# Subset of the dataframe with following filters:
# * exclude reviews and
# * include all from camera trap search

ISI.camera.ss <- subset(ISI.search.df, !grepl("REVIEW",DT) & search.group %in% target.grp)

dim(ISI.camera.ss)

#First, extract and clean the abstract text and create a corpus

ISI.camera.txt <- ISI.camera.ss[,c("UT","AB", "search.group")] #take two columns from the dataframe N2.
#Column UT is for Unique Article Identifier, and column AB is for abstracts.

#convert to lower case
ISI.camera.txt$AB <- tolower(ISI.camera.txt$AB)

## remove copyright text
ISI.camera.txt$AB <- gsub("\\(c\\) [0-9 A-Za-z.-]+", "", ISI.camera.txt$AB)
## remove some mathematical notation
ISI.camera.txt$AB <- gsub("-|\\+|<|_|=|`", "", ISI.camera.txt$AB)


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
#Next > simplify words to avoid confusion with deriv and plural terms
ISI.camera.toks <- tokens_wordstem(nostop_toks, language = quanteda_options("language_stemmer"))

head(ISI.camera.toks[[3]], 5)

#Create n-gram. (tokens in sequence)
ISI.camera.bigram <- tokens_ngrams(ISI.camera.toks, n=2) #for bigram
head(ISI.camera.bigram[[1]], 10) #show the first 10 bigram

## sets of special words to exclude or include

exclude.words <- read.table(file=sprintf("%s/dict/exclude.txt",script.dir),as.is=T)$V1
species.words <- read.table(file=sprintf("%s/dict/species_terms.txt",script.dir),as.is=T)$V1
regions.words <- read.table(file=sprintf("%s/dict/region_terms.txt",script.dir),as.is=T)$V1

# Dictionay of related terms in LIWC format
camera_thesaurus <- dictionary(file = sprintf("%s/dict/camera_trap.liwc",script.dir), format = "LIWC")

ISI.camera.bigram <- tokens_select(ISI.camera.bigram, exclude.words, selection = 'remove')

## Update word list for checking and improving the thesaurus
word.list <- table(unlist(ISI.camera.bigram))
## most common words
write.csv(file=sprintf("%s/dict/common_wordlist.csv",script.dir), rev(sort(word.list[word.list>20])))

write.csv(file=sprintf("%s/dict/common_not_in_dictionary_wordlist.csv",script.dir), rev(sort(word.list[word.list>20 &
  !(names(word.list) %in% unlist(camera_thesaurus@.Data)) & !(names(word.list) %in% species.words) & !(names(word.list) %in% regions.words)])))

## (almost) all words, we compress this using gzip
zz <- gzfile(sprintf("%s/dict/wordlist.csv.gz",script.dir), "w")
write.csv(file=zz,rev(sort(word.list[word.list>1])))
close(zz)

## save to a Rdata object:
data.set.id <- target.dir
save(file=sprintf("%s/ISI-camera-corpus.rda",Rdata.dir), ISI.camera.corpus, ISI.camera.bigram, camera_thesaurus, data.set.id)

## That's it!, we are ready for the next step.
