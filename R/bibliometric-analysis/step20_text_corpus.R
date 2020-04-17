#! R --vanilla

## need to set up three variables
## read command arguments
args <- commandArgs(TRUE)
if (!exists("script.dir")) {
  if (!is.na(args[1])) {
    script.dir <- args[1]
  } else {
    script.dir <- readline(prompt="Enter path to script directory: ")
  }
}
if (!exists("target.dir")) {
  if (!is.na(args[2])) {
    target.dir <- args[2]
  } else {
    target.dir <- readline(prompt="Enter name of target directory: ")
  }
}


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

## load dictionaries:

exclude.words <- read.table(file=sprintf("%s/dict/exclude.txt",script.dir),as.is=T)$V1
species.words <- read.table(file=sprintf("%s/dict/species_terms.txt",script.dir),as.is=T)$V1
regions.words <- read.table(file=sprintf("%s/dict/region_terms.txt",script.dir),as.is=T)$V1

# Dictionay of related terms in LIWC format
camera_thesaurus <- dictionary(file = sprintf("%s/dict/camera_trap.liwc",script.dir), format = "LIWC")

#Look at document types
table (ISI.search.df$DT)


# Subset of the dataframe with following filters:
# * exclude reviews and
# * include all from camera trap search

for (target.grp in unique(ISI.search.df$search.group)) {
  ISI.ss <- subset(ISI.search.df, !grepl("REVIEW",DT) & search.group %in% target.grp)

  #First, extract and clean the abstract text and create a corpus

  ISI.txt <- ISI.ss[,c("UT","AB", "search.group")] #take two columns from the dataframe N2.
  #Column UT is for Unique Article Identifier, and column AB is for abstracts.

  #convert to lower case
  ISI.txt$AB <- tolower(ISI.txt$AB)

  ## remove copyright text
  ISI.txt$AB <- gsub("\\(c\\) [0-9 A-Za-z.-]+", "", ISI.txt$AB)
  ## remove some mathematical notation
  ISI.txt$AB <- gsub("-|\\+|<|_|=|`", "", ISI.txt$AB)


  #Create a corpus in which each abstract is a document
  #keep the UT as document identifier
  ISI.corpus <- corpus(ISI.txt, docid_field = "UT", text_field = "AB")


  #Tokenization by word
  #Notice that we remove any remaining punctuation and numbers along the way
  temp_toks <- tokens(ISI.corpus, remove_punct = TRUE, remove_numbers = TRUE)

  #Remove stop words and a customized list of filter words.
  nostop_toks <- tokens_select(temp_toks, stopwords('en'), selection = 'remove')
  nostop_toks <- tokens_select(nostop_toks, c("abstract", "study","the", "therefore",  "elsevier", "often", "based", "new", "due", "two", "use", "used", "km", "2", "24", "also", "may", "one", "within", "results", "found", "however", "many", "elsewhere",  "n", "can", "camera", "trap", "camera-trap", "deutsch", "gesellschaft", "saugetierkund"), selection = 'remove')

  #Next > simplify words to avoid confusion with deriv and plural terms
  ISI.toks <- tokens_wordstem(nostop_toks, language = quanteda_options("language_stemmer"))

  #Create n-gram. (tokens in sequence)
  ISI.bigram <- tokens_ngrams(ISI.toks, n=2) #for bigram

  # exclude words from a list of meningless phrases
  ISI.bigram <- tokens_select(ISI.bigram, exclude.words, selection = 'remove')

  word.list <- table(unlist(ISI.bigram))

  assign(sprintf("%s.corpus",target.grp),ISI.corpus)
  assign(sprintf("%s.bigram",target.grp),ISI.bigram)
  assign(sprintf("%s.wlist",target.grp),word.list)
  ## save to a Rdata object:
  save(file=sprintf("%s/ISI-%s-corpus.rda", Rdata.dir,  target.grp), list=ls(pattern=target.grp))

}

word.list <- c(CP.wlist,CT.wlist)

word.list <- aggregate(data.frame(Freq=word.list),list(Word=names(word.list)),sum)
word.list <- word.list[rev(order(word.list$Freq)),]

## most common words
write.csv(file=sprintf("%s/dict/common_wordlist.csv",script.dir), subset(word.list,Freq>20))

write.csv(file=sprintf("%s/dict/common_not_in_dictionary_wordlist.csv",script.dir),
subset(word.list,Freq>20 & !(Word %in% unlist(camera_thesaurus@.Data)) & !(Word %in% species.words) & !(Word %in% regions.words)))

## (almost) all words, we compress this using gzip
zz <- gzfile(sprintf("%s/dict/wordlist.csv.gz",script.dir), "w")
write.csv(file=zz,rev(sort(word.list[word.list>1])))
close(zz)

## several terms with deforest... sum up to> 225
subset(word.list,grepl("deforest",Word))
## same for reintroduction> 151
subset(word.list,grepl("reintro",Word))



ISI.corpus
head(docvars(ISI.corpus))
head(temp_toks[[2]], 5) #show the first 5 tokens in the second document.
head(nostop_toks[[3]], 5)
head(ISI.toks[[3]], 5)
head(ISI.bigram[[1]], 10) #show the first 10 bigram





## That's it!, we are ready for the next step.
