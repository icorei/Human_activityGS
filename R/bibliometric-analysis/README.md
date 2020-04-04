# Analysis workflow

## Step 1: DATA SETS

### Search Web of Science database

#### data/ISI-20191211

Use saved search results from

Search term for group 1: "camera trap"
Search term for group 2: "conservation planning" and "terrestrial"

The library bibliometrix can convert bibtex files into data frames


## Step 2: CORPUS

 Create corpus
 Clean up (filters, stop words etc)

* exclude reviews

First, clean the text and create a corpus

Column UT is for Unique Article Identifier, and column AB is for abstracts.

Tokenization (lexical analysis): Tokenization is the process of splitting a text into tokens (i.e. convert the text into smaller, more specific text features, such as words or word combinations)

There are many ways to tokenize text (by sentence, by word, or by line). For our data, we tokenize by words. Notice that we remove punctuation and numbers along the way.

Create n-gram. (tokens in sequence)
Unify similar concepts using a thesaurus


#Create DTM (Document Term Matrix).
#Commun format for text analysis. A DTM is a matrix in which rows are
#documents, columns are terms, and cells indicate how often
#each term occurred in each document.
## Step 3: THEME ANALYSIS FOR CAMERA TRAPS PAPERS


Now,apply Natural Language Processing and Topic Modeling to abstracts to identify the topics published in conservation planning


## Step 4: NETWORK ANALYSIS

## Step 5: ADDITIONAL ANALYSIS


## TO DO


* Is it possible to run the Web of science searches through the clarivate APIs? [Check this](https://www.programmableweb.com/api/clarivate-web-science-expanded) and [here](https://clarivate.com/webofsciencegroup/solutions/xml-and-apis/)
