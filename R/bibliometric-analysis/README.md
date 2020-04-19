# Analysis workflow

## Step 1: Read list of references from the literature search

### Web of science searches

We searched the Web of Science database. Ressults were downloaded in bibtex format.

First query was completed in december 2019. Search term for group 1: "camera trap"; and for group 2: "conservation planning" and "terrestrial". This data is in folder [data/ISI-20191211](/data/ISI-20191211)

A second query was completed in april 2020. This data is in folder [data/ISI-20200409](/data/ISI-20200409)

In the script [step10_bibtext_to_Rda.R](step10_bibtext_to_Rda.R) we use the **R** library *bibliometrix* to convert bibtex files into data frames, we specify the path to the script directory (repository home dir) and the target folder as script arguments like this:

```R
## FOR ADA:
Rscript --vanilla R/bibliometric-analysis/step1_bibtex_to_Rda.R ~/Documentos/Publicaciones/Camera-trap Review/the-big-picture data/ISI-20191211

## FOR JR:
Rscript --vanilla R/bibliometric-analysis/step10_bibtex_to_Rda.R ~/proyectos/IVIC/the-big-picture data/ISI-20191211
Rscript --vanilla R/bibliometric-analysis/step10_bibtex_to_Rda.R ~/proyectos/IVIC/the-big-picture data/ISI-20200409
```

In the script [step11_summary_bib_search.R](step11_summary_bib_search.R) we combine this information into a single bibliometric object.


### Crossref search

We used the crossref package to follow references in the cited refernce list of each article.

This is described in script [step12_track_references_with_crossref.R](step12_track_references_with_crossref.R).


### Other searches

The original search for
[Stachowicz's review](/documents/Review_I.Stachowicz_23.03.pdf)
 is in folder [data/IZZA](/data/IZZA).

A script for integrating this data set is under development.

## Step 2: Create text corpus

The second step is to create a text corpus from the list of abstracts, and tokenize this corpus into bigrams.

First we clean up the abstracts, apply filters, extract stop words, etc. We include only a subset of the searches based on the label of the search group ("cameratrap"), exclude all reviews. Column UT is the *Unique Article Identifier*, and column AB is for abstracts.

In lexical analysis *tokenization* is the process of splitting a text into tokens (i.e. convert the text into smaller, more specific text features, such as words or word combinations). There are many ways to tokenize text (by sentence, by word, or by line). For our data, we tokenize by sequence of words (n-gram). Notice that we remove punctuation and numbers along the way, and unify similar concepts using a thesaurus.

In the script [step2_text_corpus.R](step2_text_corpus.R) a couple of **R** libraries to convert the data frame of references into a text corpus, we specify the path to the script directory (repository home dir), the target Rda-file, and the search group label as script arguments like this:


```R
## FOR ADA:
Rscript --vanilla R/bibliometric-analysis/step2_text_corpus.R ~/Documentos/Publicaciones/Camera-trap Review/the-big-picture ISI-20191211 cameratrap
##target.dir<-"ISI-20191211"
##target.grp <- "cameratrap"
##script.dir<-"~/Documentos/Publicaciones/Sanchez-Mercado_Camera-trapReview/the-big-picture"


## FOR JR:
Rscript --vanilla R/bibliometric-analysis/step20_text_corpus.R ~/proyectos/IVIC/the-big-picture ISI-search-df
```

## Step 3: Theme analysis for camera traps papers

For this step of the lexical analysis we use *R markdown* to add comments and output to the script.

```R

R --vanilla --args ~/proyectos/IVIC/the-big-picture


## in Mac OSx
##export RSTUDIO_PANDOC=/Applications/RStudio.app/Contents/MacOS/pandoc
R --vanilla

## Set up working environment (customize accordingly...)
## FOR ADA:
script.dir <- "~/Documentos/Publicaciones/Camera-trap Review/the-big-picture"
## FOR JR:
script.dir <- "~/proyectos/IVIC/the-big-picture"

work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

mi.arch <- 'step3_theme_analysis.Rmd'
rmarkdown::render(sprintf("%s/%s",work.dir,mi.arch), "word_document",output_dir=sprintf("%s/output",script.dir))

```

## Step 4: NETWORK ANALYSIS

## Step 5: ADDITIONAL ANALYSIS


## TO DO


* Is it possible to run the Web of science searches through the clarivate APIs? [Check this](https://www.programmableweb.com/api/clarivate-web-science-expanded) and [here](https://clarivate.com/webofsciencegroup/solutions/xml-and-apis/)
