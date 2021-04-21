
Processing r-markdown files in R / Para procesar los archivos r-markdown desde R:

```{r}

 R --vanilla
 require(knitr)
 script.dir <- Sys.getenv("SCRIPTDIR")
 mi.arch <- sprintf("%s/documentation/supplementary-methods-2.Rmd",script.dir)
 knitr::opts_chunk$set(warning = FALSE, echo = TRUE, eval = TRUE)

 rmarkdown::render(mi.arch,"all")

 mi.arch <- sprintf("%s/documentation/supplementary-methods-1.Rmd",script.dir)
 rmarkdown::render(mi.arch,"all")

 source(sprintf("%s/documentation/Figures.R",script.dir))

```

(Otherwise, use R-studio functions / O procesar directamente en R-studio)
