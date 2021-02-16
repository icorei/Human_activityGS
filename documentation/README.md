
Para procesar los archivos r-markdown desde R:

```r

 R --vanilla
 require(knitr)
 script.dir <- Sys.getenv("SCRIPTDIR")
 mi.arch <- sprintf("%s/R/Occupancy_models/supplementary-methods-2.Rmd",script.dir)
 mi.arch <- sprintf("%s/R/Occupancy_models/supplementary-methods-1.Rmd",script.dir)
 knitr::opts_chunk$set(warning = FALSE, echo = TRUE, eval = TRUE)

 rmarkdown::render(mi.arch,"all")

```

O procesar directamente en R-studio
