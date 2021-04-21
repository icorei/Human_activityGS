# _documentation_ folder

This folder contains R-markdown files with a mix of code and comments that can be rendered as a pdf-document for documentation of the analysis output.

The PDF outputs are included as Supplementary Information of the main article.

To render the documents in R:

```r
#!R --vanilla
require(knitr)
script.dir <- Sys.getenv("SCRIPTDIR")
knitr::opts_chunk$set(warning = FALSE, echo = TRUE, eval = TRUE)

for (mi.arch in sprintf("%s/documentation/supplementary-methods-%s.Rmd",script.dir,1:2)) {
  rmarkdown::render(mi.arch,"all")
}

```

Additional output of figures for manuscript:

```r
source(sprintf("%s/documentation/Figures.R",script.dir))

```

Alternatively, use `Rscript`:

```sh
Rscript -e "rmarkdown::render('~/proyectos/IVIC/Human_activityGS/documentation/supplementary-methods-1.Rmd',output_format='pdf_document')"
```

Or use knitr functions in `Rstudio`.

## Notes on citations and references

We are implementing citations using package `knitcitations`, some references had to be entered manually, for example this one might fail due to problems with the provider of the citation record:

```{r}
citep("https://www.elsevier.com/books/applied-hierarchical-modeling-in-ecology-analysis-of-distribution-abundance-and-species-richness-in-r-and-bugs/kery/978-0-12-801378-6")
```
