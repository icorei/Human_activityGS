require(readxl)

CT.review.Izza <- read_xlsx(sprintf("%s/documents/CTresearch_60s-2016.xlsx",script.dir),sheet=3)

table(is.na(CT.review.Izza$doi) ,CT.review.Izza$doi %in% ISI.search.df$DI)

table(is.na(CT.review.Izza$TI) ,CT.review.Izza$TI %in% ISI.search.df$TI)
