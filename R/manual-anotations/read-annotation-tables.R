#!R --vanilla --args ~/proyectos/IVIC/the-big-picture

args <- commandArgs(TRUE)
if (!exists("script.dir")) {
  if (!is.na(args[1])) {
    script.dir <- args[1]
  } else {
    script.dir <- readline(prompt="Enter path to script directory: ")
  }
}

library(googlesheets4)
require(dplyr)
require("RPostgreSQL")
##https://h.readthedocs.io/en/latest/api-reference/v1/#section/Hypothesis-API/Versions
require(httr)
require(jsonlite)
require(stringr)
library(tidytext)
require(MASS)
require(Hmisc)
require(viridis)

options(stringsAsFactors = FALSE)

work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

## Load data from previous step:
(load(file=sprintf("%s/ISI-search-df.rda",Rdata.dir)))
(load(file=sprintf("%s/ISI-lda.rda",Rdata.dir)))

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "IUCN",
                 host = "terra", port = 5432,
                 user = "jferrer")

#https://googlesheets4.tidyverse.org/articles/googlesheets4.html

# Use ID of document
gs.table <- read_sheet("1tNYpap7JJfXzoMebttLmpWwhdBN3mwt2GOcWh8oIpQk",sheet="IZZA")
gs.IZZA <- subset(gs.table,rowSums(is.na(gs.table))<10)
gs.table <- read_sheet("1tNYpap7JJfXzoMebttLmpWwhdBN3mwt2GOcWh8oIpQk",sheet="ASM")
gs.ADA <- subset(gs.table,rowSums(is.na(gs.table))<10)

gs.table <- read_sheet("1tNYpap7JJfXzoMebttLmpWwhdBN3mwt2GOcWh8oIpQk",sheet="General information")
gs.ADA$UT <- ISI.search.df$UT[match(gs.ADA$cameratrap_studies,ISI.search.df$TI)]
gs.IZZA$UT <- ISI.search.df$UT[match(gs.IZZA$cameratrap_studies,ISI.search.df$TI)]
# Check what, where, etc
dts <- rbind(
	data.frame(TI=substr(gs.ADA$cameratrap_studies,1,20),
  UT=gs.ADA$UT,
		What=grepl("what",gs.ADA$`4WH`),
		Which=grepl("which",gs.ADA$`4WH`),
		Where=grepl("where",gs.ADA$`4WH`),
		When=grepl("when",gs.ADA$`4WH`),
		How=grepl("how",gs.ADA$`4WH`)),
	data.frame(TI=substr(gs.IZZA$cameratrap_studies,1,20),
  UT=gs.IZZA$UT,
		What=grepl("what",gs.IZZA$`4WH`),
		Which=grepl("which",gs.IZZA$`4WH`),
		Where=grepl("where",gs.IZZA$`4WH`),
		When=grepl("when",gs.IZZA$`4WH`),
		How=grepl("how",gs.IZZA$`4WH`)))

## this returns 1 if ALL reviewers include it, and 0.5 if only one out of two does, etc.
 dts %>% group_by(UT) %>% summarise(What=mean(What), Which=mean(Which), Where=mean(Where), When=mean(When), How=mean(How)) %>% print.AsIs()


lda_gamma <- tidy(CT.lda, matrix = "gamma")

dts %>% filter(Where>0) %>% select(UT) -> slc
lda_gamma %>% filter(document %in% slc$UT) %>% group_by(topic) %>% summarise(gamma=sum(gamma)) %>%
ggplot(aes(topic, gamma, fill = factor(topic))) +
geom_col(show.legend = FALSE)  +
coord_flip() +
scale_x_reordered()


## Linear discriminant analysis:
dts <- rbind(
	data.frame(TI=substr(gs.ADA$cameratrap_studies,1,20),
  UT=gs.ADA$UT,WH=gs.ADA$`4WH`),
	data.frame(TI=substr(gs.IZZA$cameratrap_studies,1,20),
  UT=gs.IZZA$UT,WH=gs.IZZA$`4WH`))

y <- dts$WH
x <- CT.lda@gamma[match(dts$UT,CT.lda@documents),]
colnames(x) <- sprintf("CT%02d",1:45)

clr1 <- viridis(5)
v <- varclus(x, similarity="spear")  # spearman is the default anyway
v    # invokes print.varclus
print(round(v$sim,2))
plot(v)

slc <- !duplicated(cutree(v$hclust,h=.5))
x1 <- data.frame(x[,slc])
x1$y <- y
mdl <- lda(y~.,data=x1)
prd <- predict(mdl)
ldahist(data = prd$x[,1], g=prd$class)
plot(prd$x[,1],prd$x[,2],col=clr1[as.numeric(prd$class)],pch=19,cex=1.5)
legend("topright",levels(prd$class),fill=clr1)

subset(mdl$scaling[,1:2],(abs(mdl$scaling[,1])>150 | abs(mdl$scaling[,2])>150) )




CT.topics <- tidy(CT.lda, matrix = "beta")


CT.top_terms <- CT.topics %>%
group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)


CT.top_terms %>% filter(beta>.1 & topic %in% c(2,4,15,18,19,29,41,44)) %>% print.AsIs()










plot(mdl$scaling[,c(1,3)],col="grey77",pch=3)
text(mdl$scaling[,c(1,3)],sprintf("CT%02d",seq(along=x[1,])[slc]))


plot(mdl$scaling[,c(2,4)],col=clr1[as.numeric(as.factor(y))],pch=19,cex=2)

z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
    predict(z, Iris[-train, ])$class

## Need to use more standard vocabulary for this
sort(table(unlist(strsplit(gs.table$cameratrap_data,":"))))



table(rslts$category)
table(rslts$class_name)
table(rslts$order_name)

qry <- sprintf("SELECT * FROM rlts_spp_threats WHERE taxonid IN (%s)", paste(rslts$taxonid,collapse=","))
threats <- dbGetQuery(con,qry)

## check complete consistent names
table(unlist(strsplit(gs.IZZA$`IUCN research`,":")))
table(unlist(strsplit(gs.IZZA$`IUCN action`,":")))
table(unlist(strsplit(gs.IZZA$`IUCN threat`,":")))
table(gsub("  "," ",tolower(str_trim(unlist(strsplit(gs.IZZA$`IUCN threat`,":"))))))
table(gsub("  "," ",tolower(str_trim(unlist(strsplit(gs.ADA$`IUCN threat`,":"))))))

## colaboration networks - countries, universities, funding sources
require(bibliometrix)
M <- metaTagExtraction(ISI.search.df,Field="AU_CO")
##M <- metaTagExtraction(M,Field="FU_CO") # Funding/country no existe
##NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "universities", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
	  net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.5)

		M$AU_CO[1000]
		M$FU[1000]
M$AU_UN[1009]
M$AU1_UN[1009]


mi.url  <- "https://api.hypothes.is/"
mi.path <- "api/search"

mi.grp <- "g6LBbZpQ"
API.token <- readLines("~/.hypothesis.token")
raw.result <- GET(url = mi.url, path = mi.path, query = list(tag = "4WH:Where"),
	add_headers(Accept = "application/vnd.hypothesis.v1+json",
		Authorization = sprintf("Bearer %s",API.token)))

raw.result <- GET(url = mi.url, path = mi.path, query = list(group=mi.grp,tag = "data sources"), add_headers(Accept = "application/vnd.hypothesis.v1+json",
	Authorization = sprintf("Bearer %s",API.token)))


	raw.result <- GET(url = mi.url, path = mi.path, query = list(group=mi.grp, uri="https://link.springer.com/article/10.1007/s10344-011-0533-y"), add_headers(Accept = "application/vnd.hypothesis.v1+json",
		Authorization = sprintf("Bearer %s",API.token)))

raw.result$status_code
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)

  ##Group ID: IUCN RLE: "N2gaGJDg"
## camera trap review group=g6LBbZpQ


mi.url  <- "https://api.hypothes.is/"
mi.path <- "api/annotations"
mi.grp <- "g6LBbZpQ"
API.token <- readLines("~/.hypothesis.token")
for (target.doi in  tolower(  subset(ISI.search.df,s1 & s2 & s3 & s4 & s5)[,c("DI")])) {

}
raw.result <- GET(url = mi.url, path = mi.path, query = list(tags = "SELECTED FOR ANNOTATION",grp=mi.grp,uri=sprintf("http://dx.doi.org/%s",target.doi)),
	add_headers(Accept = "application/vnd.hypothesis.v1+json",
		Authorization = sprintf("Bearer %s",API.token)))

devtools::install_github("mdlincoln/hypothesisr")
target.doi <- "10.1046/j.1523-1739.1997.96135.x"
API.token <- "6879-ihYxdIJqO2gBmRhtTNYZ_knfXHtfj38rDHdPEwX_nfc"
require(hypothesisr)
mi.user <- "acct:jferrer@hypothes.is"
		hs_create(token = API.token,
		uri = sprintf("http://dx.doi.org/%s",target.doi),
		permissions = list(read = "group:g6LBbZpQ"),
		user = mi.user, tags = c("SELECTED FOR ANNOTATION"),
		text = "preliminary selection top 8 matching references")

dbDisconnect(con)
