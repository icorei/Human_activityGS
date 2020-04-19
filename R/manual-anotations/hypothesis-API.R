#!R --vanilla
##https://h.readthedocs.io/en/latest/api-reference/v1/#section/Hypothesis-API/Versions
##install.packages(c("httr", "jsonlite", "lubridate"))
require(httr)
require(jsonlite)
options(stringsAsFactors = FALSE)

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
