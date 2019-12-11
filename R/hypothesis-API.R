#!R --vanilla
##https://h.readthedocs.io/en/latest/api-reference/v1/#section/Hypothesis-API/Versions
##install.packages(c("httr", "jsonlite", "lubridate"))
require(httr)
require(jsonlite)
options(stringsAsFactors = FALSE)

mi.url  <- "https://api.hypothes.is/"
mi.path <- "api/search"
API.token <- readLines("~/.hypothesis.token")
raw.result <- GET(url = mi.url, path = mi.path, query = list(tag = "T6.1"),
	add_headers(Accept = "application/vnd.hypothesis.v1+json",
		Authorization = sprintf("Bearer %s",API.token)))

raw.result$status_code
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)

##Group ID: IUCN RLE: "N2gaGJDg"
## camera trap review group=g6LBbZpQ


