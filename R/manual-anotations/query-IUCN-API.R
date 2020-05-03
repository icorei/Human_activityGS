#!R --vanilla --args ~/proyectos/IVIC/the-big-picture

args <- commandArgs(TRUE)
if (!exists("script.dir")) {
  if (!is.na(args[1])) {
    script.dir <- args[1]
  } else {
    script.dir <- readline(prompt="Enter path to script directory: ")
  }
}
require(jsonlite)
require("RPostgreSQL")
library(googlesheets4)

work.dir <- sprintf("%s/R/bibliometric-analysis", script.dir)
Rdata.dir <- sprintf("%s/Rdata", script.dir)

setwd(work.dir)

mi.tkn <- readLines("~/.IUCNAPItoken")


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "IUCN",
                 host = "terra", port = 5432,
                 user = "jferrer")

gs.table <- read_sheet("1tNYpap7JJfXzoMebttLmpWwhdBN3mwt2GOcWh8oIpQk",sheet="General information")
require(stringr)

spp.names <- unique(unlist(strsplit(gs.table$species,":")))
spp.names <- unique(gsub("  "," ",str_trim(spp.names)))

qry <- "SELECT DISTINCT taxonid,scientific_name,main_common_name,category FROM rlts_species"
spp.info <- dbGetQuery(con,qry)
listos <- tolower(c(spp.info$scientific_name,spp.info$main_common_name))
spp.names <- subset(spp.names,!tolower(spp.names) %in% listos)
for (spp in sample(spp.names)) {
  print(spp)
  spp.es <- postgresqlEscapeStrings(con, spp)
  lista1 <- fromJSON(sprintf("http://apiv3.iucnredlist.org/api/v3/species/%s?token=%s",spp.es,mi.tkn))
  if (!is.null(nrow(lista1$result))) {
    qry <- with(lista1$result,sprintf("INSERT INTO rlts_species VALUES(%s,
      '%s','%s','%s','%s','%s','%s','%s','%s','%s',
      %s,
      '%s','%s','%s','%s',
      %s,%s,%s,
      '%s','%s',
      '%s','%s','%s','%s','%s','%s',
      '%s','%s','%s','%s') ON CONFLICT (taxonid) DO NOTHING",
    taxonid,
    scientific_name,kingdom,phylum,class,order,family,genus,postgresqlEscapeStrings(con, main_common_name),postgresqlEscapeStrings(con, authority),
    published_year,
    assessment_date,category,criteria,population_trend,
    marine_system,freshwater_system,terrestrial_system,
    assessor,reviewer,
    aoo_km2,eoo_km2,elevation_upper,elevation_lower,depth_upper,depth_lower,
    errata_flag,postgresqlEscapeStrings(con, errata_reason),amended_flag,postgresqlEscapeStrings(con,amended_reason)))
    qry <- gsub("NA,|'NA',","NULL,",qry)
    dbSendQuery(con,qry)
  }
}

qry <- "select * from rlts_spp_threats where taxonid IN (SELECT DISTINCT taxonid from rlts_species)"
threats <- dbGetQuery(con,qry)

table(spp.info$taxonid %in% threats$taxonid)

dbDisconnect(con)
