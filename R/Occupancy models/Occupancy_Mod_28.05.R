####################### sp  ###################################

setwd("D:/PHD_statistic/Occupancy")
data.dir <- "~D:/PHD_statistic/Occupancy"



EventosGranSabana_mam <- read.csv2("D:/PHD_statistic/EventosGranSabana/Eventos_GS_mambird4.csv")
camerasGS <- read.csv2("D:/PHD_statistic/TODO/Cameras_short_2019.csv")

GS <- merge(EventosGranSabana_mam,camerasGS,by.x=c("bloque","periodo","camara"),
            by.y=c("bloque","period","camera"),all.y=T)
tt <- table(paste(GS$bloque,GS$periodo,GS$camara),GS$species)
head(tt)




###2# data from camera traps dividied in weeks 
require(chron)

fecha1 <-chron(dates.=as.character(camerasGS[,"fecha.act"]),
               times.=as.character(camerasGS[,"hora.act"]),
               format = c(dates = "y-m-d", times = "h:m:s"),)
dias.mes <-cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))
dayofyear <- as.numeric(as.character(years(fecha1))) + 
  ((dias.mes[months(fecha1)]+as.numeric(days(fecha1)))/365)

table(cut(dayofyear,seq(1,365,16)))

fecha2 <-chron(dates.=as.character(camerasGS[,"fecha.desact.real"]),
               times.=as.character(camerasGS[,"hora.desact.real"]),
               format = c(dates = "y-m-d", times = "h:m:s"),)
dayofyear <- dias.mes[months(fecha2)]+as.numeric(days(fecha2))
cut(dayofyear,seq(1,365,16))
periodos <- paste(rep(c(2016,2015),c(14,9)),levels(cut(dayofyear,c(seq(1,365,16),366),include.lowest=T)))
periodos <- periodos[c(15:23,1:14)]

periodo.inst <- paste(as.character(years(fecha1)), 
                      cut(dias.mes[months(fecha1)]+as.numeric(days(fecha1)),c(seq(1,365,16),366),include.lowest=T))

table(factor(periodo.inst,levels=periodos[3:16]))
camerasGS$inst <- factor(periodo.inst,levels=periodos[3:16])


periodo.des <- paste(as.character(years(fecha2)), 
                     cut(dias.mes[months(fecha2)]+as.numeric(days(fecha2)),c(seq(1,365,16),366),include.lowest=T))
camerasGS$des <- factor(periodo.des,levels=periodos[3:16])
table(factor(periodo.des,levels=periodos[3:16]))

EventosGranSabana_mam$cdg <- paste(EventosGranSabana_mam$bloque,EventosGranSabana_mam$periodo,EventosGranSabana_mam$camara)

EventosGranSabana_mam$fecha <- with(EventosGranSabana_mam,
                                    
                                    chron(dates.=sprintf("%s/%s/%s",mes,dia,ano),times.=EventosGranSabana_mam$hora.ini),
                                    format = c(dates = "m-d-Y", times = "h:m:s"))
EventosGranSabana_mam$periodo.evento <- paste(as.character(years(EventosGranSabana_mam$fecha)), 
                                              cut(dias.mes[months(EventosGranSabana_mam$fecha)]+as.numeric(days(EventosGranSabana_mam$fecha)),c(seq(1,365,16),366),include.lowest=T))

#require(chron)

fecha1 <-chron(dates.=as.character(camerasGS[,"fecha.act"]),
               times.=as.character(camerasGS[,"hora.act"]),
               format = c(dates = "y-m-d", times = "h:m:s"),)
dias.mes <-cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))
dayofyear <- as.numeric(as.character(years(fecha1))) + 
  ((dias.mes[months(fecha1)]+as.numeric(days(fecha1)))/365)

table(cut(dayofyear,seq(1,365,16)))

fecha2 <-chron(dates.=as.character(camerasGS[,"fecha.desact.real"]),
               times.=as.character(camerasGS[,"hora.desact.real"]),
               format = c(dates = "y-m-d", times = "h:m:s"),)

dayofyear <- dias.mes[months(fecha2)]+as.numeric(days(fecha2))
cut(dayofyear,seq(1,365,16))
periodos <- paste(rep(c(2016,2015),c(14,9)),levels(cut(dayofyear,c(seq(1,365,16),366),include.lowest=T)))
periodos <- periodos[c(15:23,1:14)]

periodo.inst <- paste(as.character(years(fecha1)), 
                      cut(dias.mes[months(fecha1)]+as.numeric(days(fecha1)),c(seq(1,365,16),366),include.lowest=T))

table(factor(periodo.inst,levels=periodos[3:16]))
camerasGS$inst <- factor(periodo.inst,levels=periodos[3:16])


periodo.des <- paste(as.character(years(fecha2)), 
                     cut(dias.mes[months(fecha2)]+as.numeric(days(fecha2)),c(seq(1,365,16),366),include.lowest=T))
camerasGS$des <- factor(periodo.des,levels=periodos[3:16])
table(factor(periodo.des,levels=periodos[3:16]))

EventosGranSabana_mam$cdg <- paste(EventosGranSabana_mam$bloque,EventosGranSabana_mam$periodo,EventosGranSabana_mam$camara)

EventosGranSabana_mam$fecha <- with(EventosGranSabana_mam,
                                    
                                    chron(dates.=sprintf("%s/%s/%s",mes,dia,ano),times.=EventosGranSabana_mam$hora.ini),
                                    format = c(dates = "m-d-Y", times = "h:m:s"))
EventosGranSabana_mam$periodo.evento <- paste(as.character(years(EventosGranSabana_mam$fecha)), 
                                              cut(dias.mes[months(EventosGranSabana_mam$fecha)]+as.numeric(days(EventosGranSabana_mam$fecha)),c(seq(1,365,16),366),include.lowest=T))




############## elecion de especie

with(subset(EventosGranSabana_mam,species %in% "N.nasua"),table(cdg,periodo.evento))

slc <- subset(EventosGranSabana_mam,species %in% "N.nasua")

mtz <- matrix(NA,nrow=nrow(camerasGS),ncol=14)
for (k in 1:nrow(mtz)) {
  mtz[k,as.numeric(camerasGS$inst[k]):as.numeric(camerasGS$des[k])] <- 0
  
}

rownames(mtz) <- paste(camerasGS$bloque,camerasGS$period,camerasGS$camera)
colnames(mtz) <- periodos[3:16]


for (k in 1:nrow(slc)) {
  mtz[slc$cdg[k],slc$periodo.evento[k]]<-  1
  
}



### Model of occupancy of MacKenzie
## grafica
require(unmarked)
UMF <- unmarkedFrameOccu(mtz)
plot(UMF, panels=4)


fm00 <- occu(~ 1 ~ 1, UMF)
ranef(fm00)

# psi solo
logit.psi <- beta[1]
psi <- exp(logit.psi) / (1 + exp(logit.psi))
psi

##3## VARIABLES ##

##bosque
load("D:/PROJECTS/Gran Sabana/Metodologia/redisenomuestral/rasters_GS.rda")
vbsq <- raster("D:/PROJECTS/Gran Sabana/Metodologia/GS_studyarea_Izza/TREE/MOD44B.2010.GS.TREE.tif")

camerasGS$bosque <- extract(vbsq,camerasGS[,c("lon","lat")])

##fuego
camerasGS$fuego <- (camerasGS$fuego.celda)


##caza
camerasGS$caza <- (camerasGS$caza.celda2)

##conuco
camerasGS$conuco <- (camerasGS$conuco.dist.m)
## Covariables sitio
covar3 <- data.frame(bosque=camerasGS$bosque,
                     caza=camerasGS$caza.celda2,
                     fuego=camerasGS$fuego.celda,
                     conuco=camerasGS$ln.conuco.dis,
                     bufer=camerasGS$ln.buf.frag,
                     com=camerasGS$ln.comun)

siteCovs(UMF) <- covar3
##UMF@siteCovs$bosque <- scale(UMF@siteCovs$bosque, center = TRUE, scale = TRUE)


##modelos
fm00 <- occu(~ 1 ~ 1, UMF)
#fm0b <- occu(~ 1 ~ bosque, UMF)
#fm0f <- occu(~ 1 ~ fuego, UMF)
fm0c <- occu(~ 1 ~ caza, UMF)
fm0m <- occu(~ 1 ~ com, UMF)
fm0u <- occu(~ 1 ~ bufer, UMF)
fm0n <- occu(~ 1 ~ conuco, UMF)


fm.uc <- occu(~ 1 ~ bufer+caza, UMF)
#fm.uf <- occu(~ 1 ~ bufer+fuego, UMF)
fm.un <- occu(~ 1 ~ bufer+conuco, UMF)
fm.um <- occu(~ 1 ~ bufer+com, UMF)


fmm0 <- occu(~ com ~ 1, UMF)
fmmu <- occu(~ com ~ bufer, UMF)
#fmcf <- occu(~ caza ~ fuego, UMF)

fmList <- fitList(Null=fm00, 
                  .caza=fm0c, .conuco=fm0n, .com=fm0m, .bufer=fm0u,
                  .bufercaza=fm.uc, .buferconuco=fm.un, com.bufer=fmmu)



# Model selection

modSel(fmList, nullmod="Null")

# Extract coefficients and standard errors
coef(fmList)
SE(fmList)