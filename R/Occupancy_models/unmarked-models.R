#!R --vanilla
##paquetes necesarios
require(unmarked)
require(AICcmodavg)
require(chron)

## ubicación de la carpeta de trabajo y el repositorio local

if (Sys.getenv("WORKDIR") == "") {
   ## for Izza:
   work.dir <- "D:/PROJECTS/Gran Sabana/Metodologia"
   script.dir <- "???"
} else {
   ## for JR:
   work.dir <- Sys.getenv("WORKDIR")
   script.dir <- Sys.getenv("SCRIPTDIR")
}


## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
load(GIS.data)

eventos <- subset(eventos,bloque %in% sprintf("B%02d",1:6) & !(camara %in% c("RAS","",NA)))
camaras <- subset(camaras,bloque %in% sprintf("B%02d",1:6) )



#### Sampling effort: data from camera traps dividied in weeks

fecha1 <-chron(dates.=as.character(camaras[,"fecha.act"]),
               times.=as.character(camaras[,"hora.act"]),
               format = c(dates = "y-m-d", times = "h:m:s"))

fecha2 <-chron(dates.=as.character(camaras[,"fecha.desact.real"]),
               times.=as.character(camaras[,"hora.desact.real"]),
               format = c(dates = "y-m-d", times = "h:m:s"))

f1 <- with(eventos,chron(dates.=sprintf("%s-%s-%s",ano,mes,dia),
               times.=as.character(hora.ini),
               format = c(dates = "y-m-d", times = "h:m:s")))

f2 <-with(eventos,chron(dates.=sprintf("%s-%s-%s",ano,mes,dia),
               times.=as.character(hora.ini),
               format = c(dates = "y-mon-d", times = "h:m:s"),
               out.format = c(dates = "y-m-d", times = "h:m:s")))


eventos$fecha <-chron(ifelse(is.na(f2),f1,f2),
format = c(dates = "y-m-d", times = "h:m:s"))

ini <- chron(dates.="2015-09-21",times.="00:00:00",format = c(dates = "y-m-d", times = "h:m:s"))

semanas <- ini + seq(from=7,by=7,length.out=28)

eventos$sessions <- cut(eventos$fecha,breaks=semanas,label=as.character(semanas)[-1])
table(eventos$sessions)

camaras$cdg <- paste(camaras$bloque,camaras$camera)
eventos$cdg <- paste(eventos$bloque,eventos$camara)

mtz <- matrix(0,nrow=length(unique(camaras$cdg)),ncol=length(semanas)-1,dimnames=list(unique(camaras$cdg),as.character(semanas)[-1]))

for (k in 1:nrow(camaras)) {
   mtz[ camaras[k,"cdg"],] <-    mtz[ camaras[k,"cdg"],] + table(cut(seq(fecha1[k],fecha2[k],by=1),breaks=semanas,label=as.character(semanas)[-1]))
}
# este tiene un error:
 mtz["B06 IS10",] <-  mtz["B06 IS10",]/3

sfrz <- mtz
mtz[mtz==0] <- NA
 mtz <- mtz*0

obs <- mtz
for (k in  seq(along=eventos$species)[eventos$species =="N.nasua"]) {
   obs[eventos[k,"cdg"],eventos[k,"sessions"]] <-    obs[eventos[k,"cdg"],eventos[k,"sessions"]] + eventos[k,"number.of.animals"]
}
UMF <- unmarkedFrameOccu(obs)
plot(UMF, panels=4)
fm00 <- occuRN(~ 1 ~ 1, UMF)
ranef(fm00)


mb.gof.test(fm00)

UMF <- unmarkedFrameOccu(obs>0)
plot(UMF, panels=4)
fm00 <- occu(~ 1 ~ 1, UMF)
ranef(fm00)


mb.gof.test(fm00)
mb.gof.test(fm00)

 mb.gof.test(fm00,nsim=100)




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
