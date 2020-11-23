#!R --vanilla
##paquetes necesarios
require(unmarked)
require(AICcmodavg)
require(chron)
require(raster)

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

camaras$bsq <- extract(vbsq,camaras[,c("lon","lat")])
x <- extract(dist.comunidades,camaras[,c("lon","lat")])
camaras$dcom <- (x-mean(x))/sd(x)
x <- extract(dist.conucos,camaras[,c("lon","lat")])
camaras$dcon <- (x-mean(x))/sd(x)

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

## this is too sparse, c hat values remain too low
semanas <- ini + seq(from=7,by=7,length.out=28)
semanas <- ini + seq(from=0,to=210,by=21)
eventos$sessions <- cut(eventos$fecha,breaks=semanas,label=as.character(semanas)[-1])
table(eventos$sessions,useNA='always')

camaras$cdg <- as.character(camaras$ID.original) # paste(camaras$bloque,camaras$camera)
eventos$cdg <- as.character(camaras$ID.original)[match(paste(eventos$bloque,eventos$periodo,eventos$camara), paste(camaras$bloque,camaras$period,camaras$camera))]

mtz <- matrix(0,nrow=length(unique(camaras$cdg)),ncol=length(semanas)-1,dimnames=list(unique(camaras$cdg),as.character(semanas)[-1]))

for (k in 1:nrow(camaras)) {
   mtz[ camaras[k,"cdg"],] <-    mtz[ camaras[k,"cdg"],] + table(cut(seq(fecha1[k],fecha2[k],by=1),breaks=semanas,label=as.character(semanas)[-1]))
}
# este tiene un error:
 mtz["Is10",] <-  mtz["Is10",]/3

sfrz <- mtz
mtz[mtz==0] <- NA
 mtz <- mtz*0

ss <- rowSums(is.na(mtz))!=ncol(mtz)

x <-as.numeric(semanas[-1])
x <- (x-mean(x))/sd(x)
obsDate <- matrix(rep(x,sum(ss)),nrow=sum(ss),byrow=T)


 obs <- mtz

 sC <- data.frame(camaras[match(rownames(obs),camaras$cdg),c("bloque","H","h","dcon","dcom","caza.bloque","caza.celda","caza.celda2","bsq")])
  sC$bloque <- droplevels(sC$bloque)
  sC$h <- (sC$h-mean(sC$h))/sd(sC$h)
  sC$H <- (sC$H-mean(sC$H))/sd(sC$H)

 mi.spp <- "C.alector"
 mi.spp <- "E.barbara"
 mi.spp <- "C.paca"
 sort(table(eventos$species))
 mi.spp <- "D.leporina"
 mi.spp <- "L.rufaxilla"

 nsim.val <- 10000 # change to 1000 for manuscript results

for (mi.spp in  sample(levels(droplevels(eventos$species)))) {

   obs <- mtz

  for (k in  seq(along=eventos$species)[eventos$species ==mi.spp]) {
     obs[eventos[k,"cdg"],eventos[k,"sessions"]] <-    obs[eventos[k,"cdg"],eventos[k,"sessions"]] + eventos[k,"number.of.animals"]
  }


 UMF <- unmarkedFrameOccu((obs[ss,]>0)+0,
 siteCovs=sC[ss,,drop=F],
 obsCovs=list(date=obsDate[ss,],sfrz=sfrz[ss,]/21))
 mi.rda <- sprintf("%s/Rdata/occuRN/%s.rda",script.dir,mi.spp)
  if (!file.exists(mi.rda)) {
    fm00 <- occuRN(~ sfrz+dcom+date ~ H+bsq, UMF,K=30)
    fm01 <- occuRN(~ sfrz+dcom+date ~ H+bsq+dcon, UMF,K=30)

    ts02 <- mb.gof.test(fm01,nsim=nsim.val,maxK=30)
    save(file=mi.rda,UMF,fm00,fm01,ts02)
  }

  mi.rda <- sprintf("%s/Rdata/occu/%s.rda",script.dir,mi.spp)
  if (!file.exists(mi.rda)) {
   fm00 <- occu(~ sfrz+dcom+date ~ H+bsq, UMF,linkPsi= "cloglog")
   fm01 <- occu(~ sfrz+dcom+date ~ H+bsq+dcon, UMF,linkPsi= "cloglog")

   ts02 <- mb.gof.test(fm01,nsim=nsim.val)
   save(file=mi.rda,UMF,fm00,fm01,ts02)
  }
}
