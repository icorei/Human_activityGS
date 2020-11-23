#!R --vanilla
##paquetes necesarios
require(unmarked)
require(AICcmodavg)
require(chron)
require(raster)

require(MuMIn)

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

## comparar prediccion en caza celda para ver si la caceria se dirige a los sitios de mayor prob. de presencia de la especie
##fm10 <- occu(~ dcom+date ~ H+bsq+caza.celda2, UMF,linkPsi= "cloglog") ## usar prediccion en caza.celda2
##fm11 <- occu(~ dcom+date ~ H+bsq+dcon+caza.celda2, UMF,linkPsi= "cloglog")
##fm10 <- occuRN(~ sfrz+dcom+date ~ H+bsq+caza.celda2, UMF,K=30)
##fm11 <- occuRN(~ sfrz+dcom+date ~ H+bsq+dcon+caza.celda2, UMF,K=30)


plot(UMF, panels=4)


load("proyectos/IVIC/Hunting_in_GS/Rdata/occuRN/C.paca.rda")

AICtab <- aictab(list(fm00,fm01),modnames=c("B","B+C"),c.hat= ifelse(ts02$c.hat.est<1,1,ts02$c.hat.est))
##aictab(list(fm00,fm02),modnames=c("B","B+C2"))
evidence(AICtab)

modavg(list(fm00,fm01),modnames=c("B","B+C"),c.hat= ifelse(ts02$c.hat.est<1,1,ts02$c.hat.est), parm='dcon',parm.type='lambda')

model.avg(list(fm00,fm01))

# if c.hat <= 1
oms <- dredge(fm01,rank="AICc")
# if c.hat > 1
oms <- dredge(fm01,rank="QAICc",chat=ts02$c.hat.est)

summary(model.avg(oms, subset = delta < 10))


aggregate(confint(ranef(fm01,K=30), level=0.95),list(UMF@siteCovs$bloque),sum)
aggregate(confint(ranef(fm01,K=30), level=0.95),list(UMF@siteCovs$caza.celda),sum)

#dredge(budworm.lg, rank = "QAICc", chat = chat)
#dredge(budworm.lg, rank = "AIC")

fmList <- fitList(Null=fm00,
                  .caza=fm01)


# Model selection

modSel(fmList, nullmod="Null")

# Extract coefficients and standard errors
coef(fmList)
SE(fmList)

nsim.val <- 100 # change to 1000 for manuscript results
mb.gof.test(fm01,nsim=nsim.val,maxK=30)

occuRN(~ dcom+date ~ H+bsq+caza.celda2, UMF,K=30)
nsim.val <- 5 # change to 1000 for manuscript results
mb.gof.test(fm00,nsim=nsim.val,maxK=30)

fm00 <- occuRN(~ bloque+date ~ H+bsq, UMF,K=30)

fm00 <- occu(~ bloque+date ~ H+bsq, UMF,linkPsi='cloglog')
fm00 <- occu(~ bloque+date ~ H+bsq, UMF,linkPsi='logit')

fm01 <- occu(~ date ~ conuco+caza.celda2, UMF,linkPsi='cloglog')

fm00 <- occuRN(~ bloque+date ~ bsq, UMF,K=30)
fm01 <- occuRN(~ date ~ H+h+conuco, UMF,K=30)
fm01 <- occuRN(~ date ~ bloque+conuco+caza.celda2, UMF,K=30)
fm01 <- occu(~ date ~ bloque+conuco+caza.celda2, UMF,linkPsi='cloglog')
fm01 <- occu(~ date ~ conuco+caza.celda2, UMF,linkPsi='cloglog')


ranef(fm00,K=30)

nsim.val <- 5 # change to 1000 for manuscript results
mb.gof.test(fm00,nsim=nsim.val,maxK=30)

mb.gof.test(fm01,nsim=nsim.val,maxK=30)






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
