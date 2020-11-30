#!R --vanilla
##paquetes necesarios
require(unmarked)
require(AICcmodavg)
require(chron)
require(raster)
require(cluster)

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

## lack of fit
load(sprintf("%s/Rdata/occuRN/P.concolor.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/P.tajacu.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/C.unicinctus.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/H.hydrochaeris.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/L.wiedii.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/O.virginianus.rda",script.dir))
## c-hat >1 and large standard errors, not useful (few detections)
load(sprintf("%s/Rdata/occuRN/D.marsupialis.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/M.tridactyla.rda",script.dir))



## c-hat >1 and large standard errors, still useful?
load(sprintf("%s/Rdata/occuRN/N.nasua.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/C.olivaceus.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/D.novemcinctus.rda",script.dir))

## c-hat >1, use QAICc
load(sprintf("%s/Rdata/occuRN/P.onca.rda",script.dir))

## c-hat <1, good
load(sprintf("%s/Rdata/occuRN/C.alector.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/L.rufaxilla.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/D.kappleri.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/L.pardalis.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/C.paca.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/D.leporina.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/C.thous.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/D.imperfecta.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/E.barbara.rda",script.dir))

## c-hat <1, so la la
load(sprintf("%s/Rdata/occuRN/M.americana.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/M.gouazoubira.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/P.maximus.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/T.terrestris.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/T.tetradactyla.rda",script.dir))

ts02
# if c.hat <= 1
oms <- dredge(fm01,rank="AICc")
# if c.hat > 1
#
oms <- dredge(fm01,rank="QAICc",chat=ts02$c.hat.est)

summary(model.avg(oms, subset = delta < 10))

avgmod.95p <- model.avg(oms, cumsum(weight) <= .95,fit=T)
prd <- predict(avgmod.95p,type='state')

#  The ‘subset’ (or ‘conditional’) average only averages over the
     # models where the parameter appears. An alternative, the ‘full’
     # average assumes that a variable is included in every model, but in
     # some models the corresponding coefficient (and its respective
     # variance) is set to zero.  Unlike the ‘subset average’, it does
     # not have a tendency of biasing the value away from zero. The
     # ‘full’ average is a type of shrinkage estimator, and for variables
     # with a weak relationship to the response it is smaller than
     # ‘subset’ estimators.


summary(avgmod.95p)
aggregate(prd$fit,list(UMF@siteCovs$bloque),sum)
 aggregate(prd$fit,list(UMF@siteCovs$caza.celda),mean)
boxplot(prd$fit~UMF@siteCovs$caza.celda,varwidth=T)


#modavg(list(fm00,fm01),modnames=c("B","B+C"),c.hat= ifelse(ts02$c.hat.est<1,1,ts02$c.hat.est), parm='dcon',parm.type='lambda')



aggregate(confint(ranef(fm01,K=50), level=0.95),list(UMF@siteCovs$bloque),sum)
aggregate(confint(ranef(fm01,K=50), level=0.95),list(UMF@siteCovs$caza.celda),sum)

#dredge(budworm.lg, rank = "QAICc", chat = chat)
#dredge(budworm.lg, rank = "AIC")







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
