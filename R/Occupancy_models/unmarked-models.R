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


## c-hat >1 and large standard errors, not useful (few detections)
load(sprintf("%s/Rdata/occuRN/O.virginianus.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/L.wiedii.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/D.marsupialis.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/C.olivaceus.rda",script.dir))

ts02
oms <- dredge(fm01,rank="QAICc",chat=ts02$c.hat.est)
summary(model.avg(oms, subset = delta < 10))

## c-hat >1, use QAICc
load(sprintf("%s/Rdata/occuRN/P.onca.rda",script.dir))

ts02
oms <- dredge(fm01,rank="QAICc",chat=ts02$c.hat.est)
summary(model.avg(oms, subset = delta < 10))

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


ts02
# if c.hat <= 1
oms <- dredge(fm03,rank="AICc")
# if c.hat > 1
#

summary(model.avg(oms, subset = delta < 10))

## c-hat <1, so la la
load(sprintf("%s/Rdata/occuRN/M.americana.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/M.gouazoubira.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/P.maximus.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/T.terrestris.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/T.tetradactyla.rda",script.dir))

load(sprintf("%s/Rdata/occuRN/M.tridactyla.rda",script.dir))
load(sprintf("%s/Rdata/occuRN/N.nasua.rda",script.dir))

load(sprintf("%s/Rdata/occuRN/D.novemcinctus.rda",script.dir))

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
