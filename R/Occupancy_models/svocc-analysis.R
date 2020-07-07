#!R --vanilla
##paquetes necesarios
require(dplyr)
require(tidyr)
require(raster)
##require(gdata)
#require(foreign)
#require(fractaldim)
#require(SDMTools)
require(chron)
require(detect)

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

for (k in dir(sprintf("%s/Rdata",script.dir),pattern="svocc*",full.names=T))
   load(k)


   # ## al menos 12 species con modelos decentes y tres mas con modelos parciales
ccis <- data.frame()
   for (mdl in c(ls(pattern=".full"),ls(pattern=".alt"),ls(pattern=".step"))) {
      fit <- get(mdl)
       nm <- paste(strsplit(mdl,"\\.")[[1]][1:2],collapse=". ")
       d1 <- data.frame(species=nm,var=names(fit$coefficients$sta),coef=fit$coefficients$sta)
       d2 <- cbind(d1,confint(fit,sprintf("sta_%s",d1$var)))
      ccis <- rbind(ccis,data.frame(species=nm,d2))

   }
    sort(table(ccis$var))

   d1 <- subset(ccis,var %in% "bsq")
   d1 <- subset(ccis,var %in% "frs") # no effect
   d1 <- subset(ccis,var %in% "dcon") # two attracted by conucos
   d1 <- subset(ccis,var %in% "dbsq") # one avoids deforested areas
   d1 <- subset(ccis,var %in% "dcom") # two avoid communities
   d1 <- subset(ccis,var %in% "dhum") # one attracted?
   subset(ccis,var %in% "cazTRUE") # pos. but not sign. for P. onca

   d1 <- d1[order(d1$coef),]
   o1 <- 1:nrow(d1)
   c1 <- rep(1,length(o1))
   c1[d1[,6]<0] <- 2
   c1[d1[,5]>0] <- 3

   par(mar=c(4,8,1,1))
   plot(d1$coef,o1,pch=19,col=c1,cex=1.5,axes=F,xlab="coefficient",ylab="",xlim=c(-5,8))
   axis(1)
   axis(2,o1,d1$species,las=2,font=3)
   segments(d1[,5],o1,d1[,6],o1,col=c1)
   abline(v=0,lty=3)


   #
   # Full
   #            Var1 Freq
   # 1          C.paca  344
   # 3         C.thous   94
   # 12   M.tridactyla   23
   #
   # Alt
   # 2      D.leporina  236
   # 4   M.gouazoubira   69
   # 5      D.kappleri   56
   # 6  D.novemcinctus   47
   # 7    T.terrestris   35
   # 14   D.imperfecta   14
   # 13      E.barbara   22
   # 19 T.tetradactyla    6
   # 10         P.onca   27
   #
   # 8      P.concolor   32
   # 16      P.maximus    9
   # 9     M.americana   32

   #
   # 20  O.virginianus    5
   #
   # Faltan
   #            Var1 Freq
   # 11     L.pardalis   24
   # 15  H.hydrohaeris   10
   # 17        N.nasua    8
   # 18    C.olivaceus    8
   # 21       P.tajacu    4
   # 22   C.unicinctus    4
   # 23  D.marsupialis    3
   # 24       T.pecari    2
   # 25       M.pratti    2
   # 26       L.wiedii    2
   # 27    S.venaticus    1
   # 28     L.tigrinus    1
   #
   #


   ## Because of the low information content in binary data, estimation of occupancy and detection from single-survey data is extremely difficult (Welsh, Lindenmayer & Donnelly 2013). As described in Moreno & Lele (2010) and Lele, Moreno & Bayne (2012), one may need to use penalized likelihood function to stabilize the estimators. Unfortunately, it is not clear how to choose a good penalty function in general. In the Supplementary Information, we use a quasi-Bayesian approach where the means of the prior distributions for the parameters are determined from the observations themselves instead of based on a probabilistic quantification of ‘belief’. This seems to stabilize the estimation process considerably, resulting in estimators that are nearly unbiased. Unfortunately, this estimation method lacks a strong theoretical basis.

   ##resource selection probability functions RSPF
   #
   # fit <- svocc(pa ~ bsq + dst | walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = TRUE, method = c( "optim"))
   # m06 <- bootstrap(fit, B=25)
   #      attr(m06, "bootstrap")
   #      extractBOOT(m06)
   #      summary(m06, type="mle")
   #      summary(m06, type="pmle") ## no SEs! PMLE!!!
   #      summary(m06, type="boot")
   #
   # fit$std.error
   #
   # summary(fit)
   #
   #
   # confint(C.paca.full,"sta_bsq")
   # confint(C.thous.full)
   #
   # ## good sta, bad det
   #
   # summary(P.maximus.step)
   #
   #   fit <- svocc(pa ~ caz + bsq + dst + frs + dbsq| walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = TRUE, method = c( "optim"))
   #   bfit <- bootstrap(fit, B=50)
   #
   #   sfit <- svocc.step(bfit, model="sta")
   #   sbfit <- bootstrap(sfit, B=50)
   #
   #
   #
   # summary(bfit,"mle")
   # summary(bfit,"pmle")
   # summary(bfit,"boot")
   #
   #
   #
   # AIC(L.pardalis.full,L.pardalis.bsq,L.pardalis.dst)
   # AIC(T.tetradactyla.full,T.tetradactyla.bsq,T.tetradactyla.dst)
   #
