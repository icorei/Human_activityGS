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

NagR2 <- function(fullmodel,nullmodel) {
    Lnull <- exp(nullmodel$loglik)
    Lfull <- exp(fullmodel$loglik)
    N <- fullmodel$nobs
    R2 <-  (1-((Lnull/Lfull)^(2/N)))/(1-(Lnull^(2/N)))
    return(R2)
}

R2s <- data.frame()
for(k in ls(pattern="boot")) {
  Mfull <- get(k)
  Mnull <- get(sub("boot","null",k))
  R2s <- rbind(R2s,data.frame(k,R2= NagR2(Mfull,Mnull),deltaAIC=AIC(Mnull)-AIC(Mfull)) )
} 
NagR2(C.thous.boot,C.thous.null)
NagR2(P.concolor.boot,P.concolor.null)

## use null model to validate fitted model (lower AIC and maybe calculate Nagelkerke R2)

   GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
    load(GIS.data)

   plot(vbsq)
   plot(grd,add=T,border="maroon")
   points(lat~long,data=eventos,subset=!camara %in% "RAS",col=4,pch=19)
   points(lat~long,data=eventos,subset=camara %in% "RAS",col=5,pch=3)
   points(comunidades,cex=2)
   points(conucos,cex=2,pch=5)


   eventos$grid <-  extract(rgrd,eventos[,c("long","lat")])
   track_points@data$grid <- extract(rgrd,track_points)

   camaras$grid <- extract(rgrd,camaras[,c("lon","lat")])

   track_points@data %>% group_by(grid) %>% tally() %>% transmute(grid,walk=(n-mean(n))/sd(n))-> walk
   camaras %>% group_by(grid) %>% summarise(cam=sum(dias.de.trabajo),caz=max(caza.celda)) %>% transmute(grid,caz,cam=(cam-mean(cam))/sd(cam)) -> cams

   dts <- data.frame(bsq=values(vbsq),dcon=values(dist.conucos),dcom=values(dist.comunidades),frs=values(dist.frs),dbsq=values(dist.dbsq),dcaz1=values(dist.caza1),dcaz2=values(dist.caza2),grid=values(rgrd)) %>% group_by(grid) %>% summarise(bsq=mean(bsq),dbsq=mean(dbsq),dcon=mean(dcon),dcom=mean(dcom),dpob=mean(min(dcon,dcom)),dhum=mean(min(dcon,dcom,dcaz1,dcaz2)),dcaz1=mean(dcaz1),dcaz2=mean(dcaz2),dcaz=mean(min(dcaz1,dcaz2)),frs=mean(frs))


   nw.dts <- data.frame(bsq=values(fbsq),dcon=values(dist.conucos),dcom=values(dist.comunidades),frs=values(current.frs),dbsq=values(current.dbsq),dcaz1=values(dist.caza1),dcaz2=values(dist.caza2),grid=values(rgrd)) %>% group_by(grid) %>% summarise(bsq=mean(bsq),dbsq=mean(dbsq),dcon=mean(dcon),dcom=mean(dcom),dpob=mean(min(dcon,dcom)),dhum=mean(min(dcon,dcom,dcaz1,dcaz2)),dcaz1=mean(dcaz1),dcaz2=mean(dcaz2),dcaz=mean(min(dcaz1,dcaz2)),frs=mean(frs))

      plot(dts$frs,nw.dts$frs)
      plot(dts$dbsq,nw.dts$dbsq)
      abline(0,1)

      eventos %>% mutate(target=species %in% sp) %>% group_by(grid) %>% summarise(pa=max(target)) -> species

      ## two events (camera and ras) outside grid, are they valid?

   dts %>% summarise(mu.bsq=mean(bsq),sg.bsq=sd(bsq),
   mu.dbsq=mean(dbsq),sg.dbsq=sd(dbsq),
   mu.frs=mean(frs),sg.frs=sd(frs),
   mu.dcon=mean(dcon),sg.dcon=sd(dcon),
   mu.dcom=mean(dcom),sg.dcom=sd(dcom),
   mu.dpob=mean(dpob),sg.dpob=sd(dpob),
   mu.dhum=mean(dhum),sg.dhum=sd(dhum),
   mu.dcaz=mean(dcaz),sg.dcaz=sd(dcaz)
   ) -> mus

      nw.dts  %>% transmute(grid,caz=T,
      bsq=(bsq-mus$mu.bsq)/mus$sg.bsq,
      dcon=(dcon-mus$mu.dcon)/mus$sg.dcon,
      dcom=(dcom-mus$mu.dcom)/mus$sg.dcom,
      dhum=(dhum-mus$mu.dhum)/mus$sg.dhum, ## all human activities together (com, con, caz)
      dpob=(dpob-mus$mu.dpob)/mus$sg.dpob,
      dcaz=(dcaz-mus$mu.dcaz)/mus$sg.dcaz,
      frs=(frs-mus$mu.frs)/mus$sg.frs,
      dbsq=(dbsq-mus$mu.dbsq)/mus$sg.dbsq) -> nw.data
   dts  %>% transmute(grid,caz=T,
      bsq=(bsq-mus$mu.bsq)/mus$sg.bsq,
      dcon=(dcon-mus$mu.dcon)/mus$sg.dcon,
      dcom=(dcom-mus$mu.dcom)/mus$sg.dcom,
      dhum=(dhum-mus$mu.dhum)/mus$sg.dhum, ## all human activities together (com, con, caz)
      dpob=(dpob-mus$mu.dpob)/mus$sg.dpob,
      dcaz=(dcaz-mus$mu.dcaz)/mus$sg.dcaz,
      frs=(frs-mus$mu.frs)/mus$sg.frs,
      dbsq=(dbsq-mus$mu.dbsq)/mus$sg.dbsq) -> mi.data

      ## asignar bloques para hacer predicciones por bloque:
      ## hacer matrices de probabilidad de uso por bloques...
      ## comparar diferencias entre bloques: naive, null model, best fit model

      ## comparar predicciones de probablidad de uso vs. caceria (no, si, antes)

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
table(ccis$species,ccis$var)

   d1 <- subset(ccis,var %in% "bsq") # one avoids, five prefer
   d1 <- subset(ccis,var %in% "dbsq") # no effect
   d1 <- subset(ccis,var %in% "dcom") # one avoid communities

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

   d1 <- subset(ccis,var %in% "frs") # no effect
   d1 <- subset(ccis,var %in% "dcon") # two attracted by conucos
   d1 <- subset(ccis,var %in% "dhum") # no effect
   d1 <- d1[order(d1$coef),]
   o1 <- 1:nrow(d1)
   c1 <- rep(1,length(o1))
   c1[d1[,6]<0] <- 2
   c1[d1[,5]>0] <- 3

   par(mar=c(4,8,1,1))
   plot(d1$coef,o1,pch=19,col=c1,cex=1.5,axes=F,xlab="coefficient",ylab="",xlim=c(-10,5))
   axis(1)
   axis(2,o1,d1$species,las=2,font=3)
   segments(d1[,5],o1,d1[,6],o1,col=c1)
   abline(v=0,lty=3)

   subset(ccis,var %in% "cazTRUE") # pos. but not sign. for P. onca



   predict(C.paca.alt)
   predict(C.paca.alt,mi.data)
   predict(C.paca.alt,nw.data)

## esta es la prediccion en 250 celdas
   hist(boot::inv.logit(predict(C.paca.alt,mi.data)))
   hist(boot::inv.logit(predict(C.paca.alt,nw.data)))

   sum(boot::inv.logit(predict(C.paca.alt,mi.data)))
   sum(boot::inv.logit(predict(C.paca.alt,nw.data)))

   sum(boot::inv.logit(predict(C.paca.alt,mi.data)))
   sum(boot::inv.logit(predict(C.paca.alt,nw.data)))

   sum(boot::inv.logit(predict(M.gouazoubira.full,mi.data)))
   sum(boot::inv.logit(predict(M.gouazoubira.full,nw.data)))


   sum(boot::inv.logit(predict(M.americana.step,mi.data)))
   sum(boot::inv.logit(predict(M.americana.step,nw.data)))

prds <- data.frame()
for (mdl in ls(pattern=".boot")) {
      fit <- get(mdl)
      prd0 <- predict(fit,mi.data,type="response",se.fit=T)
      A <- 1.85 * boot::logit(prd0$se.fit)
      A[is.infinite(A)] <- -60
      B <- boot::logit(prd0$fit)

      ci.min <- boot::inv.logit(B+A)
      ci.max <- boot::inv.logit(B-A)
      nm <- gsub(".boot","",mdl)


      prd1 <- predict(fit,nw.data,type="response",se.fit=T)
      A <- 1.85 * boot::logit(prd1$se.fit)
      A[is.infinite(A)] <- -60
      B <- boot::logit(prd1$fit)

      c2.min <- boot::inv.logit(B+A)
      c2.max <- boot::inv.logit(B-A)

      prds <- rbind(prds,data.frame(specie=nm,antes=sum(prd0$fit),a.lower=sum(ci.min),a.upper=sum(ci.max),
         despues=sum(prd1$fit),d.lower=sum(c2.min),d.upper=sum(c2.max)))
   }

   prds <- prds[order(prds$antes),]
   o1 <- 1:nrow(prds)
   c1 <- o1^0
   c1[prds$despues<(prds$antes*.95)] <- 2
   c1[prds$antes<(prds$despues*.95)] <- 3
   par(mar=c(4,8,1,1))
   plot(prds$antes,o1,pch=19,col=c1,cex=1.5,axes=F,xlab="coefficient",ylab="",xlim=c(0,250))
   points(prds$despues,o1+.25,pch=1,col=c1,cex=1.5,axes=F,xlab="coefficient",ylab="",xlim=c(0,250))
   axis(1)
   axis(2,o1,prds$specie,las=2,font=3)
   segments(prds$a.lower,o1,prds$a.upper,o1,col=c1)
   segments(prds$d.lower,o1+.25,prds$d.upper,o1+.25,col=c1)

predict(C.paca.full)

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
