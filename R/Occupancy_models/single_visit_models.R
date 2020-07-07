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


args <- commandArgs(TRUE)
sp <- args[1]


## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
 load(GIS.data)

##table(camaras$caza.celda,camaras$Si.se.caza.aqui)

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

##for (sp in levels(eventos$species)) {
   eventos %>% mutate(target=species %in% sp) %>% group_by(grid) %>% summarise(pa=max(target)) -> species

   ## two events (camera and ras) outside grid, are they valid?

   dts  %>% left_join(walk) %>% left_join(cams) %>% left_join(species) %>% filter(!is.na(grid) & (!is.na(walk) | !is.na(cam))) %>% transmute(grid, walk, cam, caz=as.factor(!caz %in% 0), pa,
   bsq=(bsq-mean(bsq))/sd(bsq),
   dcon=(dcon-mean(dcon,na.rm=T))/sd(dcon,na.rm=T),
   dcom=(dcom-mean(dcom,na.rm=T))/sd(dcom,na.rm=T),
   dhum=(dhum-mean(dhum,na.rm=T))/sd(dhum,na.rm=T), ## all human activities together (com, con, caz)
   dpob=(dpob-mean(dpob,na.rm=T))/sd(dpob,na.rm=T),
   dcaz=(dcaz-mean(dcaz,na.rm=T))/sd(dcaz,na.rm=T),
   frs=(frs-mean(frs,na.rm=T))/sd(frs,na.rm=T),
   dbsq=(dbsq-mean(dbsq,na.rm=T))/sd(dbsq,na.rm=T)) -> pa.data

   ##colSums(is.na(pa.data))
   pa.data$walk <- coalesce(pa.data$walk,min(walk$walk,na.rm=T))
   pa.data$cam <- coalesce(pa.data$cam,min(cams$cam,na.rm=T))
   ##   pa.data$d <- coalesce(pa.data$dst,max(pa.data$dst,na.rm=T))
   ##pa.data$dbsq <- coalesce(pa.data$dbsq,max(pa.data$dbsq,na.rm=T))
   ##pa.data$frs <- coalesce(pa.data$frs,max(pa.data$frs,na.rm=T))
   pa.data$pa <- coalesce(pa.data$pa,0L)
   ##  pa.data$caz <- coalesce(pa.data$caz,0L)

   ## cor(pa.data[,-4])

   ## caz events and distance to conucos
   ##orig <- svocc(pa ~ caz + bsq + dcon + frs + dbsq| walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
   ## keep conucos and communities as two different proxies (atracting and rejecting fauna)

   fit0 <- fit <- svocc(pa ~ bsq + dcon + dcom + frs + dbsq | walk * cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))

   exito <- "NOT"

   if (all(abs(fit$coefficients$sta)<10) & all(!is.na(fit$std.error$sta)) & all(fit$std.error$sta<6) ) {
      assign(sprintf("%s.full",sp),fit)
      exito <- "OK"
   } else {
      fit1 <- svocc(pa ~ bsq + dcon + dcom  + frs + dbsq | walk + cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit2 <- svocc(pa ~ caz + bsq + dcon + frs + dbsq | walk*cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit3 <- svocc(pa ~ bsq + dcom  + frs + dbsq | walk*cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit4 <- svocc(pa ~ bsq + dcon + dcom  + frs + dbsq | walk * cam, data=pa.data, link.sta = "probit", link.det = "logit", penalized = FALSE, method = c( "optim"))

      fit5 <- svocc(pa ~ bsq + dhum | walk*cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit6 <- svocc(pa ~ bsq + frs + dbsq | walk*cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit7 <- svocc(pa ~ bsq + dcom + dcon | walk*cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))

      fit8 <- svocc(pa ~ caz + bsq + dcon + frs + dbsq| walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit9 <- svocc(pa ~ caz + bsq + dcon + frs + dbsq| walk*cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit10 <- svocc(pa ~ caz + bsq + dcon + frs + dbsq| walk*cam, data=pa.data, link.sta = "probit", link.det = "logit", penalized = FALSE, method = c( "optim"))
      fit11 <- svocc(pa ~ caz + bsq + dcon + frs + dbsq| walk+cam, data=pa.data, link.sta = "probit", link.det = "logit", penalized = FALSE, method = c( "optim"))

      tt <- AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11)

      for (k in order(tt$AIC)) {
         if (exito != "OK") {
            fit <- get(sprintf("fit%s",k))
            if (all(abs(fit$coefficients$sta)<10) & all(!is.na(fit$std.error$sta)) & all(fit$std.error$sta<6) ) {
               assign(sprintf("%s.alt",sp),fit)
               exito <- "OK"
            }
         }
      }
   }

   for (k in 0:11) {
      if (exito != "OK") {
         fit <- svocc.step(get(sprintf("fit%s",k)), model="sta")
         if (all(abs(fit$coefficients$sta)<10) & all(!is.na(fit$std.error$sta)) & all(fit$std.error$sta<6) ) {
            assign(sprintf("%s.step",sp),fit)
            exito <- "OK"
         }
      }
   }
   save(file=sprintf("%s/Rdata/svocc-%s.rda",script.dir,sp),list=c(ls(pattern=".full"),ls(pattern=".alt"),ls(pattern=".step")))



# ## al menos 10-13 species con modelos decentess
# ls(pattern=".full")
# ls(pattern=".alt")
# ls(pattern=".step")
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
#
# 8      P.concolor   32
#
# 10         P.onca   27
# 16      P.maximus    9
# 20  O.virginianus    5
#
# Faltan
#            Var1 Freq
# 9     M.americana   32
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
