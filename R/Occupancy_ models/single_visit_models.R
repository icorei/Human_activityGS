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


## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
 load(GIS.data)

eventos <- read.csv2(sprintf("%s/input/fieldwork/Eventos_GS_CAM_RAS_2019.csv",script.dir))
camaras <- read.csv2(sprintf("%s/input/fieldwork/Camaras_GS_2019.csv",script.dir))


plot(vbsq)
plot(grd,add=T,border="maroon")
points(lat~long,data=eventos,subset=!camara %in% "RAS",col=4,pch=19)
points(lat~long,data=eventos,subset=camara %in% "RAS",col=5,pch=3)

eventos$grid <-  extract(rgrd,eventos[,c("long","lat")])
track_points@data$grid <- extract(rgrd,track_points)

camaras$grid <- extract(rgrd,camaras[,c("lon","lat")])

track_points@data %>% group_by(grid) %>% tally() %>% transmute(grid,walk=(n-mean(n))/sd(n))-> walk
camaras %>% group_by(grid) %>% summarise(cam=sum(dias.de.trabajo),caz=max(caza.celda)) %>% transmute(grid,caz,cam=(cam-mean(cam))/sd(cam)) -> cams

dts <- data.frame(bsq=values(vbsq),dst=values(dist.conucos),frs=values(dist.frs),dbsq=values(dist.dbsq),grid=values(rgrd)) %>% group_by(grid) %>% summarise(bsq=mean(bsq),dbsq=mean(dbsq),dst=mean(dst),frs=mean(frs))

eventos %>% mutate(target=species %in% "D.kappleri") %>% group_by(grid) %>% summarise(pa=max(target)) -> species
eventos %>% mutate(target=species %in% "E.barbara") %>% group_by(grid) %>% summarise(pa=max(target)) -> species

for (sp in levels(eventos$species)) {
  eventos %>% mutate(target=species %in% sp) %>% group_by(grid) %>% summarise(pa=max(target)) -> species


  dts  %>% left_join(walk) %>% left_join(cams) %>% left_join(species) %>% filter(!is.na(walk) | !is.na(cam)) %>% transmute(grid, walk, cam, caz=as.factor(!caz %in% 0), pa,
      bsq=(bsq-mean(bsq))/sd(bsq),
      dst=(dst-mean(dst,na.rm=T))/sd(dst,na.rm=T),
      frs=(frs-mean(frs,na.rm=T))/sd(frs,na.rm=T),
      dbsq=(dbsq-mean(dbsq,na.rm=T))/sd(dbsq,na.rm=T),
    ) -> pa.data

  pa.data$walk <- coalesce(pa.data$walk,min(walk$walk,na.rm=T))
  pa.data$cam <- coalesce(pa.data$cam,min(cams$cam,na.rm=T))
  pa.data$dst <- coalesce(pa.data$dst,max(pa.data$dst,na.rm=T))
  pa.data$dbsq <- coalesce(pa.data$dbsq,max(pa.data$dbsq,na.rm=T))
  pa.data$frs <- coalesce(pa.data$frs,max(pa.data$frs,na.rm=T))
  pa.data$pa <- coalesce(pa.data$pa,0L)
##  pa.data$caz <- coalesce(pa.data$caz,0L)


  fit <- svocc(pa ~ caz + bsq + dst + frs + dbsq| walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))

  assign(sprintf("%s.full",sp),fit)

  sfit <- svocc.step(fit, model="sta")

  assign(sprintf("%s.step",sp),sfit)

}

sprintf("summary(%s.full)\n",levels(eventos$species))

# Full model mle -- look no further
summary(C.paca.full)
summary(C.thous.full)
summary(D.kappleri.full)
summary(M.gouazoubira.full)
summary(M.tridactyla.full)
summary(O.virginianus.full)
summary(T.tetradactyla.full)

## good sta, bad det

summary(P.maximus.step)



for (sp in c("P.maximus","C.olivaceus","C.unicinctus", "D.imperfecta", "D.leporina", "D.marsupialis", "D.novemcinctus", "E.barbara", "H.hydrohaeris", "L.pardalis", "L.tigrinus", "L.wiedii", "M.americana", "M.pratti", "N.nasua", "P.concolor", "P.onca", "P.tajacu", "S.venaticus","T.terrestris", "T.pecari")) {
  eventos %>% mutate(target=species %in% sp) %>% group_by(grid) %>% summarise(pa=max(target)) -> species


  dts  %>% left_join(walk) %>% left_join(cams) %>% left_join(species) %>% filter(!is.na(walk) | !is.na(cam)) %>% transmute(grid, walk, cam, caz=as.factor(!caz %in% 0), pa,
      bsq=(bsq-mean(bsq))/sd(bsq),
      dst=(dst-mean(dst,na.rm=T))/sd(dst,na.rm=T),
      frs=(frs-mean(frs,na.rm=T))/sd(frs,na.rm=T),
      dbsq=(dbsq-mean(dbsq,na.rm=T))/sd(dbsq,na.rm=T),
    ) -> pa.data

  pa.data$walk <- coalesce(pa.data$walk,min(walk$walk,na.rm=T))
  pa.data$cam <- coalesce(pa.data$cam,min(cams$cam,na.rm=T))
  pa.data$dst <- coalesce(pa.data$dst,max(pa.data$dst,na.rm=T))
  pa.data$dbsq <- coalesce(pa.data$dbsq,max(pa.data$dbsq,na.rm=T))
  pa.data$frs <- coalesce(pa.data$frs,max(pa.data$frs,na.rm=T))
  pa.data$pa <- coalesce(pa.data$pa,0L)
##  pa.data$caz <- coalesce(pa.data$caz,0L)


  fit <- svocc(pa ~ caz + bsq + dst + frs + dbsq| walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = TRUE, method = c( "optim"))
  bfit <- bootstrap(fit, B=50)

  assign(sprintf("%s.full.pen",sp),bfit)


  sfit <- svocc.step(bfit, model="sta")
  sbfit <- bootstrap(sfit, B=50)

  assign(sprintf("%s.step.pen",sp),sbfit)

}


summary(C.olivaceus.step)
 summary(C.unicinctus.step)
 summary(D.imperfecta.step)
 summary(D.leporina.step)
 summary(D.marsupialis.step)
 summary(D.novemcinctus.step)
 summary(E.barbara.step)
 summary(H.hydrohaeris.step)
 summary(L.pardalis.step)
 summary(L.tigrinus.step)
 summary(L.wiedii.step)
 summary(M.americana.step)
 summary(M.pratti.step)

 summary(N.nasua.step)
 summary(P.concolor.step)


 summary(P.onca.step)
 summary(P.tajacu.step)
 summary(S.venaticus.step)
 summary(T.terrestris.step)
 # not converged
 summary(T.pecari.step)

summary(bfit,"mle")
summary(bfit,"pmle")
summary(bfit,"boot")

fit <- svocc(pa ~ bsq + dst + frs + dbsq  | walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))

sfit <- svocc.step(fit, model="sta")

bfit <- bootstrap(sfit, B=50)

  fit <- svocc(pa ~ bsq  | walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
  assign(sprintf("%s.bsq",sp),fit)

  fit <- svocc(pa ~ dst  | walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
  assign(sprintf("%s.dst",sp),fit)


AIC(L.pardalis.full,L.pardalis.bsq,L.pardalis.dst)
AIC(T.tetradactyla.full,T.tetradactyla.bsq,T.tetradactyla.dst)

fit <- svocc(pa ~ poly(bsq,2) + dst | walk + cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = TRUE, method = c( "optim"))


  fit <- svocc(pa ~ bsq  | walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
  assign(sprintf("%s.bsq",sp),fit)


## Because of the low information content in binary data, estimation of occupancy and detection from single-survey data is extremely difficult (Welsh, Lindenmayer & Donnelly 2013). As described in Moreno & Lele (2010) and Lele, Moreno & Bayne (2012), one may need to use penalized likelihood function to stabilize the estimators. Unfortunately, it is not clear how to choose a good penalty function in general. In the Supplementary Information, we use a quasi-Bayesian approach where the means of the prior distributions for the parameters are determined from the observations themselves instead of based on a probabilistic quantification of ‘belief’. This seems to stabilize the estimation process considerably, resulting in estimators that are nearly unbiased. Unfortunately, this estimation method lacks a strong theoretical basis.

##resource selection probability functions RSPF

fit <- svocc(pa ~ bsq + dst | walk+cam, data=pa.data, link.sta = "cloglog", link.det = "logit", penalized = TRUE, method = c( "optim"))
m06 <- bootstrap(fit, B=25)
     attr(m06, "bootstrap")
     extractBOOT(m06)
     summary(m06, type="mle")
     summary(m06, type="pmle") ## no SEs! PMLE!!!
     summary(m06, type="boot")

fit$std.error

summary(fit)





table(eventos$grid)

with(dts,
     text(x,y+0.036,
          sprintf("h=%0.1f%% H=%0.2f",hs*100,fd-1),
          font=2,cex=.7,col=1))
points(frs.c,pch=3,cex=.3,col=2)

table(eventos$species,eventos$camara %in% "RAS")

pa <- eventos$species %in% "P.concolor"

#dts <- aggregate(data.frame(pa=eventos$species %in% "P.concolor"),by=eventos[,c("bloque","periodo","camara")],max)
left_join(eventos,camaras,by=c('bloque'='bloque','periodo'='period','camara'='camera')) %>%
   group_by(bloque,periodo,camara) %>%
   summarise(lon=mean(long),lat=mean(lat.x), pa=max(species %in% "P.concolor"),effort=replace_na(mean(dias.de.trabajo),5)) %>%
   mutate(rastro=camara %in% "RAS") -> dts

dts$bosque <- extract(vbsq,dts[,c("lon","lat")]) ##> 50
dts$bosque.cell <- extract(rbsq,dts[,c("lon","lat")]) ##> 50
## distribucion bimodal, pero funciona mejor con la variable cuantitativa que con una binomal
##hist(dts$bosque)
dts$v1 <- ( dts$bosque.cell - mean(dts$bosque.cell) ) / sd(dts$bosque.cell)
dts$v2 <- ( dts$bosque - mean(dts$bosque) ) / sd(dts$bosque)
dts$e1 <- ( dts$effort - mean(dts$effort) ) / sd(dts$effort)

dim(dts)

## even after installing dclone and dcmle it throws errors in dc methods...
fit <- svocc(pa ~ bosque + bosque.cell | effort+rastro, data=dts, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "dc"))

fit <- svocc(pa ~ v1+v2 | e1+rastro, data=dts, link.sta = "cloglog", link.det = "logit", penalized = FALSE, method = c( "optim"))
summary(fit)


GS <- merge(EventosGranSabana_mam,camerasGS,by.x=c("bloque","periodo","camara"),
            by.y=c("bloque","period","camera"),all.y=T)
tt <- table(paste(GS$bloque,GS$periodo,GS$camara),GS$species)
head(tt)
