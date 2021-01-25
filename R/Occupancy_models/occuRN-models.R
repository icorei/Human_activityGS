#!R --vanilla
##paquetes necesarios
require(unmarked)
require(AICcmodavg)
require(chron)
require(raster)
require(cluster)
require(MuMIn)

## ubicación de la carpeta de trabajo y el repositorio local

work.dir <- Sys.getenv("WORKDIR")
script.dir <- Sys.getenv("SCRIPTDIR")
slcspp <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

##Sys.getenv()

cat(sprintf("PBS ARRAY INDEX is %s\n",slcspp))

## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
load(GIS.data)


camaras %>% filter(bloque %in% sprintf("B%02i",1:6)) %>%
  mutate(bloque=droplevels(bloque),
    fecha1 = chron(dates.=as.character(fecha.act), times.=as.character(hora.act),
      format = c(dates = "y-m-d", times = "h:m:s")),
    fecha2 = chron(dates.=as.character(fecha.desact.real),
    times.=as.character(hora.desact.real),
      format = c(dates = "y-m-d", times = "h:m:s")),
    cdg = as.character(ID.original)) %>%
  group_by(cdg) %>%
  summarise(lat=mean(lat), lon=mean(lon), bloque=unique(bloque),
    hunting=unique(factor(caza.celda>0)), grp=unique(grp), H=mean(H), h=mean(h),
    tree_0500m=mean(tree_0500m), tree_1000m=mean(tree_1000m),
    tree_2500m=mean(tree_2500m), tree_5000m=mean(tree_5000m),
    drios=mean(drios), bsq=mean(buf.fragmen), ndvi=mean(ndvi.mu),
    fecha1=min(fecha1), fecha2=max(fecha2)) %>%
  mutate(duration=as.numeric(fecha2-fecha1)) ->
    cam.data

d1 <- pointDistance(cam.data[,c("lon","lat")],
  coordinates(comunidades), lonlat=T, allpairs=T)

cam.data$dcom <- apply(d1,1,min)


d1 <- pointDistance(cam.data[,c("lon","lat")],
   coordinates(conucos)[,1:2], lonlat=T, allpairs=T)

cam.data$dcon <- apply(d1,1,min)


p <- 0.25
w <- 1/((d1)^p)
cam.data$wcon <- apply(w,1,sum)

cam.data$dhum <- with(cam.data,ifelse(dcon<dcom,dcon,dcom))


drastros <- pointDistance(subset(eventos,camara %in% "RAS")[,c("long","lat")], cam.data[,c("lon","lat")], lonlat=TRUE)

p <- 0.25
w <- 1/((drastros)^p)
cam.data$dras <- apply(w,2,sum)

mi.spp <- levels(droplevels(eventos$species))[slcspp]
cat(sprintf("La especie es %s (nr. %s)",mi.spp,slcspp))

eventos$cdg <- as.character(camaras$ID.original)[
  match(paste(eventos$bloque,eventos$periodo,eventos$camara),
    paste(camaras$bloque,camaras$period,camaras$camera))]

eventos %>% mutate(f1 = chron(dates.=sprintf("%s-%s-%s",ano,mes,dia),
    times.=as.character(hora.ini),
    format = c(dates = "y-m-d", times = "h:m:s")),
   f2 = chron(dates.=sprintf("%s-%s-%s",ano,mes,dia),
    times.=as.character(hora.ini),
    format = c(dates = "y-mon-d", times = "h:m:s"),
    out.format = c(dates = "y-m-d", times = "h:m:s"))) %>%
  transmute(cdg, camara, fotos, species=as.character(species), number.of.animals,
   fecha=chron(ifelse(is.na(f2),f1,f2),
   format = c(dates = "y-m-d", times = "h:m:s"))) %>%
  filter(cdg %in% cam.data$cdg & species %in% mi.spp) ->
    event.data



    make.obs.matrix <- function(x,y,w=NULL) {
      mtz <- matrix(0,nrow=length(unique(x$cdg)),
        ncol=length(y)-1,
          dimnames=list(unique(x$cdg),as.character(y)[-1]))
      for (k in 1:nrow(x)) {
         mtz[ x[k,"cdg"],] <-
          mtz[ x[k,"cdg"],] +
            table(cut(seq(x$fecha1[k],x$fecha2[k],by=1),
              breaks=y,label=as.character(y)[-1]))
      }
      if (!is.null(w)) {
        w %>%  mutate(sessions=cut(fecha,breaks=y,label=as.character(y)[-1])) -> z
        mtz[mtz==0] <- NA
        mtz <- mtz*0
        for (k in  seq(along=z$species)) {
           mtz[z[k,"cdg"],z[k,"sessions"]] <-
            mtz[z[k,"cdg"],z[k,"sessions"]] +
              z[k,"number.of.animals"]
        }
      }
      return(mtz)
    }


ini <- chron(dates.="2015-09-21",times.="00:00:00",format = c(dates = "y-m-d", times = "h:m:s"))
visits3 <- ini + seq(from=0,to=210,by=21)


## Detection history
obs <- make.obs.matrix(data.frame(cam.data),visits3,data.frame(event.data))

## Observation covariates:
## sampling effort
sfrz <- make.obs.matrix(data.frame(cam.data),visits3)

## observation date
x <- seq(-1,1,length=ncol(obs))
obsDate <- matrix(rep(x,nrow(obs)),nrow=nrow(obs),byrow=T)


sC <- data.frame(cam.data[match(rownames(obs),cam.data$cdg),
   c("bloque","H","h","dcon","dcom","wcon",
      "dhum","tree_0500m","tree_1000m","tree_2500m","tree_5000m","drios","ndvi","grp","dras","hunting")])
sC$bloque <- droplevels(sC$bloque)

for (k in c("H","h","dcon","dras","ndvi","tree_0500m","tree_1000m","tree_2500m","tree_5000m","drios","dcom","wcon","dhum")) {
   sC[,k] <- (sC[,k]-mean(sC[,k]))/sd(sC[,k])
}

ss <- rownames(obs) %in% subset(cam.data,duration>7 & dcon<5000)$cdg

nsim.val <- 10000 # change to 1000 for manuscript results



UMF <- unmarkedFrameOccu((obs[ss,]>0)+0,
  siteCovs=sC[ss,,drop=F],
  obsCovs=list(date=obsDate[ss,],sfrz=sfrz[ss,]/21))

mi.rda <- sprintf("%s/Rdata/occuRN/%s.rda",script.dir,mi.spp)

(fm01 <- occuRN(~ dras+sfrz+date ~ tree_1000m+dcon+drios, UMF,K=50))
(fm03 <- occuRN(~ dras+sfrz+date ~ tree_1000m+I(tree_1000m^2)+dcon+drios, UMF,K=50))

save(file=mi.rda,UMF,fm01,fm03)

ts03 <- mb.gof.test(fm03,nsim=nsim.val,maxK=50,parallel = TRUE)
save(file=mi.rda,UMF,fm01,fm03,ts03)

if (ts03$c.hat.est>1) {
  oms01 <- dredge(fm01,rank="QAICc",chat=ts03$c.hat.est)
  oms03 <- dredge(fm03,rank="QAICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),),chat=ts03$c.hat.est)
} else {
  oms01 <- dredge(fm01,rank="AICc")
  oms03 <- dredge(fm03,rank="AICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),))
}

mavg01 <- model.avg(oms01, subset = delta < 10,fit=T)
mavg03 <- model.avg(oms03, subset = delta < 10,fit=T)


save(file=mi.rda,UMF,fm01,fm03,ts03,oms01,oms03)

fm11 <- occuRN(~ dras+sfrz+date ~ tree_1000m+dcon+drios+wcon, UMF,K=50)
fm13 <- occuRN(~ dras+sfrz+date ~ tree_1000m+I(tree_1000m^2)+dcon+drios+wcon, UMF,K=50)
ts13 <- mb.gof.test(fm13,nsim=nsim.val,maxK=50,parallel = TRUE)

if (ts13$c.hat.est>1) {
  oms11 <- dredge(fm11,rank="QAICc",chat=ts13$c.hat.est)
  oms13 <- dredge(fm13,rank="QAICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),),chat=ts13$c.hat.est)
} else {
  oms11 <- dredge(fm11,rank="AICc")
  oms13 <- dredge(fm13,rank="AICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),))
}

mavg11 <- model.avg(oms11, subset = delta < 10,fit=T)
mavg13 <- model.avg(oms13, subset = delta < 10,fit=T)

save(file=mi.rda,UMF,fm01,fm03,ts03,oms01,oms03,fm11,fm13,ts13,oms11,oms13)


(fm31 <- occuRN(~ dras+sfrz+date ~ tree_1000m+dcon, UMF,K=50))
(fm33 <- occuRN(~ dras+sfrz+date ~ tree_1000m+I(tree_1000m^2)+dcon, UMF,K=50))

ts33 <- mb.gof.test(fm33,nsim=nsim.val,maxK=50,parallel = TRUE)


if (ts33$c.hat.est>1) {
  oms31 <- dredge(fm31,rank="QAICc",chat=ts33$c.hat.est)
  oms33 <- dredge(fm33,rank="QAICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),),chat=ts33$c.hat.est)
} else {
  oms31 <- dredge(fm31,rank="AICc")
  oms33 <- dredge(fm33,rank="AICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),))
}

mavg31 <- model.avg(oms31, subset = delta < 10,fit=T)
mavg33 <- model.avg(oms33, subset = delta < 10,fit=T)

save(file=mi.rda,UMF,fm01,fm03,ts03,oms01,oms03,fm11,fm13,ts13,oms11,oms13,fm31,fm33,ts33,oms31,oms33)

for (k in c("UMF","fm01","fm03","ts03","oms01","oms03","fm11","fm13","ts13","oms11","oms13","fm31","fm33","ts33","oms31","oms33","mavg01","mavg03","mavg11","mavg13","mavg31","mavg33")) {
  assign(sprintf("%s.%s",k,mi.spp),get(k))
}

save(file=mi.rda,list=ls(pattern=mi.spp))
# mi.rda <- sprintf("%s/Rdata/occu/%s.rda",script.dir,mi.spp)
# if (!file.exists(mi.rda)) {
#  fm00 <- occu(~ sfrz+dcom+date ~ H+bsq, UMF,linkPsi= "cloglog")
#  fm01 <- occu(~ sfrz+dcom+date ~ H+bsq+dcon, UMF,linkPsi= "cloglog")
#
#  ts02 <- mb.gof.test(fm01,nsim=nsim.val)
#  save(file=mi.rda,UMF,fm00,fm01,ts02)
# }
