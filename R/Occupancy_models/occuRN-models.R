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
   slcspp <- Sys.getenv("PBS_ARRAY_INDEX")
}


## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
load(GIS.data)



## Distance to rastros

drastros <- pointDistance(subset(eventos,camara %in% "RAS")[,c("long","lat")], camaras[,c("lon","lat")], lonlat=TRUE)

# IDW
# g(u) = (sum of w[i] )
# where the weights are the inverse p-th powers of distance,
# w[i] = 1/d(u,x[i])^p
# where d(u,x[i]) is the Euclidean distance from u to x[i].

p <- 0.25
w <- 1/((drastros)^p)
idw.rastros <- apply(w,2,sum)
#idw.conucos <- (idw.conucos - mean(idw.conucos)) / sd(idw.conucos)
hist(idw.rastros)


## Distance to conucos

dconucos <- pointDistance(coordinates(conucos)[,1:2], camaras[,c("lon","lat")], lonlat=TRUE)


#distance to nearest
min.conucos <- apply(dconucos,2,min)/max(dconucos)/9000

# IDW
# g(u) = (sum of w[i] )
# where the weights are the inverse p-th powers of distance,
# w[i] = 1/d(u,x[i])^p
# where d(u,x[i]) is the Euclidean distance from u to x[i].

p <- 0.25
w <- 1/((dconucos)^p)
idw.conucos <- apply(w,2,sum)
##idw.conucos <- (idw.conucos - mean(idw.conucos)) / sd(idw.conucos)
hist(idw.conucos)

camaras$wcon <- idw.conucos
camaras$dcon <- min.conucos
camaras$dras <- idw.rastros
boxplot(extract(vbsq,conucos)~conucos@data$short_name)

table(as.vector(substring(viq.camara,15,16)))

qry <- ndvi.camara
qry[!substring(viq.camara,15,16)%in% "00"] <- NA

hdvi <- pam(dist(qry),k=3)
table(hdvi$clustering)
hdvi$silinfo$avg.width

camaras$ndvi.mu <- apply(qry,1,mean,na.rm=T)
camaras$ndvi.sg <- apply(qry,1,sd,na.rm=T)

camaras$grp <- factor(hdvi$clustering,labels=c("savanna","shrub","forest"))

camaras$bsq <- extract(vbsq,camaras[,c("lon","lat")])
camaras$dcom <- extract(dist.comunidades,camaras[,c("lon","lat")])

boxplot(camaras$ndvi.mu~camaras$grp)

boxplot(camaras$buf.fragmen~camaras$grp)

cor.test(camaras$buf.fragmen,idw.conucos)
cor.test(camaras$ndvi.mu,idw.conucos)

eventos <- subset(eventos,bloque %in% sprintf("B%02d",1:6) & !(camara %in% c("RAS","",NA)))
camaras <- subset(camaras,bloque %in% sprintf("B%02d",1:6) )
## x <- extract(dist.conucos,camaras[,c("lon","lat")])
## camaras$dcon <- (x-mean(x))/sd(x)

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

sC <- data.frame(camaras[match(rownames(obs),camaras$cdg),c("bloque","H","h","dcon","wcon","dcom","buf.fragmen","ndvi.mu","grp","dras","caza.celda")])
sC$bloque <- droplevels(sC$bloque)
for (k in c("H","h","dcon","wcon","dras","ndvi.mu","buf.fragmen")) {
   sC[,k] <- (sC[,k]-mean(sC[,k]))/sd(sC[,k])
}


nsim.val <- 10000 # change to 1000 for manuscript results
mi.spp <- levels(droplevels(eventos$species))[slcspp]
print(mi.spp)
obs <- mtz

for (k in  seq(along=eventos$species)[eventos$species ==mi.spp]) {
   obs[eventos[k,"cdg"],eventos[k,"sessions"]] <-    obs[eventos[k,"cdg"],eventos[k,"sessions"]] + eventos[k,"number.of.animals"]
}


UMF <- unmarkedFrameOccu((obs[ss,]>0)+0,
siteCovs=sC[ss,,drop=F],
obsCovs=list(date=obsDate[ss,],sfrz=sfrz[ss,]/21))
mi.rda <- sprintf("%s/Rdata/occuRN/%s.rda",script.dir,mi.spp)
fm01 <- occuRN(~ dras+sfrz+date ~ buf.fragmen+dcon, UMF,K=50)
fm03 <- occuRN(~ dras+sfrz+date ~ buf.fragmen+I(buf.fragmen^2)+dcon, UMF,K=50)
save(file=mi.rda,UMF,fm01,fm03)

ts03 <- mb.gof.test(fm03,nsim=nsim.val,maxK=50,parallel = TRUE)
save(file=mi.rda,UMF,fm01,fm03,ts03)

fm11 <- occuRN(~ dras+sfrz+date ~ buf.fragmen+wcon, UMF,K=50)
fm13 <- occuRN(~ dras+sfrz+date ~ buf.fragmen+I(buf.fragmen^2)+wcon, UMF,K=50)
ts13 <- mb.gof.test(fm13,nsim=nsim.val,maxK=50,parallel = TRUE)
save(file=mi.rda,UMF,fm01,fm03,ts03,fm11,fm13,ts13)


fm31 <- occuRN(~ dras+sfrz+date ~ ndvi.mu+dcon, UMF,K=50)
fm33 <- occuRN(~ dras+sfrz+date ~ ndvi.mu+I(ndvi.mu^2)+dcon, UMF,K=50)
ts33 <- mb.gof.test(fm33,nsim=nsim.val,maxK=50,parallel = TRUE)

save(file=mi.rda,UMF,fm01,fm03,ts03,fm11,fm13,ts13,fm31,fm33,ts33)

# mi.rda <- sprintf("%s/Rdata/occu/%s.rda",script.dir,mi.spp)
# if (!file.exists(mi.rda)) {
#  fm00 <- occu(~ sfrz+dcom+date ~ H+bsq, UMF,linkPsi= "cloglog")
#  fm01 <- occu(~ sfrz+dcom+date ~ H+bsq+dcon, UMF,linkPsi= "cloglog")
#
#  ts02 <- mb.gof.test(fm01,nsim=nsim.val)
#  save(file=mi.rda,UMF,fm00,fm01,ts02)
# }
