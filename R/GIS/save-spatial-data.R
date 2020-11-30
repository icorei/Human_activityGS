#!R --vanilla
##paquetes necesarios
#require(dplyr)
#require(tidyr)
require(raster)
##require(gdata)
#require(foreign)
#require(fractaldim)
#require(SDMTools)
require(chron)

## ubicación de la carpeta de trabajo y el repositorio local

if (Sys.getenv("WORKDIR") == "") {
  ## for Izza:
  work.dir <- "D:/PROJECTS/Gran Sabana/Metodologia"
  script.dir <- "???"
} else {
  ## for JR:
  work.dir <- Sys.getenv("WORKDIR")
  script.dir <- Sys.getenv("SCRIPTDIR")
  gis.dir <- Sys.getenv("GISOUT")
}

## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
if (file.exists(GIS.data)) {
   obj.list <- (load(GIS.data))
} else {
   obj.list <- c()
}


   # unzip shapefiles from local repository to working directory
   unzip(sprintf("%s/input/sampling/conucos.zip",script.dir))
   unzip(sprintf("%s/input/sampling/comunidades_GS.zip",script.dir))
      ## read both grids and join them
   conucos <- shapefile("conucos.shp")
   comunidades <- shapefile("comunidades.shp")
   obj.list <- c(obj.list,"conucos","comunidades")
   save(file=GIS.data,list=obj.list)


   # unzip shapefiles from local repository to working directory
   unzip(sprintf("%s/input/sampling/grid2km_Kavanayen.zip",script.dir))
   unzip(sprintf("%s/input/sampling/grid2km_Warapata.zip",script.dir))
   ## read both grids and join them
   grd1 <- shapefile("grid2km_Warapata.shp")
   grd2 <- shapefile("grid2km_Kavanayen.shp")
   grd <- rbind(grd1,grd2)
   obj.list <- c(obj.list,"grd")
   save(file=GIS.data,list=obj.list)

   e <- extent(-61.869,-61.005,5.453,5.750)
   r0 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_treecover2000.tif")
   r1 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_lossyear.tif")
   fbsq <- vbsq <- crop(r0,e)
   pbsq <- crop(r1,e)
   values(vbsq)[values(pbsq) %in% (1:15)] <- 0
   values(fbsq)[values(pbsq)>0] <- 0
   if (!inMemory(vbsq))
    vbsq <- readAll(vbsq)

   if (!inMemory(fbsq))
    fbsq <- readAll(fbsq)


    r1 <- aggregate(r0,10)
   rbsq <- crop(r1,e)
   rgrd <- rasterize(grd,vbsq)
   if (!inMemory(rgrd))
    rgrd <- readAll(rgrd)
   d0 <- distanceFromPoints(rbsq,coordinates(conucos)[,1:2])
   d1 <- disaggregate(d0,10)
   dist.conucos <- resample(d1,vbsq)
   if (!inMemory(dist.conucos))
    dist.conucos <- readAll(dist.conucos)
   d0 <- distanceFromPoints(rbsq,coordinates(comunidades))
   d1 <- disaggregate(d0,10)
   dist.comunidades <- resample(d1,vbsq)
   if (!inMemory(dist.comunidades))
    dist.comunidades <- readAll(dist.comunidades)


   xy <- xyFromCell(pbsq,1:ncell(pbsq))
   ## five years prior to sampling
   pbsq1 <- rbind(xy[(values(pbsq) %in% 11:15) & xy[,1] > -61.4,],
    xy[(values(pbsq) %in% 13:17) & xy[,1] < -61.4,])
   ## last five years
    pbsq2 <- xy[(values(pbsq) %in% 11:19),]

    d0 <- distanceFromPoints(rbsq,pbsq1)
    d1 <- disaggregate(d0,10)
    dist.dbsq <- resample(d1,vbsq)
    if (!inMemory(dist.dbsq))
      dist.dbsq <- readAll(dist.dbsq)

   d0 <- distanceFromPoints(rbsq,pbsq2)
   d1 <- disaggregate(d0,10)
   current.dbsq <- resample(d1,vbsq)
   if (!inMemory(current.dbsq))
     current.dbsq <- readAll(current.dbsq)

      obj.list <- unique(c(obj.list,"rgrd","vbsq","fbsq","dist.conucos","dist.dbsq","dist.comunidades","current.dbsq"))
      save(file=GIS.data,list=obj.list)


   eventos <- read.csv2(sprintf("%s/input/fieldwork/Event_GS_CAM_RAS_bird_mam.csv",script.dir))
   eventos <- subset(eventos,!species %in% "")
   eventos$species <- droplevels(eventos$species)

   camaras <- read.csv2(sprintf("%s/input/fieldwork/Camaras_GS_2019.csv",script.dir))

   caza.reciente <- subset(camaras,caza.celda %in% 1)[,c("lon","lat")]
   d0 <- distanceFromPoints(rbsq,caza.reciente)
   d1 <- disaggregate(d0,10)
   dist.caza1 <- resample(d1,vbsq)
   if (!inMemory(dist.caza1))
    dist.caza1 <- readAll(dist.caza1)
   caza.pasada <- subset(camaras,caza.celda %in% 2)[,c("lon","lat")]
   d0 <- distanceFromPoints(rbsq,caza.pasada)
   d1 <- disaggregate(d0,10)
   dist.caza2 <- resample(d1,vbsq)
   if (!inMemory(dist.caza2))
    dist.caza2 <- readAll(dist.caza2)


   obj.list <- unique(c(obj.list,"eventos","camaras","dist.caza1","dist.caza2"))
   save(file=GIS.data,list=obj.list)

   frs <- shapefile("GS_M6_FIRES.shp")
   ## cortar capa de fuego, usar solo datos con CONFIDENCE mayor a 40
   frs.c <- subset(frs,frs@data$confidence>40)
   frs.c <- crop(frs.c,e)
   frs.c@data$fch <- chron(dates.=frs.c@data$acq_date,format="y/m/d")

   frs1 <- rbind(subset(frs.c,years(fch) %in% 2011:2015 & coordinates(frs.c)[,1] > -61.4),
      subset(frs.c,years(fch) %in% 2013:2017 & coordinates(frs.c)[,1] < -61.4))
   frs2 <- subset(frs.c,years(fch) %in% 2011:2020 )

  d0 <- distanceFromPoints(rbsq,frs1)
  d1 <- disaggregate(d0,10)
  dist.frs <- resample(d1,vbsq)
  if (!inMemory(dist.frs))
    dist.frs <- readAll(dist.frs)

 d0 <- distanceFromPoints(rbsq,frs2)
 d1 <- disaggregate(d0,10)
 current.frs <- resample(d1,vbsq)
 if (!inMemory(current.frs))
   current.frs <- readAll(current.frs)
   obj.list <- unique(c(obj.list,"frs.c","dist.frs","current.frs"))
   save(file=GIS.data,list=obj.list)

  tracks <- shapefile("tracks.shp")
  track_points <- shapefile("track_points.shp")
  obj.list <- unique(c(obj.list,"tracks","track_points"))
  save(file=GIS.data,list=obj.list)



  ## two events (camera and ras) outside grid, are they valid? -> YES, we will use proximity to asign to a cell in the grid
  ## points(subset(camaras,is.na(grid))[,c("lon","lat")],cex=4)

  eventos$grid <-  extract(rgrd,eventos[,c("long","lat")])
  d1 <- pointDistance(eventos[,c("long","lat")], coordinates(grd), lonlat=T,allpairs=T)
  ss <- is.na(eventos$grid)
  eventos$grid[ss] <- apply(d1[ss,],1,which.min)

  camaras$grid <- extract(rgrd,camaras[,c("lon","lat")])
  d1 <- pointDistance(camaras[,c("lon","lat")], coordinates(grd), lonlat=T,allpairs=T)
  ss <- is.na(camaras$grid)
  camaras$grid[ss] <- apply(d1[ss,],1,which.min)

  tps <- crop(track_points,vbsq)
  tps@data$grid <- extract(rgrd,tps)
  d1 <- pointDistance(coordinates(tps), coordinates(grd), lonlat=T,allpairs=T)

  #plot(tps,col=is.na(tps@data$grid)+1)
  #plot(grd,add=T)
  # hist(apply(d1[!is.na(tps@data$grid),],1,min))
  ss <- is.na(tps$grid) & (apply(d1,1,min) < 1500)
  tps$grid[ss] <- apply(d1[ss,],1,which.min)

  tps <- subset(tps,!is.na(grid))
  ##table(camaras$caza.celda,camaras$Si.se.caza.aqui)

 obj.list <- unique(c(obj.list,"camaras","eventos","tps"))
  save(file=GIS.data,list=obj.list)


#nL <- raster("nightlights/SVDNB_npp_20140101-20140131_vcmcfg_v10.tif")
#nLights <- crop(nL,grd)


### MODIS SubSets
mSSt <- function(x,ll=-2000,ul=10000,cf=0.0001,os=0,setNA=0) {
    if (!is.na(ll))
        x[x<ll] <- NA
    if (!is.na(ul))
        x[x>ul] <- NA

    x<- (x*cf) + os
    if (!is.na(setNA))
        x[x==setNA] <- NA
    return(x)
}

archs <- dir(sprintf("%s/mapas/Venezuela/250m_16_days_NDVI.006",gis.dir),"tif$",full.names=T)
qrchs <- dir(sprintf("%s/mapas/Venezuela/250m_16_days_VI_Quality.006",gis.dir),"tif$",full.names=T)

qry <- matrix(nrow=nrow(camaras),ncol=length(archs),
                      dimnames=list(c(apply(unique(camaras[,1:3]),1,paste,collapse=":")),
                          sapply(archs,function(x) strsplit(x,"\\.")[[1]][3])))

for (aa in archs) {
    qa <- strsplit(aa,"\\.")[[1]][3]
    qry[,qa] <-
        as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",camaras$lon,camaras$lat),collapse="\n"),aa),intern=T))
}
 ndvi.camara <- mSSt(qry)


 ## quality of NDVI measurement
 ## https://gis.stackexchange.com/questions/163457/handling-mod13q1-ndvi-product-quality-assessment-qa-flags
 ## https://vip.arizona.edu/documents/MODIS/MODIS_VI_UsersGuide_June_2015_C6.pdf
 qry <- matrix(nrow=nrow(camaras),ncol=length(qrchs),
                       dimnames=list(c(apply(unique(camaras[,1:3]),1,paste,collapse=":")),
                           sapply(archs,function(x) strsplit(x,"\\.")[[1]][3])))

 for (aa in qrchs) {
     qa <- strsplit(aa,"\\.")[[1]][3]
     qry[,qa] <-
         as.numeric(system(sprintf("echo '%s'| gdallocationinfo -valonly -wgs84 %s ",paste(sprintf("%s %s",camaras$lon,camaras$lat),collapse="\n"),aa),intern=T))
 }
  viq.camara <- R.utils::intToBin(qry)

 ## VI quality (pixels 0 and 1) 00 produced good quality, 01 produced check QA, 10 produced but cloudy...
 ## usefulness (pixels 2-5) 0000 highest -- 1100 lowest -- 1111 not useful

 obj.list <- unique(c(obj.list,"ndvi.camara","viq.camara"))
  save(file=GIS.data,list=obj.list)
