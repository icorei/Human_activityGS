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
}

## set working directory
setwd(work.dir)
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
if (file.exists(GIS.data)) {
   obj.list <- (load(GIS.data))
} else {
   obj.list <- c()
}


if (!exists("conucos")) {
   # unzip shapefiles from local repository to working directory
   unzip(sprintf("%s/input/sampling/conucos.zip",script.dir))
   unzip(sprintf("%s/input/sampling/comunidades_GS.zip",script.dir))
      ## read both grids and join them
   conucos <- shapefile("conucos.shp")
   comunidades <- shapefile("comunidades.shp")
   obj.list <- c(obj.list,"conucos","comunidades")
   save(file=GIS.data,list=obj.list)
}


if (!exists("grd")) {
   # unzip shapefiles from local repository to working directory
   unzip(sprintf("%s/input/sampling/grid2km_Kavanayen.zip",script.dir))
   unzip(sprintf("%s/input/sampling/grid2km_Warapata.zip",script.dir))
   ## read both grids and join them
   grd1 <- shapefile("grid2km_Warapata.shp")
   grd2 <- shapefile("grid2km_Kavanayen.shp")
   grd <- rbind(grd1,grd2)
   obj.list <- c(obj.list,"grd")
   save(file=GIS.data,list=obj.list)
}

if (!exists("vbsq")) {
   r0 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_treecover2000.tif")
   r1 <- aggregate(r0,10)
    e <- extent(-61.869,-61.005,5.453,5.750)
   vbsq <- crop(r0,e)
   if (!inMemory(vbsq))
    vbsq <- readAll(vbsq)
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

   r0 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_lossyear.tif")
   pbsq <- crop(r0,e)
   xy <- xyFromCell(pbsq,1:ncell(pbsq))
   ## five years prior to sampling
   pbsq1 <- rbind(xy[(values(pbsq) %in% 12:16) & xy[,1] > -61.4,],
    xy[(values(pbsq) %in% 14:18) & xy[,1] < -61.4,])
   ## last five years
    pbsq2 <- rbind(xy[(values(pbsq) %in% 15:19)])

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

      obj.list <- unique(c(obj.list,"rgrd","vbsq","dist.conucos","dist.dbsq","dist.comunidades","current.dbsq"))
      save(file=GIS.data,list=obj.list)

   }

if (!exists("dist.caza1")) {

   eventos <- read.csv2(sprintf("%s/input/fieldwork/Eventos_GS_CAM_RAS_2019.csv",script.dir))
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
}


if (!exists("frs.c")) {
   frs <- shapefile("GS_M6_FIRES.shp")
   ## cortar capa de fuego, usar solo datos con CONFIDENCE mayor a 40
   frs.c <- subset(frs,frs@data$confidence>40)
   frs.c <- crop(frs.c,e)
   frs.c@data$fch <- chron(dates.=frs.c@data$acq_date,format="y/m/d")

   frs1 <- rbind(subset(frs.c,years(fch) %in% 2012:2016 & coordinates(frs.c)[,1] > -61.4),
      subset(frs.c,years(fch) %in% 2014:2018 & coordinates(frs.c)[,1] < -61.4))
   frs2 <- subset(frs.c,years(fch) %in% 2015:2019 )

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
}

if (!exists("tracks")) {
  tracks <- shapefile("tracks.shp")
  track_points <- shapefile("track_points.shp")
  obj.list <- unique(c(obj.list,"tracks","track_points"))
  save(file=GIS.data,list=obj.list)
}




#nL <- raster("nightlights/SVDNB_npp_20140101-20140131_vcmcfg_v10.tif")
#nLights <- crop(nL,grd)
