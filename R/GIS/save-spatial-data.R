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
   load(GIS.data)
}


if (!exists("conucos")) {
   # unzip shapefiles from local repository to working directory
   unzip(sprintf("%s/input/sampling/conucos.zip",script.dir))
   ## read both grids and join them
   conucos <- shapefile("conucos.shp")
   save(file=GIS.data,conucos)
}


if (!exists("grd")) {
   # unzip shapefiles from local repository to working directory
   unzip(sprintf("%s/input/sampling/grid2km_Kavanayen.zip",script.dir))
   unzip(sprintf("%s/input/sampling/grid2km_Warapata.zip",script.dir))
   ## read both grids and join them
   grd1 <- shapefile("grid2km_Warapata.shp")
   grd2 <- shapefile("grid2km_Kavanayen.shp")
   grd <- rbind(grd1,grd2)
   save(file=GIS.data,conucos,grd)
}

f (!exists("vbsq")) {
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

   r0 <- raster("GFC-2019-v1.7/Hansen_GFC-2019-v1.7_lossyear.tif")
   pbsq <- crop(r0,e)
   xy <- xyFromCell(pbsq,1:ncell(pbsq))
   pbsq1 <- rbind(xy[(values(pbsq) %in% 11:16) & xy[,1] > -61.4,],
    xy[(values(pbsq) %in% 13:18) & xy[,1] < -61.4,])
    d0 <- distanceFromPoints(rbsq,pbsq1)
    d1 <- disaggregate(d0,10)
    dist.dbsq <- resample(d1,vbsq)
    if (!inMemory(dist.dbsq))
      dist.dbsq <- readAll(dist.dbsq)

   save(file=GIS.data,conucos,grd,vbsq,rgrd,rbsq,dist.conucos,dist.dbsq)
}


if (!exists("frs.c")) {
   frs <- shapefile("GS_M6_FIRES.shp")
   ## cortar capa de fuego, usar solo datos con CONFIDENCE mayor a 40
   frs.c <- subset(frs,frs@data$confidence>40)
   frs.c <- crop(frs.c,e)
   frs.c@data$fch <- chron(dates.=frs.c@data$acq_date,format="y/m/d

   frs1 <- rbind(subset(frs.c,years(fch) %in% 2011:2016 & coordinates(frs.c)[,1] > -61.4),
      subset(frs.c,years(fch) %in% 2013:2018 & coordinates(frs.c)[,1] < -61.4))

  d0 <- distanceFromPoints(rbsq,frs1)
  d1 <- disaggregate(d0,10)
  dist.frs <- resample(d1,vbsq)
  if (!inMemory(dist.frs))
    dist.frs <- readAll(dist.frs)

    save(file=GIS.data,conucos,grd,vbsq,rgrd,rbsq,dist.conucos,frs.c,dist.dbsq,dist.frs)
}

if (!exists("tracks")) {
  tracks <- shapefile("tracks.shp")
  track_points <- shapefile("track_points.shp")
  save(file=GIS.data,conucos,grd,vbsq,rgrd,rbsq,dist.conucos,frs.c,tracks,track_points,dist.dbsq,dist.frs)
}




nL <- raster("nightlights/SVDNB_npp_20140101-20140131_vcmcfg_v10.tif")
nLights <- crop(nL,grd)
