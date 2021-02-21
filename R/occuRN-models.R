#!R --vanilla

#################
## Shifting cultivation and hunting across the savanna-forest mosaic in the Gran Sabana, Venezuela
## @jrfep :: creado el 2021-01-29
## Ajuste de modelo "Royle Nicholson" para estimar la abundancia latente a partir de historias de detección
## Fit of Royle Nicholson models to estimate latent abundance from detection history data
## Script structure:
## PRELIMINARES / PRELIMINARIES
## PASO 1: formato de los datos / STEP 1: set up data
## PASO 2: ajuste del modelo / STEP 2: model fit
#################

## Preliminares
#################
init.time <- Sys.time()
cat(sprintf("Inicio %s\n",init.time))

##paquetes necesarios / load packages
require(unmarked)
require(AICcmodavg)
require(chron)
require(raster)
require(cluster)
require(MuMIn)
require(dplyr)

## establecer la ubicación de la carpeta de trabajo y el repositorio local
## Set up working directory and local repository
if (Sys.getenv("WORKDIR") != "") {
   work.dir <- Sys.getenv("WORKDIR")
   setwd(work.dir)
} else {
   work.dir <- getwd()
}
if (Sys.getenv("SCRIPTDIR") != "") {
   script.dir <- Sys.getenv("SCRIPTDIR")
} else {
   script.dir <- work.dir
}


## Variables del entorno / BASH environment variables:
## Si la rutina se corre desde una tarea PBS, se usa la variable `PBS_ARRAY_INDEX` para seleccionar la especie
## si no se establece un valor predeterminado de 1 o se cambia manualmente
## If run within a PBS job, we use PBS_ARRAY_INDEX, otherwise use a predefined value to select the first species, or edit manually to select other species

if (Sys.getenv("PBS_ARRAY_INDEX") != "") {
   slcspp <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
   cat(sprintf("PBS ARRAY INDEX is %s\n",slcspp))
} else {
   slcspp <- 1
}

## Número de simulaciones para el cálculo de bootstrap
## Number of simulations for the bootstrap of the GOF test
nsim.val <- 10000 # 10 o 100 para pruebas, 10.000 para versión final
# use 10 or 100 for testing, 10,000 for final version

## Funciones: definimos una función para formatear la matriz de observaciones
## Functions: define a function to format the observation matrix

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


## PASO 1: formato de los datos
## STEP 1: data set up
#################
## cargar datos para el análisis / load data for analysis:
GIS.data <- sprintf("%s/Rdata/GIS.rda",script.dir)
load(GIS.data)

## usar `dplyr` para filtrar y reformatear los datos de las cámaras / use `dplyr` para filtering, transmutation y reshaping camera data

camaras %>% filter(bloque %in% sprintf("B%02i",1:6)) %>% # filtrar bloques
  mutate(bloque=droplevels(bloque),
    fecha1 = chron(dates.=as.character(fecha.act), times.=as.character(hora.act),
      format = c(dates = "y-m-d", times = "h:m:s")), # fecha y hora de activación
    fecha2 = chron(dates.=as.character(fecha.desact.real),
    times.=as.character(hora.desact.real),
      format = c(dates = "y-m-d", times = "h:m:s")),# fecha y hora de des-activación
    cdg = as.character(ID.original)) %>% # formatear fechas
  group_by(cdg) %>%
  summarise(lat=mean(lat), lon=mean(lon), bloque=unique(bloque),
    hunting=unique(factor(caza.celda>0)), grp=unique(grp), H=mean(H), h=mean(h),
    tree_0500m=mean(tree_0500m), tree_1000m=mean(tree_1000m),
    tree_2500m=mean(tree_2500m), tree_5000m=mean(tree_5000m),
    drios=mean(drios), bsq=mean(buf.fragmen), ndvi=mean(ndvi.mu),
    fecha1=min(fecha1), fecha2=max(fecha2)) %>% # agrupar por código de la cámara
  mutate(duration=as.numeric(fecha2-fecha1)) -> # calcular duración del periodo de actividad de la cámara
    cam.data

## calcular distancia de cámaras a las comunidades / distance to Pemon communities
d1 <- pointDistance(cam.data[,c("lon","lat")],
  coordinates(comunidades), lonlat=T, allpairs=T)
cam.data$dcom <- apply(d1,1,min)

## calcular distancia de cámaras a los conucos / distance to conucos
d1 <- pointDistance(cam.data[,c("lon","lat")],
   coordinates(conucos)[,1:2], lonlat=T, allpairs=T)
cam.data$dcon <- apply(d1,1,min)

## calcular distancia de cámaras a los conucos/comunidades / shortest distance to communities or conucos
cam.data$dhum <- with(cam.data,ifelse(dcon<dcom,dcon,dcom))

## calcular densidad de los conucos usando fórmula de IDW / calculate conuco density using Inverse Distance Weighting
p <- 0.25
w <- 1/((d1)^p)
cam.data$wcon <- apply(w,1,sum)

## calcular densidad de los rastros usando fórmula de IDW / calculate density of animal tracks using Inverse Distance Weighting
drastros <- pointDistance(subset(eventos,camara %in% "RAS")[,c("long","lat")], cam.data[,c("lon","lat")], lonlat=TRUE)
p <- 0.25
w <- 1/((drastros)^p)
cam.data$dras <- apply(w,2,sum)

## seleccionar especie para el análisis (ver PRELIMINARES: Variables del entorno) / select species (see PRELIMINARIRES: BASH environment variables)

mi.spp <- levels(droplevels(eventos$species))[slcspp]
cat(sprintf("La especie es %s (nr. %s)",mi.spp,slcspp))

## si se desea hacer el análisis para todas las especies en secuencia:
## uncomment following line if doins all analysis in sequence:
## for (mi.spp  in levels(droplevels(eventos$species))) {

## usar `dplyr` para filtrar y reformatear los datos de los eventos / use `dplyr` para filtering, transmutation y reshaping detection event data

eventos$cdg <- as.character(camaras$ID.original)[
  match(paste(eventos$bloque,eventos$periodo,eventos$camara),
    paste(camaras$bloque,camaras$period,camaras$camera))]

eventos %>% mutate(f1 = chron(dates.=sprintf("%s-%s-%s",ano,mes,dia),
    times.=as.character(hora.ini),
    format = c(dates = "y-m-d", times = "h:m:s")),  # fecha y hora de del evento
   f2 = chron(dates.=sprintf("%s-%s-%s",ano,mes,dia),
    times.=as.character(hora.ini),
    format = c(dates = "y-mon-d", times = "h:m:s"), # alternativa para algunos eventos registrados con formato diferente
    out.format = c(dates = "y-m-d", times = "h:m:s"))) %>%
  transmute(cdg, camara, fotos, species=as.character(species), number.of.animals,
   fecha=chron(ifelse(is.na(f2),f1,f2),
   format = c(dates = "y-m-d", times = "h:m:s"))) %>% # formatear fechas
  filter(cdg %in% cam.data$cdg & species %in% mi.spp) -> # filtrar por código de cámara válido y especie
    event.data

## Definimos los intervalos de observación / observation intervals ('visit' duration)


ini <- chron(dates.="2015-09-21",times.="00:00:00",format = c(dates = "y-m-d", times = "h:m:s"))
visits3 <- ini + seq(from=0,to=210,by=21)


## Usamos una función para formatear las historias de captura (ver PRELIMINARES: Funciones) / Use function to format detection histories (see PRELIMINARIES)
obs <- make.obs.matrix(data.frame(cam.data),visits3,data.frame(event.data))

## Calculamos las covariables de las observaciones / add observation covariates
## esfuerzo de muestreo / sampling effort
sfrz <- make.obs.matrix(data.frame(cam.data),visits3)
## fecha de la observación / observation date
x <- seq(-1,1,length=ncol(obs))
obsDate <- matrix(rep(x,nrow(obs)),nrow=nrow(obs),byrow=T)

## Calculamos las covariables de los sitios / add site covariates
sC <- data.frame(cam.data[match(rownames(obs),cam.data$cdg),
   c("bloque","H","h","dcon","dcom","wcon",
      "dhum","tree_0500m","tree_1000m","tree_2500m","tree_5000m","drios","ndvi","grp","dras","hunting")])
sC$bloque <- droplevels(sC$bloque)

## estandarizamos variables (promedio:0, desviación estándar:1) /
## standardize to zero mean and unit standard deviation
for (k in c("H","h","dcon","dras","ndvi","tree_0500m","tree_1000m","tree_2500m","tree_5000m","drios","dcom","wcon","dhum")) {
   sC[,k] <- (sC[,k]-mean(sC[,k]))/sd(sC[,k])
}

## filtro para incluir datos válidos (mínimo una semana de duración total y menos de 5km al conuco más cercano)
## filter to include only valid data: minimum one week active and less than 5km to nearest conuco

ss <- rownames(obs) %in% subset(cam.data,duration>7 & dcon<5000)$cdg

## Datos en el formato requerido para la funcion occuRN / assign to an unmarked data frame
UMF <- unmarkedFrameOccu((obs[ss,]>0)+0,
  siteCovs=sC[ss,,drop=F],
  obsCovs=list(date=obsDate[ss,],sfrz=sfrz[ss,]/21))

## guardar resultados en / save results in:
mi.rda <- sprintf("%s/Rdata/occuRN/%s.rda",script.dir,mi.spp)

## ajustar modelos / fit models
(fm01 <- occuRN(~ dras+sfrz+date ~ tree_1000m+dcon+drios, UMF,K=50)) # componente lineal / lineal component
(fm03 <- occuRN(~ dras+sfrz+date ~ tree_1000m+I(tree_1000m^2)+dcon+drios, UMF,K=50)) # componente cuadrátrico / quadratic component
 save(file=mi.rda,UMF,fm01,fm03)
 print(Sys.time())

## prueba de bondad de ajuste basado en bootstrap / Goodness of fit test
## con un nsim.val elevado -> usar parallel=TRUE y aumentar el número de procesadores en PBS / To optimize this, use a high value of nsim.val and parallel option and increase the number of cores in the PBS job file

ts03 <- mb.gof.test(fm03,nsim=nsim.val,maxK=50,parallel = TRUE)
 save(file=mi.rda,UMF,fm01,fm03,ts03)
 print(Sys.time())

## ajustar modelos para todas las combinaciones de variables
## fit models for all combinations of variables
if (ts03$c.hat.est>1) {
  oms01 <- dredge(fm01,rank="QAICc",chat=ts03$c.hat.est)
  oms03 <- dredge(fm03,rank="QAICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),),chat=ts03$c.hat.est)
} else {
  oms01 <- dredge(fm01,rank="AICc")
  oms03 <- dredge(fm03,rank="AICc",subset=dc(lam(tree_1000m),lam(I(tree_1000m^2)),))
}

## promedios ponderados de los modelos
## weighted averages of model parameters using AICc- or QAICc- weights
mavg01 <- model.avg(oms01, subset = delta < 10,fit=T)
mavg03 <- model.avg(oms03, subset = delta < 10,fit=T)
 save(file=mi.rda,UMF,fm01,fm03,ts03,oms01,oms03,mavg01,mavg03)
 print(Sys.time())

## renombrar archivos con el nombre de la especie / rename files to include species name:
for (k in c("UMF","fm01","fm03","ts03","oms01","oms03","mavg01","mavg03")) {
  assign(sprintf("%s.%s",k,mi.spp),get(k))
}

## Guardar resultados / save final results
save(file=mi.rda,list=ls(pattern=mi.spp))

## si se desea hacer el análisis para todas las especies en secuencia:
## if running the analysis sequentially, uncomment following line:
## } ## cerrar paréntesis de  `for (mi.spp  in levels(droplevels(eventos$species))) {`

end.time <- Sys.time()
cat(sprintf("Fin %s\n",end.time))
print(end.time-init.time)
