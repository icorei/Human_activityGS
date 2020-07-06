## merge de los archivos con registros de mamiferos y el de los datos de camaras trampas


setwd("D:/PHD_statistic/EventosGranSabana")
data.dir <- "~D:/PHD_statistic/EventosGranSabana"
EventosGranSabanaCAM_RAS <- read.csv2("D:/PHD_statistic/EventosGranSabana/Eventos_GS_CAM_RAS_bez2b.csv")

require(unmarked)
require(dismo)
require(vegan)

require(raster)
require(gdata)
require(foreign)

## BLOQUE # merge de los archivos con registros de mamiferos y el de los datos de camaras trampas para BLOQUE
GS <- EventosGranSabanaCAM_RAS
tt <- table(paste(GS$bloque,GS$camara),GS$species)
head(tt)

## para Charact_SBOF
GS <- merge(EventosGranSabanaCAM_RAS,camerasGS,by.x=c("bloque","periodo","camara"),
            by.y=c("bloque","period","camera"),all.y=T)
tt <- table(paste(GS$charact_SBOF),GS$species)
head(tt)

###1### calcular Beta diversidad para los bloques turnover y nestness para los grupos de tamano y dieta
tt <- table(paste(GS$bloque),GS$species)
estimateR(tt)
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)
oecosimu(tt,nestedtemp,"r1", nsimul = 1000)
oecosimu(tt,nestedtemp,"quasiswap",nsimul = 1000)
oecosimu(tt,nestedtemp,"r00", nsimul = 1000)

oecosimu(tt,nestedtemp,"r1", nsimul = 99)
oecosimu(tt,nestedtemp,"quasiswap",nsimul = 99)
oecosimu(tt,nestedtemp,"r00", nsimul = 99)

out <- nestedtemp(tt)
#es el valor por fila (bloques)
out$r 
#es el valor por columna (especies)
out$c

tt <- with(subset(GS,tamano %in% "grande"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out

grande1 <-subset(GS$tamano %in% "grande")
boxplot(out$c~"grande")
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,tamano %in% "pequeno"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,tamano %in% "mediano"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,dieta %in% "herb"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,dieta %in% "carniv"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,dieta %in% "insect"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,dieta %in% "omniv"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

##charact_SBOF

tt <- with(subset(GS,dieta %in% "herb"),table(charact_SBOF,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,dieta %in% "carniv"),table(charact_SBOF,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,dieta %in% "insect"),table(charact_SBOF,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,dieta %in% "omniv"),table(charact_SBOF,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

####

tt <- with(subset(GS,tamano %in% "mediano" & dieta %in% "insect"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid", name=T)

tt <- with(subset(GS,tamano %in% "grande" & dieta %in% "insect"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid")

tt <- with(subset(GS,tamano %in% "mediano" & dieta %in% "omniv"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid")

tt <- with(subset(GS,tamano %in% "grande" & dieta %in% "omniv"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid")

tt <- with(subset(GS,tamano %in% "grande" & dieta %in% "carniv"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid")

tt <- with(subset(GS,tamano %in% "pequeno" & dieta %in% "herb"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid")

tt <- with(subset(GS,tamano %in% "mediano" & dieta %in% "herb"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid")

tt <- with(subset(GS,tamano %in% "grande" & dieta %in% "herb"),table(bloque,species))
tt <- tt[,colSums(tt)>0]
nestedbetasor(tt)
out <-nestedtemp(tt)
out
plot (out)
plot(out, kind="incid")



