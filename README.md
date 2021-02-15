---
title: "Shifting cultivation and hunting across the savanna-forest mosaic in the Gran Sabana, Venezuela"
author: '@jrfep - @izolina - @adasanchez'
date: "29/1/2021"
---

# Hunting in GS

This repository contains data and code for the scientific manuscript _Shifting cultivation and hunting across the savanna-forest mosaic in the Gran Sabana, Venezuela: Facing changes_ (under review)

Abstract Overexploitation of bushmeat in tropical forests has increased in recent years, creating debate about the sustainability of current hunting rates. The Empty Forest hypothesis predicts that current hunting rates in tropical forests can lead to a widespread loss of biodiversity and a reduction in vertebrate abundance. Alternatively, the Garden Hunting hypothesis states that heterogeneous agroforestry landscapes maintain similar species richness as pristine forests, but with species composition dominated by savanna species. Here, we combined cameras trap surveys and spatially explicit dataset on Pemón indigenous hunting in mosaic of savanna and rainforest in the Gran Sabana, Venezuela.

## Repository structure

* *R* folder: Contains code (R scripts) for analysis (model fit)
* *Rdata* folder: Contains R-data files with the raw observations and spatial data
* *pbs* folder: PBS job files for running the R-code on the Katana HPC @ UNSW
* *documentation*: R-markdown files and PDF output describing the model fit and analysis



## Flujo de trabajo Katana @ UNSW

El codigo ha sido desarrollado para correr el análisis en el cluster computacional de Katana ([Universidad de New South Wales](https://github.com/unsw-edu-au)).

Enlaces a la documentación de Katana:
* [Katana HPC](https://unsw-restech.github.io/index.html)
* [Katana OnDemand](https://unsw-restech.github.io/using_katana/ondemand.html).

En Linux he configurado la conexión a través de un terminal `bash` para reconocer el número de usuario (`zID`) y el uso de [identificación con una llave de SSH pública](https://www.ssh.com/ssh/public-key-authentication).

Uso las herramientas `ssh` y `scp` para copiar los archivos a través del nodo *katana data mover* (kdm):


## Analysis in katana
```sh
 source ~/proyectos/IVIC/Hunting_in_GS/env/load.sh
 cd $WORKDIR
 nohup Rscript --vanilla $SCRIPTDIR/R/GIS/save-spatial-data.R &
 ls -lah $SCRIPTDIR/Rdata

 ##nohup Rscript --vanilla $SCRIPTDIR/R/Occupancy_models/occuRN-models.R &

 source ~/proyectos/IVIC/Hunting_in_GS/env/load.sh
 cd $WORKDIR

 R --vanilla
 require(knitr)
 script.dir <- Sys.getenv("SCRIPTDIR")
 mi.arch <- sprintf("%s/R/Occupancy_models/supplementary-methods-2.Rmd",script.dir)
 mi.arch <- sprintf("%s/R/Occupancy_models/supplementary-methods-1.Rmd",script.dir)
 knitr::opts_chunk$set(warning = FALSE, echo = TRUE, eval = TRUE)

 rmarkdown::render(mi.arch,"all")


 for SPECIE in C.alector C.olivaceus C.paca C.thous C.unicinctus D.imperfecta D.kappleri D.leporina D.marsupialis D.novemcinctus E.barbara H.hydrochaeris L.pardalis L.rufaxilla L.tigrinus L.wiedii M.americana M.gouazoubira M.pratti M.tridactyla N.nasua O.virginianus P.concolor P.maximus P.onca P.tajacu S.venaticus T.major T.pecari T.terrestris T.tetradactyla
 do
    nohup Rscript --vanilla $SCRIPTDIR/R/Occupancy_models/presence-absence-dataframe.R $SPECIE &
 done

 for k in $(seq 1 4)
 do
   for SPECIE in C.alector C.olivaceus C.paca C.thous C.unicinctus D.imperfecta D.kappleri D.leporina D.marsupialis D.novemcinctus E.barbara H.hydrochaeris L.pardalis L.rufaxilla L.tigrinus L.wiedii M.americana M.gouazoubira M.pratti M.tridactyla N.nasua O.virginianus P.concolor P.maximus P.onca P.tajacu S.venaticus T.major T.pecari T.terrestris T.tetradactyla
   do
     nohup Rscript --vanilla $SCRIPTDIR/R/Occupancy_models/single_visit_models.R $SPECIE $k &
   done
 done

```

Start interactive session with graphical session and test...
```sh
ssh -X $zID@katana.restech.unsw.edu.au
source $HOME/proyectos/IVIC/Hunting_in_GS/env/load.sh
cd $WORKDIR
qsub -I -X -l select=1:ncpus=32:mem=16gb,walltime=4:00:00

cd $TMPDIR
module add R/4.0.2
source $HOME/proyectos/IVIC/Hunting_in_GS/env/load.sh

```
Now run:

```sh
ssh -X $zID@katana.restech.unsw.edu.au
source $HOME/proyectos/IVIC/Hunting_in_GS/env/load.sh

cd $WORKDIR
qsub -J 1-2 $SCRIPTDIR/pbs/run-occu-models.pbs
qsub -J 3-29 $SCRIPTDIR/pbs/run-occu-models.pbs
##qsub -J 20-29:9 $SCRIPTDIR/pbs/run-occu-models.pbs
qstat -tu $(whoami)

qsub -I -l select=1:ncpus=2:mem=8gb,walltime=4:00:00
source $HOME/proyectos/IVIC/Hunting_in_GS/env/load.sh
cd $WORKDIR
module add R/4.0.2


```


#### References


Goodness of fit and other steps adapted from chapter five of :
Strimas-Mackey, M., W.M. Hochachka, V. Ruiz-Gutierrez, O.J. Robinson, E.T. Miller, T. Auer, S. Kelling, D. Fink, A. Johnston. 2020. Best Practices for Using eBird Data. Version 1.0. https://cornelllabofornithology.github.io/ebird-best-practices/. Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.5281/zenodo.3620739
Especialmente el capitulo 5
