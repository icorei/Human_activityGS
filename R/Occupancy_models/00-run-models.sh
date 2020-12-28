cd $WORKDIR
nohup Rscript --vanilla $SCRIPTDIR/R/GIS/save-spatial-data.R &
ls -lah $SCRIPTDIR/Rdata

nohup Rscript --vanilla $SCRIPTDIR/R/Occupancy_models/occuRN-models.R &

source ~/proyectos/IVIC/Hunting_in_GS/env/load.sh
cd $WORKDIR

R --vanilla
require(knitr)
script.dir <- Sys.getenv("SCRIPTDIR")
mi.arch <- sprintf("%s/R/Occupancy_models/unmarked-models.Rmd",script.dir)
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
