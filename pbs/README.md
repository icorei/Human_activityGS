
## Flujo de trabajo para  Katana @ UNSW

El codigo ha sido desarrollado para correr el análisis en el cluster computacional de Katana ([Universidad de New South Wales](https://github.com/unsw-edu-au)).

Enlaces a la documentación de Katana:
* [Katana HPC](https://unsw-restech.github.io/index.html)
* [Katana OnDemand](https://unsw-restech.github.io/using_katana/ondemand.html).

En Linux he configurado la conexión a través de un terminal `bash` para reconocer el número de usuario (`zID`) y el uso de [identificación con una llave de SSH pública](https://www.ssh.com/ssh/public-key-authentication).

Uso las herramientas `ssh` y `scp` para copiar los archivos a través del nodo *katana data mover* (kdm).


Para correr una sesión interactiva y probar el código:

```sh
ssh -X $zID@katana.restech.unsw.edu.au
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
cd $WORKDIR
qsub -I -X -l select=1:ncpus=32:mem=16gb,walltime=4:00:00

cd $TMPDIR
module add R/4.0.2
source ~/proyectos/IVIC/Human_activityGS/env/load.sh

```

Una vez completada la secuencia en un script de `R`, se puede usar PBS para correr los análisis en paralelo usando *[array jobs](https://unsw-restech.github.io/using_katana/running_jobs.html#array-jobs)*

```sh
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
cd $WORKDIR
ls -lah $SCRIPTDIR/Rdata/occuRN

qsub -J 1-2 $SCRIPTDIR/pbs/run-occu-models.pbs
qsub -J 3-8 $SCRIPTDIR/pbs/run-occu-models.pbs
qsub -J 9-29 $SCRIPTDIR/pbs/run-occu-models.pbs
##qsub -J 20-29:9 $SCRIPTDIR/pbs/run-occu-models.pbs
qstat -tu $(whoami)

```

Después de finalizar los análisis, sólo hace faltar correr rmarkdown para generar los documentos con el resumen de los resultados:

```sh
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
cd $WORKDIR
qsub -I -X -l select=1:ncpus=1:mem=8gb,walltime=1:00:00

cd $TMPDIR
module add R/4.0.2
source ~/proyectos/IVIC/Human_activityGS/env/load.sh

Rscript -e "rmarkdown::render('~/proyectos/IVIC/Human_activityGS/documentation/supplementary-methods-1.Rmd',output_format='pdf_document')"
Rscript -e "rmarkdown::render('~/proyectos/IVIC/Human_activityGS/documentation/supplementary-methods-2.Rmd',output_format='pdf_document')"

```
