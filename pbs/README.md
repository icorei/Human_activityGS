# _pbs_ folder

This folder contains a pbs file to set up a job array in a computing cluster using the PBS program. This code allow parallel computation of the models for all species. Alternatively the models can be fitted sequentially by editing the R-scripts in the [/R] folder.

## Workflow for Katana HPC @ UNSW

This code has been created for running the analysis in the Katana HPC cluster at the ([University of New South Wales](https://github.com/unsw-edu-au)), but can be adapted for similar systems.

Documentation on Katana:
* [Katana HPC](https://unsw-restech.github.io/index.html)
* [Katana OnDemand](https://unsw-restech.github.io/using_katana/ondemand.html).

Connection to the katana login nodes has been configured using using [public SSH key identification](https://www.ssh.com/ssh/public-key-authentication) and `bash` environment variables. I used `ssh` and `scp` to copy files with the *katana data mover* (kdm) node.

To run an interactive session and test the code I use the following command sequence:

```sh
ssh -X $zID@katana.restech.unsw.edu.au
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
cd $WORKDIR
qsub -I -X -l select=1:ncpus=32:mem=16gb,walltime=4:00:00
## wait for job to be scheduled...
cd $TMPDIR
module add R/4.0.2
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
## then run the code...
```

After fine tuning the script, I use PBS to schedule an *[array job](https://unsw-restech.github.io/using_katana/running_jobs.html#array-jobs)*:

```sh
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
cd $WORKDIR
ls -lah $SCRIPTDIR/Rdata/occuRN

qsub -J 1-2 $SCRIPTDIR/pbs/run-occu-models.pbs
qsub -J 3-8 $SCRIPTDIR/pbs/run-occu-models.pbs
qsub -J 9-29 $SCRIPTDIR/pbs/run-occu-models.pbs
qstat -tu $(whoami)

```

After completing all analysis, it is possible to render the rmarkdown documents in folder [/documentation] using an interactive session:

```sh
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
cd $WORKDIR
qsub -I -X -l select=1:ncpus=1:mem=8gb,walltime=1:00:00
## wait for job to be scheduled...
cd $TMPDIR
module add R/4.0.2
source ~/proyectos/IVIC/Human_activityGS/env/load.sh

Rscript -e "rmarkdown::render('~/proyectos/IVIC/Human_activityGS/documentation/supplementary-methods-1.Rmd',output_format='pdf_document')"
Rscript -e "rmarkdown::render('~/proyectos/IVIC/Human_activityGS/documentation/supplementary-methods-2.Rmd',output_format='pdf_document')"

```
