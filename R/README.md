# _R_ folder

This folder contains an R-script file to fit RN-models for each species.

This file can be run to fit the models for a single species:

```sh
source ~/proyectos/IVIC/Human_activityGS/env/load.sh
cd $WORKDIR
Rscript --vanilla $SCRIPTDIR/Rdata/occuRN
```

You can edit the code to include a for-loop to iterate for all species, or use the `pbs` file to call a PBS-array job to fit all models in parallel. See folder [/pbs] for details.
