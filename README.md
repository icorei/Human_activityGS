# Hunting in GS
Abstract Overexploitation of bushmeat in tropical forests has increased in recent years, creating debate about the sustainability of current hunting rates. The Empty Forest hypothesis predicts that current hunting rates in tropical forests can lead to a widespread loss of biodiversity and a reduction in vertebrate abundance. Alternatively, the Garden Hunting hypothesis states that heterogeneous agroforestry landscapes maintain similar species richness as pristine forests, but with species composition dominated by savanna species. Here, we combined cameras trap surveys and spatially explicit dataset on Pemón indigenous hunting in mosaic of savanna and rainforest in the Gran Sabana, Venezuela. We fitted occupancy models and MANOVA to assess how important are human activities (indigenous farming and hunting activity), and landscape characteristics (forest cover and fragmentation) to explain wildlife occupancy, and changes of species composition across landscape. Consistent with Garden Hunting hypothesis predictions, we found higher occurrence of savanna related herbivores in habitat with medium disturbance than in unperturbed habitats. Evidence for decreasing predator’s occurrence in perturbed habitats was mixed, with some species being attracted and other repealed by the human presence and agricultural activity. Although over-hunting reduces population density abundance of targeted game species white-tailed deer, the current scheme of resource use does not seem to produce a generalized pattern of defaunation. Mammal diversity seems to respond to amount and distribution of remaining forest cover, suggesting that deforestation has a larger impact than hunting. The forest cover loss in Gran Sabana has doubled in 2016 – 2018 in compared to 2001-2015 period and new threat as mining has emerged.  We discuss the applicability of the estimates generated as a baseline in this study and opportunities and challenges of wildlife management in complex landscape with high cultural and biological diversity.  .


## Repository structure

### *documents* folder

Contains:

* shp. of study area, Gran Sabana and Canaima NP and  10 blocks of work
* data on hunting _ interviews
* data on species occurance

Also some aditional figures and tables

### *R* folder

Contains code (R scripts) for analysis and output figures in pdf

#### References

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
