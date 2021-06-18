---
title: "Shifting cultivation and hunting across the savanna-forest mosaic in the Gran Sabana, Venezuela"
author: 'JR Ferrer Paris @jrfep - Izabela Stachowicz @icorei - Ada Y. Sánchez Mercado @adasanchez'
date: "29/1/2021"
---

# Hunting in Gran Sabana, Venezuela

This repository contains data and code for the scientific manuscript _Shifting cultivation and hunting across the savanna-forest mosaic in the Gran Sabana, Venezuela: facing changes_ (Stachowicz et al. 2020)

## Abstract
*Background* Human encroachment and overexploitation of natural resources in the Neotropics is constantly increasing. Indigenous communities all across the Amazon, are trapped between a population rise and a hot debate about the sustainability of hunting rates. The Garden Hunting hypothesis states that shifting cultivation schemes (conucos) used by Amazon indigenous communities may generate favorable conditions, increasing abundance of small and medium wildlife species close to the ‘gardens’ providing game for indigenous hunters.

*Methods* Here, we combined camera trap surveys and spatially explicit interview dataset on Pemón indigenous hunting scope and occurrence in a mosaic of savanna and forest in the Gran Sabana, Venezuela to evaluate to what extent the wildlife resource use corresponds to Garden Hunting hypothesis. We applied the Royle–Nichols model and binomial regression in order to: (1) assess whether abundance of small and medium wildlife species is higher close to conucos and (2) evaluate whether hunters select hunting localities based on accessibility to wildlife resources (closeness to conuco) more than wildlife abundance.

*Results* We find mixed evidence supporting the Garden Hunting hypothesis predictions. Abundance of small and medium species was high close to conucos but the pattern was not statistically significant for most of them. Pemón seem to hunt in locations dominated by forest, where species abundance was predicted to be higher, than in close vicinity to conucos. Hunting scope was focused on the most abundant species located close to the conuco (Cuniculus paca), but also in less abundant and unavailable species (Crax alector, Tapirus terrestris and Odocoileus virginianus).

*Conclusions* Our research provided the first attempt of a systematic sampling survey in the Gran Sabana, generating a quantitative dataset that not only describes the current pattern of wildlife abundance, but sets the base-line to monitor temporal and spatial change in this region of highland Amazon. We discuss the applicability of the estimates generated as a baseline as well as, environmental challenges imposed by economic, social and cultural changes such as mining encroachment for wildlife management.

## Full citation:
> Stachowicz I, Ferrer-Paris JR, Sanchez-Mercado A. 2021. _Shifting cultivation and hunting across the savannaforest mosaic in the Gran Sabana, Venezuela: facing changes_. *PeerJ* 9:e11612 DOI: [10.7717/peerj.11612](http://doi.org/10.7717/peerj.11612)

## Repository structure

This repository includes the following folders:

* *documentation*: R-markdown files and PDF output describing the model fit and analysis
* *env*:
* *pbs*: PBS job files for running the R-code on the [Katana HPC](https://unsw-restech.github.io/index.html) @ [UNSW](https://github.com/unsw-edu-au)
* *R*: Contains code (R scripts) for analysis (model fit)
* *Rdata*: Contains R-data files with the raw observations and spatial data

Each folder has a README file with instructions in English. Files in the documentation folder are written in English. Code in folders _env_, _pbs_ and _R_ include comments in Spanish and English.
