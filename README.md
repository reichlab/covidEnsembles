# covidEnsembles
R package for building ensembles of covid forecasts

## Installation

You will need Python 3 and R 3.x or 4.x, as well as the packages listed below:

Python packages:

* `pandas` and `requests`. To install, run the following in a terminal:

```
pip install pandas
pip install requests
```

R packages:

* The `quantgen` package by Ryan Tibshirani.  Follow the instructions at https://github.com/ryantibs/quantgen.

* Other packages: `tidyverse, MMWRweek, magrittr, Matrix, NlcOptim, zeallot, googledrive, yaml, here`.  In an R session, run the following:

```
install.packages(c("tidyverse", "MMWRweek", "magrittr", "Matrix", "NlcOptim", "zeallot", "googledrive", "yaml", "here"))
```

* The `covidEnsembles` package.  Clone the [repository](https://github.com/reichlab/covidEnsembles) and install as an R package.  In a terminal, you can run the following commands:

```
git clone https://github.com/reichlab/covidEnsembles
R CMD INSTALL covidEnsembles
```

* The `covidData` package.  Clone the [repository](https://github.com/reichlab/covidData).  You don't need to install it as an R package immediately; this will be done as part of the automated ensemble build process.

```
git clone https://github.com/reichlab/covidData
```

1. Clone the [`covidData` repository](https://github.com/reichlab/covidData). If you are cloing this repo for the first time on your machine, you will need to manually create a `covidData/data-raw/JHU` directory and leave it empty.
1. In a terminal window, navigate to `covidData/code/data-processing/` and run the `make all` command.

## Overview of workflow for application

 * Weekly ensemble build in [code/application/build_ensembles.R](https://github.com/reichlab/covidEnsembles/blob/master/code/application/build_ensembles.R)
 * Retrospective comparison of ensemble methods in [code/application/retrospective-qra-comparison](https://github.com/reichlab/covidEnsembles/tree/master/code/application/retrospective-qra-comparison)

## Brief Overview of Ensemble Methods

Currently implemented:

 * window of past weeks for parameter estimation
 * mean imputation for missing forecasts followed by redistributing weight according to effective weight received in training set
 * combine by per-quantile median, or per-quantile weighted mean
 * for weighted mean, model weights separately for all quantiles, one weight per model, or 3 groups of quantiles

Among these options, there is not consistent or strong evidence that anything is better than a per-quantile median forecast, which is what we are using now.

Unimplemented ideas:
 * weighted median
 * enforce or encourage consistency between incident and cumulative death ensemble forecasts
 * weights per horizon
