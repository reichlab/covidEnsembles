# covidEnsembles
R package for building ensembles of covid forecasts

## Installation

You can install this package from GitHub by running

```
devtools::install_github('https://github.com/reichlab/covidEnsembles')
```

## Overview of workflow for application

Current status: equally weighted ensemble for cumulative deaths

 * Data are pulled in from Zoltar in [code/application/download_zoltar_forecasts.R](https://github.com/reichlab/covidEnsembles/blob/master/code/application/download_zoltar_forecasts.R)
 * Creation of equally weighted ensemble in [code/application/build_ensembles.R](https://github.com/reichlab/covidEnsembles/blob/master/code/application/build_ensembles.R)
