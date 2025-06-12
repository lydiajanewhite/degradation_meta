# Analysis that accompanies White and Norkko 2025

## Introduction

This repository gathers the scripts used for the analyses part of the manuscript: "A path towards appropriate degradation experiments for assessing carbon sequestration potential of macroalgae" by White and Norkko.

## Input data

Files containing the underlying data for all figures in the main text and supplementary information, and DOIs to all original sources where data was extracted from can be found [here](https://doi.org/10.5061/dryad.44j0zpcsb)

## Scripts

The project contains 7 scripts located in the `script` directory:

`1_dataextract_functions.R` includes functions to fit single phase exponential decay curve models to each data set of biomass or carbon loss over time, separate functions fit models with and without a refractory term, and use a range of starting values or self-starting functions.

`2_dataextract_k.R` collates all published data on macroalgal degradation rates, explains how datasets were extracted from figures downloaded from published articles using metaDigitise and fits single phase exponential decay curve models to each dataset

`3_dataextract_temperature.R` collates published data on temperature, light and oxygen from degradation experiments when averages were not reported in the study

`4_analysis_upsetplot.R` synthesises data at the study and experiment level, summarising experiment duration, depth, sampling frequency, habitat etc and creates figure 3

`5_analysis_k_exploration.R` explores how degradation constant k varies with different biological, environmental and experimental variables

`6_analysis_k_exploration_browns.R` explores how degradation constant k of brown forest forming macroalgae varies with different biological, environmental and experimental variables, and sensitivity of carbon export models to k and created figure 4

`7_supplementary_temperature_k_regressions.R` explores how degradation constant k varies with temperature and latitude and creates figure S2 and S3 in supplementary info
