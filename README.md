
<!-- README.md is generated from README.Rmd. Please edit that file -->
Project [Kamino](http://starwars.wikia.com/wiki/Kamino)
=======================================================

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![LICENSE.](https://img.shields.io/badge/license-OGL--3-brightgreen.svg?style=flat)](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

The fisheries sector is a significant economic and social driver in many rural and coastal communities and potentially in areas classed as socially deprived. The fisheries sector, comprising capture fisheries, aquaculture and processing activities, is exposed to a range of environmental and human factors such as over fishing, habitat destruction, pollution, climate change as well as regional social, economic and political change. These factors have impacted upon the productivity, profitability and social benefits in coastal communities.

The Data Science Campus has been investigating the use of ONS data in providing government organisations with low area economic information without breaching the legal obligation to protect business information. The level of detail and resolution required in the data, the greater the risk of it being disclosive. This project has explored methods of providing insight into datasets while preserving high resolution of both the geographical areas represented and specificity of the industries considered.

This package contains a Shiny application which consists of two main outputs (a map and a sankey widget) controlled by a panel of selection controls.

The app combines MMO fleet landings data, Companies House register data and The ONS Inter-Departmental Business Register to provide an analysis dashboard for the UK Fishing Industry.

This prototype has been developed as a potential system by including other datasets to aid analysis of large spatial information.

Installation
------------

You can install kamino from github with:

``` r
# install.packages("devtools")
devtools::install_github("datasciencecampus/kamino")
```

Usage
-----

To use the app, once you have it installed run

``` r
library(kamino)
run_fisheries_app()
```
