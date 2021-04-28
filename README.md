<p align="center">
  <img src="public/_FigMatrix.png" width="100%">
</p>

# TUS ➦ HSEM | converter

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

This `R` project converts original **Time Use Survey** data sources into **Housing Stock Energy Model** (HSEM) tables for modelling and simulating activity transitions.

The main reason for having a dedicated *TUS project* is <u>to comply with licensing rules</u>. Therefore, upon accessing the sources for the first time, users must request access to the relevant datasets from [UK-Data-Service](https://www.ukdataservice.ac.uk).

<p align="center">
  <img src="public/UKDS Logos_Col_Grey_300dpi.png" width="30%">
</p>

Once granted, the datasets need to be stored (or redirected) as shown at the bottom of this page. (Only the *documentation files* are hard copied to `/myData` because these files are converted into plain text.)

Another reason for having a dedicated *repo* is that **TUS** information is employed along different projects, and so the datasets can be stored and shared in a central repository.

<span style="background-color: #ffb82e"> Please note that...</span> the generation of profiles has been integrated into the [EnHub](github.com/EnHub-UK/Platform) platform.


### Data Sources
|           	|                                                                                                                                                                                                                       	|
|-----------	|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	|
| 2014-2015 	| Study Number **8128 - United Kingdom Time Use Survey, 2014-2015**                                                                                                                                                     	|
|           	| Gershuny, J., Sullivan, O. (2017). United Kingdom Time Use Survey, 2014-2015. Centre for Time Use Research, University of Oxford. [data collection]. UK Data Service. SN: 8128, http://doi.org/10.5255/UKDA-SN-8128-1 	|
| 2000      	| Study Number **4504 - United Kingdom Time Use Survey, 2000**                                                                                                                                                          	|
|           	| Ipsos-RSL and ONS, United Kingdom Time Use Survey, 2000 [computer file]. 3rd Edition. Colchester, Essex: UK Data Archive [distributor], September 2003. SN: 4504.                                                     	|




---

**NOTES**
1. `/myData` folder employs `symlinks` to a centralised database where the original repositories are previously retrieved from [UK-Data-Service](https://www.ukdataservice.ac.uk/get-data.aspx).


```sh
myData
├── AuxiliaryData
│   ├── activities
│   └── allissue
├── UKDA-4504-stata8_se -> <symlink-to>/UK-TUS/UKDA-4504-stata8_se
├── UKDA-8128-stata11_se -> <symlink-to>/UK-TUS/UKDA-8128-stata11_se
└── UKDA-Core -> <symlink-to>/UK-TUS/UKDA-Core
```
