# TUS ➦ HSEM | converter

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

This R project processes the original **UK-TUS** data repository into *Housing Stock Energy Model (HSEM)* tables for modelling and simulating activity transitions.

<p align="center">
  <img src="public/_FigMatrix.png" width="100%">
</p>


## Data Sources

|           	|                                                                                                                                                                                                                       	|
|-----------	|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	|
| 2014-2015 	| Study Number **8128 - United Kingdom Time Use Survey, 2014-2015**                                                                                                                                                     	|
|           	| Gershuny, J., Sullivan, O. (2017). United Kingdom Time Use Survey, 2014-2015. Centre for Time Use Research, University of Oxford. [data collection]. UK Data Service. SN: 8128, http://doi.org/10.5255/UKDA-SN-8128-1 	|
| 2000      	| Study Number **4504 - United Kingdom Time Use Survey, 2000**                                                                                                                                                          	|
|           	| Ipsos-RSL and ONS, United Kingdom Time Use Survey, 2000 [computer file]. 3rd Edition. Colchester, Essex: UK Data Archive [distributor], September 2003. SN: 4504.                                                     	|




---

Notes:
 1. **myData** folder employs `symlinks` directing to a centralised database where the the original repositories are stored.

```sh
myData
├── AuxiliaryData
│   ├── activities
│   └── allissue
├── UKDA-4504-stata8_se -> <symlink-to>/UK-TUS/UKDA-4504-stata8_se
├── UKDA-8128-stata11_se -> <symlink-to>/UK-TUS/UKDA-8128-stata11_se
└── UKDA-Core -> <symlink-to>/UK-TUS/UKDA-Core
```
