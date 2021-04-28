#' ----------------------------------------------------------------------------
#' TUS Converter                                                       {Setup}
#'
#' This loads required libraries, system variables,
#' and loads basic datasets, functions and global variables.
#'
#' ----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'

# Environment SETUP -----------------------------------------------------------

# (a) required libraries ----

.fnStartLibraries <- function(){
  message("Loading libraries")
  packageStartupMessage("initializing ...", appendLF = FALSE)

  # ... for loading and parsing data
  # library(renv)
  library(plyr)
  library(dplyr)
  library(Hmisc)
  library(foreign)
  library(pbapply)
  library(data.table)
  library(timeDate)
  library(lubridate)
  library(reshape2)
  library(ggplot2)
  library(gplots)
  library(treemap)

  packageStartupMessage(" done")
}

.fnStartLibraries()


# (b) auxiliary functions ----
source('myScripts/_aux_Environment.R')
source('myScripts/_aux_GetData.R')



# Initiate --------------------------------------------------------------------

path.TUS <- fnGetTUSProjectPath()
dir.create(path.TUS.out <- paste0(path.TUS,'/outputs'), showWarnings = F)

if(var.TUSyear %in% c(2000, 2015)) {

 if (var.TUSyear == 2000) {
  var.TUSversion <- "UKDA-4504-stata8_se"
  var.TUSfolder <- "stata8_se/"
 } else {
  var.TUSversion <- "UKDA-8128-stata11_se"
  var.TUSfolder <- "stata11_se/"
 }

 path.TUS_data <-
  paste(path.TUS, "myData", var.TUSversion, var.TUSfolder, sep = "/")

 tblTUSRawSubsets <- fnLoadRawData(path.TUS_data)

 source("myScripts/_get_Additional.R", verbose = FALSE)

 # .. identify unique households
 tblHhds <- unique(uktus15_diary_ep_long$serial)

} else {
 warning("Check version.")
}
