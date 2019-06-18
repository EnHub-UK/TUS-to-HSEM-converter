#' -----------------------------------------------------------------------------
#' TUS Converter                                                 {Data Parser}
#'
#' @file `_aux_GetData.R` contains auxiliary functions to load raw data.
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'

# structure of stata folders
# --------------------------
# .
# ├── 4504_file_information.rtf
# ├── mrdoc
# │   ├── UKDA
# │   │   └── UKDA_Study_4504_Information.htm
# │   ├── allissue
# │   │   ├── Individual_data_5_UKDA_Data_Dictionary.rtf
# │   │   ├── diary_data_8_UKDA_Data_Dictionary.rtf
# │   │   ├── diary_data_8_episode_UKDA_Data_Dictionary.rtf
# │   │   ├── hhld_data_6_UKDA_Data_Dictionary.rtf
# │   │   ├── weight_diary_person_UKDA_Data_Dictionary.rtf
# │   │   └── worksheet_data_3_UKDA_Data_Dictionary.rtf
# │   ├── pdf
# │   │   ├── 4504userguide1.pdf
# │   │   ├── 4504userguide2.pdf
# │   │   └── in4504.pdf
# │   └── stataissue
# │       └── 4504_SPSS_to_STATA_conversion.rtf
# ├── read4504.txt
# └── stata8_se
# ├── Individual_data_5.dta
# ├── diary_data_8.dta
# ├── diary_data_8_episode.dta
# ├── hhld_data_6.dta
# ├── weight_diary_person.dta
# └── worksheet_data_3.dta
#
# .
# └── UKDA-8128-stata11_se
# ├── 8128_file_information.rtf
# ├── mrdoc
# │   ├── UKDA
# │   │   └── UKDA_Study_8128_Information.htm
# │   ├── allissue
# │   │   ├── uktus15_diary_ep_long_ukda_data_dictionary.rtf
# │   │   ├── uktus15_diary_wide_ukda_data_dictionary.rtf
# │   │   ├── uktus15_dv_time_vars_ukda_data_dictionary.rtf
# │   │   ├── uktus15_household_ukda_data_dictionary.rtf
# │   │   ├── uktus15_individual_ukda_data_dictionary.rtf
# │   │   └── uktus15_wksched_ukda_data_dictionary.rtf
# │   └── pdf
# │       ├── 8128_ctur_report.pdf
# │       └── 8128_natcen_reports.pdf
# ├── read8128.htm
# └── stata11_se
# ├── uktus15_diary_ep_long.dta
# ├── uktus15_diary_wide.dta
# ├── uktus15_dv_time_vars.dta
# ├── uktus15_household.dta
# ├── uktus15_individual.dta
# └── uktus15_wksched.dta
#

fnLoadTUSRawFiles <- function(){

  files <- list.files(pattern="dta", recursive=TRUE)
  files <- files[!is.na(files)]

  fnLoadTUSRawFiles <- function(file){
    perpos <- which(strsplit(file, "")[[1]]==".")   # find the '.*' in file
    prepos <- which(strsplit(file, "")[[1]]=="/")   # find the '.*' in file
    prepos <- ifelse(is.integer(prepos) && length(prepos) == 0L, 0, prepos)
    assign(gsub(" ","", substr(file, prepos + 1, perpos - 1)),
           read.dta(file, warn.missing.labels=F),
           envir = globalenv())
    return(substr(file, prepos + 1, perpos - 1))
  }

  varTUSRawSub <- pblapply(files, fnLoadTUSRawFiles)
  varTUSRawSub <- unlist(varTUSRawSub)
  return(varTUSRawSub)
}

fnLoadRawData <- function(pat.RawData){
  setwd(pat.RawData)
  tblTUSRawSubsets <- fnLoadTUSRawFiles()
  setwd(path.TUS)
  return(tblTUSRawSubsets)
}

fnGetActivity <- function(x, dtaRef=tblActivities){
  valId <- dtaRef$Id[dtaRef$Name==x]
  return(valId)
}

fnGetActivityGroup <- function(dtaRef=tblActivitiesRef){
  dtaRef <- dtaRef[!is.na(dtaRef$Name),]
  dtaRef$Name <- gsub("-",".",dtaRef$Name)
  rownames(dtaRef) <- NULL
  return(dtaRef)
}

fnGetRelationship <- function(x, vrbFac=F, dtaRef=tblRelationships){

  x <- as.character(x)
  x <- ifelse(is.na(x), "Item not applicable", x)
  x <- ifelse(x=="0", "Item not applicable", x)

  valId <- dtaRef$Id[dtaRef$Name==x]

  if(vrbFac==TRUE){
    valId <- factor(valId, levels=dtaRef$Id, labels=dtaRef$Name)
  }

  return(valId)
}

fnMakeTimeSlotTable <- function(){
  valYearRef <- 2015
  valDairyTimeSlot <- "10min"
  dtaTimeSlot <- data.frame(tid=1:144)
  dtaTimeSlot$slotA <-
    timeSequence(from = paste(valYearRef,"-01-01 04:00:00",sep=""),
                 to = paste(valYearRef,"-01-02 03:59:00",sep=""),
                 by= valDairyTimeSlot)
  dtaTimeSlot$slotB <-
    timeSequence(from = paste(valYearRef,"-01-01 04:10:00",sep=""),
                 to = paste(valYearRef,"-01-02 04:09:00",sep=""),
                 by= valDairyTimeSlot)
  dtaTimeSlot$hourA <- strftime(dtaTimeSlot$slotA, format="%H:%M")
  dtaTimeSlot$hourB <- strftime(dtaTimeSlot$slotB, format="%H:%M")
  dtaTimeSlot$tid.slot <- paste(dtaTimeSlot$hourA,dtaTimeSlot$hourB,sep="-")
  dtaTimeSlot <- dtaTimeSlot[,c('tid','hourA','hourB','tid.slot')]
  colnames(dtaTimeSlot) <- c('tid','tid.from','tid.to','tid.slot')
  return(dtaTimeSlot)
}

fnMakeTimeSlot <- function(varTimeSlt){
  if(varTimeSlt=="1hour" | varTimeSlt=="60min"){
    tblTimeSlot <- seq(0, 23.99, by=1)
  }else if(varTimeSlt=="10min"){
    tblTimeSlot <- trunc(seq(0, 239.9, by=10/6))/10
  }else if(varTimeSlt=="30min"){
    tblTimeSlot <- seq(0, 23.99, by=0.5)
  }else{
    tblTimeSlot <- seq(0, 23.99, by=1)
    message("Please check requested time-slot... 1 hour assigned")
  }
  return(tblTimeSlot)
}
