#' -----------------------------------------------------------------------------
#' TUS Converter                                                 {Data Parser}
#'
#' This contains auxiliary functions to load raw data.
#' 
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
    perpos <- which(strsplit(file, "")[[1]]==".")
    prepos <- which(strsplit(file, "")[[1]]=="/")
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

findStrings <- function(varToFind){

  path.TUS_dict <- paste(path.TUS, "myData/AuxiliaryData/allissue/", sep="/")
  setwd(path.TUS_dict)

  if (fnGetOS() == "mac" | fnGetOS() == "unix") {
    exename <- "grep -rl ";
    command <- paste(exename,"'",varToFind,"' *", sep="");
  } else {
    exename <- "dir /B | findstr /R /M ";
    command <- paste(exename,'"',varToFind,'" *.*', sep="");
  }

  rm(exename);
  t1 <- try(system(command, wait = F, intern = T))
  t1 <- gsub("_ukda_data_dictionary", "", t1, perl=TRUE, ignore.case = FALSE)

  setwd(path.TUS)
  return(t1)
}

fnExtractInfo <- function(varNumbers, dtaToCheck, varHhld, tblHhdsRef=tblHhds){

  dtaExtraction <- subset(dtaToCheck, serial==tblHhdsRef[varHhld])
  dtaExtraction <- as.data.frame(t(dtaExtraction[,varNumbers]))
  colnames(dtaExtraction) <- tblHhdsRef[varHhld]

  return(dtaExtraction)
}

fnExtractProfile <- function(valHhd, tblHhdRef, tblHhd=uktus15_household,
  tblInd=uktus15_individual, tblDay=uktus15_diary_ep_long){

  #.. Identify variable numbers (see documentation):
  #   `myData/AuxiliaryData/allissue/uktus15_household_ukda_data_dictionary`

  valHhd_hh_profiles <- list(general=c(23,13,14,64,73,72,65,36,206,216,226),
                             appliances=37:53,
                             status=206:235,
                             relationships=236:335)
  valHhd_ind_profiles <- c(321,463,599,588,594)

  #.. Extract variable information

  dtaHhd_hh_profiles <-
    pblapply(valHhd_hh_profiles, fnExtractInfo, tblHhd, valHhd)

  dtaHhd_hh_composition <- as.data.frame(t(
    subset(tblInd,
           serial==tblHhdRef[valHhd] & pnum==1,
           select=colnames(tblInd)[valHhd_ind_profiles])))
  colnames(dtaHhd_hh_composition) <- colnames(dtaHhd_hh_profiles[['general']])

  #.. Perform additional edits to the retrieved information

  dtaHhd_hh_profiles[['general']] <-
    rbind(dtaHhd_hh_profiles[['general']], dtaHhd_hh_composition)

  dtaHhd_hh_profiles$status <-
    as.data.frame(matrix(as.character(dtaHhd_hh_profiles$status[,1]),
                         nrow = 10, ncol = 3, byrow = FALSE,
                         dimnames = list(1:10,c('gender','employment','age'))))
  dtaHhd_hh_profiles$status <- as_tibble(
    dtaHhd_hh_profiles$status[complete.cases(dtaHhd_hh_profiles$status),])
  dtaHhd_hh_profiles$status$age <-
    as.numeric(as.character(dtaHhd_hh_profiles$status$age))

  dtaHhd_hh_profiles$relationships <-
    matrix(apply(as.data.frame(t(dtaHhd_hh_profiles$relationships)), 2,
                 FUN=fnGetRelationship, F),
           nrow = 10, ncol = 10, byrow = TRUE,
           dimnames = list(1:10,1:10))

  dtaHhd_hh_profiles[['dailyacts']] <- as_tibble(
    subset(tblDay, serial==tblHhdRef[valHhd],
           select = c('pnum','tid','whatdoing',
                      'DiaryDate_Act','DVAge','WhereWhen',
                      'What_Oth1','What_Oth2','What_Oth3')))

  #.. Return household profile as a list
  return(dtaHhd_hh_profiles)
}

fnGetIndices <- function(dtaTUS=uktus15_individual, path=path.TUS.out){

  dta.selection <-
    subset(dtaTUS,
           select = c(serial, NumAdult, NumChild, dtenure, dgorpaf, Accom))
  dta.selection <- dta.selection[!duplicated(dta.selection),]

  dta.selection$.dwtype <-
    ifelse(dta.selection$Accom=="House or bungalow","House",
           ifelse(dta.selection$Accom=="Flat or maisonette", "Apartment", NA))
  dta.selection$.region <-
    ifelse(dta.selection$dgorpaf=="North West (incl merseyside)","North West",
           ifelse(dta.selection$dgorpaf=="Yorkshire & Humberside","Yorkshire and the Humber",
                  ifelse(dta.selection$dgorpaf=="East of England","Eastern England", as.character(dta.selection$dgorpaf))))
  dta.selection$.tenure <-
    ifelse(dta.selection$dtenure=="Owns outright"|dta.selection$dtenure=="Own with a mortgage","Private",
           ifelse(dta.selection$dtenure=="Rented (private) / rent free"|dta.selection$dtenure=="Rented (public)","Rented", NA))
  dta.selection$.hhsize <-
    paste(ifelse(dta.selection$NumAdult>4,'X',dta.selection$NumAdult), ifelse(dta.selection$NumChild>3,'more',dta.selection$NumChild), sep=".")

  dta.selection <- dta.selection[!is.na(dta.selection$.dwtype)|!is.na(dta.selection$.tenure),]
  dta.selection <-
    subset(dta.selection, select = c(serial,.dwtype,.region,.tenure,.hhsize))
  dta.selection <- as_tibble(dta.selection)

  file.out <- paste0(path, "/tbl_TUS_index.csv")
  write.csv(dta.selection, file.out)

  return(dta.selection)
}

fnGetInParameters <- function(lstProfiles, dtaHhdsRef=tblHhds){

  fnGetGeneralProfile <- function(lstRef, lstData, lstDataSet){
    dtaExtract <- as.data.frame(t(lstData[[lstRef]][[lstDataSet]]))
    return(dtaExtract)
  }

  dtaHhd_hh_i <- 1:length(lstProfiles)

  dtaHhd_hh_gral <- pblapply(dtaHhd_hh_i,
                             fnGetGeneralProfile, lstProfiles, 'general')
  dtaHhd_hh_gral <- ldply(dtaHhd_hh_gral, data.frame)
  dtaHhd_hh_gral$serial <- dtaHhdsRef[dtaHhd_hh_i]
  rownames(dtaHhd_hh_gral) <- NULL

  dta.hhd <- subset(fnGetIndices(),  serial %in% dtaHhd_hh_gral$serial)
  dtaHhd_hh_gral <- join(dta.hhd, dtaHhd_hh_gral, by='serial')
  dtaHhd_hh_gral <-
    subset(dtaHhd_hh_gral,
           select = c(serial, .dwtype, .region, .tenure, .hhsize,
                      NumAdult, NumChild, dhhtype, Repairs, IncCat,
                      Income, Wages, NumRooms, DMSex_P1, WorkSta_P1,
                      DVAge_P1, HiQual, SatisOv))
  colnames(dtaHhd_hh_gral) <- tolower(colnames(dtaHhd_hh_gral))


  dtaHhd_hh_apps <- pblapply(dtaHhd_hh_i,
                             fnGetGeneralProfile, lstProfiles, 'appliances')
  dtaHhd_hh_apps <- ldply(dtaHhd_hh_apps, data.frame)
  dtaHhd_hh_apps$serial <- dtaHhdsRef[dtaHhd_hh_i]
  rownames(dtaHhd_hh_apps) <- NULL

  lstHhd_hh <- list(dtaHhd_hh_gral, dtaHhd_hh_apps)
  names(lstHhd_hh) <- c('household.demographics', 'appliances.ownership')
  #
  return(lstHhd_hh)
}

fnLoadTUSProcessed <- function(path=path.TUS.out){

  lblTables <- c('-household','-individual','-profiles','-diary')
  (varFiles <- list.files(path, pattern = ".rds"))
  varFiles <- as.numeric(sapply(lblTables, function(x) grep(x,varFiles)))

  if(any(is.na(varFiles))){
    #.. generation ----------------
    #   ~ 4 mins in 3.4 GHz i5, 16 GB 2400 MHz
    uktus15_profiles <- pblapply(1:length(tblHhds), fnExtractProfile, tblHhds)
    names(uktus15_profiles) <- tblHhds[1:length(tblHhds)]
    saveRDS(uktus15_profiles, paste0(path.TUS.out, file="/TUS-profiles.rds"))
    saveRDS(uktus15_individual, paste0(path.TUS.out, file="/TUS-individual.rds"))
    saveRDS(uktus15_household, paste0(path.TUS.out, file="/TUS-household.rds"))
    saveRDS(uktus15_diary_ep_long, paste0(path.TUS.out, file="/TUS-diary.rds"))
  }else{
    #.. retrieving ----------------
    uktus15_profiles <-
      readRDS(file = paste0(path.TUS.out, file="/TUS-profiles.rds"))
    uktus15_individual <-
      readRDS(file = paste0(path.TUS.out, file="/TUS-individual.rds"))
    uktus15_household <-
      readRDS(file = paste0(path.TUS.out, file="/TUS-household.rds"))
    uktus15_diary_ep_long <-
      readRDS(file = paste0(path.TUS.out, file="/TUS-diary.rds"))
  }

  assign(x = 'dtaHhd_hh_profiles', value = uktus15_profiles, envir = .GlobalEnv)
  assign(x = 'uktus15_individual', value = uktus15_individual, envir = .GlobalEnv)
  assign(x = 'uktus15_household', value = uktus15_household, envir = .GlobalEnv)
  assign(x = 'uktus15_diary_ep_long', value = uktus15_diary_ep_long, envir = .GlobalEnv)
}
