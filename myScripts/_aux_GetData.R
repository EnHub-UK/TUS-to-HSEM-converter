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

fnGetInParameters <- function(lstProfiles, dtaHhdsRef=tblHhds){

  fnGetGeneralProfile <- function(lstRef, lstData, lstDataSet){
    dtaExtract <- as.data.frame(t(lstData[[lstRef]][[lstDataSet]]))
    return(dtaExtract)
  }

  dtaHHd_i <- 1:length(lstProfiles)

  dtaHHd_gral <-
    pblapply(dtaHHd_i, fnGetGeneralProfile, lstProfiles, 'general')
  dtaHHd_gral <- ldply(dtaHHd_gral, data.frame)
  dtaHHd_gral$serial <- dtaHhdsRef[dtaHHd_i]
  rownames(dtaHHd_gral) <- NULL

  dtaHHd_ind <- subset(fnGetIndices(), serial %in% dtaHHd_gral$serial)
  dtaHHd_gral <- join(dtaHHd_ind, dtaHHd_gral, by='serial')
  dtaHHd_gral <- subset(dtaHHd_gral,
                        select = c(serial, .dwtype, .region, .tenure, .hhsize,
                                   NumAdult, NumChild, dhhtype, Repairs,
                                   IncCat, Income, Wages, NumRooms, DMSex_P1,
                                   WorkSta_P1, DVAge_P1, HiQual, SatisOv))
  colnames(dtaHHd_gral) <- tolower(colnames(dtaHHd_gral))

  dtaHHd_apps <-
    pblapply(dtaHHd_i, fnGetGeneralProfile, lstProfiles, 'appliances')
  dtaHHd_apps <- ldply(dtaHHd_apps, data.frame)
  dtaHHd_apps$serial <- dtaHhdsRef[dtaHHd_i]
  rownames(dtaHHd_apps) <- NULL

  lstHHd <- list(dtaHHd_gral, dtaHHd_apps)
  names(lstHHd) <- c('household.demographics', 'appliances.ownership')
  #
  return(lstHHd)
}

fnGetActionsPerSurveyed <- function(valActId, dtaHHd_toCheck){

  tblTime <- fnMakeTimeSlotTable()
  valActSelect <- tblActivities$Name[tblActivities$Id==valActId]
  print(valActSelect)
  dtaAct <- subset(dtaHHd_toCheck, whatdoing==valActSelect,
                   select = c('tid', 'whatdoing', 'DiaryDate_Act', 'DVAge'))
  if(dim(dtaAct)[1]<1){
    print("--> This activity was not performed")
    dtaAct <- NULL
  }else{
    colnames(dtaAct) <- c('tid','whatdoing','DiaryDate_Act','DVAge')
    dtaAct <- join(tblTime, dtaAct, by="tid")
    dtaAct$value <-
      ifelse(!is.na(dtaAct$whatdoing) & dtaAct$whatdoing==valActSelect, 1, 0)
    dtaAct <- dtaAct[!is.na(dtaAct$DVAge),]
    dtaAct <- dcast(dtaAct, tid ~ DiaryDate_Act + DVAge + value)
    dtaAct <- join(tblTime, dtaAct, by="tid")
    dtaAct[is.na(dtaAct)] <- 0
    dtaAct <- dtaAct[, !(names(dtaAct) %in% c('tid.from','tid.to','tid.slot'))]
    dtaAct <- melt(dtaAct, id = 1)
    print("--> Profiles Generated")
  }
  return(dtaAct)
}

fnGetActionsTable <- function(dtaAct, dtaFull=tblActivities){

  # [instant] is a dummy value employed to identify activities
  # that make their value consistent (e.g. sleep)
  dtaFull$Instant <- sample(c(0,1), dim(dtaFull)[1], replace = TRUE)

  # data from chosen household is mined for existing activities
  tblSvy <- as.data.frame(table(dtaAct$whatdoing))
  tblSvy <- tblSvy[tblSvy$Freq>0,]
  colnames(tblSvy) <- c('Name', 'Freq')

  tblSvy <- join(tblSvy, tblActivities, by='Name')
  tblSvy <- tblSvy[order(tblSvy$Freq),]
  rownames(tblSvy) <- NULL

  tblSvy <- join(tblSvy, dtaFull, by=c("Id","Name"))

  return(tblSvy)
}

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
  varYr <- 2015
  valStep <- "10min"
  tblTime <- data.frame(tid=1:144)
  tblTime$slotA <-
    timeSequence(from = paste(varYr,"-01-01 04:00:00",sep=""),
                 to = paste(varYr,"-01-02 03:59:00",sep=""),
                 by= valStep)
  tblTime$slotB <-
    timeSequence(from = paste(varYr,"-01-01 04:10:00",sep=""),
                 to = paste(varYr,"-01-02 04:09:00",sep=""),
                 by= valStep)
  tblTime$hourA <- strftime(tblTime$slotA, format="%H:%M")
  tblTime$hourB <- strftime(tblTime$slotB, format="%H:%M")
  tblTime$tid.slot <- paste(tblTime$hourA,tblTime$hourB,sep="-")
  tblTime <- tblTime[,c('tid','hourA','hourB','tid.slot')]
  colnames(tblTime) <- c('tid','tid.from','tid.to','tid.slot')
  return(tblTime)
}

fnMakeTimeSlot <- function(varStep){
  if(varStep=="1hour" | varStep=="60min"){
    tblTime <- seq(0, 23.99, by=1)
  }else if(varStep=="10min"){
    tblTime <- trunc(seq(0, 239.9, by=10/6))/10
  }else if(varStep=="30min"){
    tblTime <- seq(0, 23.99, by=0.5)
  }else{
    tblTime <- seq(0, 23.99, by=1)
    message("Please check requested time-slot... 1 hour assigned")
  }
  return(tblTime)
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

fnExtractInfo <- function(varNo, dtaToCheck, varHhld, tblHhdsRef=tblHhds){

  dtaExt <- subset(dtaToCheck, serial==tblHhdsRef[varHhld])
  dtaExt <- as.data.frame(t(dtaExt[,varNo]))
  colnames(dtaExt) <- tblHhdsRef[varHhld]

  return(dtaExt)
}

fnExtractProfile <- function(valHhd, tblHhdRef, tblHhd=uktus15_household,
  tblInd=uktus15_individual, tblDay=uktus15_diary_ep_long){

  #.. Identify variable numbers (see documentation):
  #   `myData/AuxiliaryData/allissue/uktus15_household_ukda_data_dictionary`
  #'@note replace with regex

  valHhd_hhp <- list(general = c(23,13,14,64,73,72,65,36,206,216,226),
                     appliances = 37:53,
                     status = 206:235,
                     relationships = 236:335)
  valHhd_ppp <- c(321,463,599,588,594)

  #.. Extract variable information

  dtaHhd_hhp <-
    pblapply(valHhd_hhp, fnExtractInfo, tblHhd, valHhd)

  dtaHhd_hh_composition <- as.data.frame(t(
    subset(tblInd,
           serial==tblHhdRef[valHhd] & pnum==1,
           select=colnames(tblInd)[valHhd_ppp])))
  colnames(dtaHhd_hh_composition) <- colnames(dtaHhd_hhp[['general']])

  #.. Perform additional edits to the retrieved information

  dtaHhd_hhp[['general']] <-
    rbind(dtaHhd_hhp[['general']], dtaHhd_hh_composition)

  dtaHhd_hhp$status <-
    as.data.frame(matrix(as.character(dtaHhd_hhp$status[,1]),
                         nrow = 10, ncol = 3, byrow = FALSE,
                         dimnames = list(1:10,c('gender','employment','age'))))
  dtaHhd_hhp$status <- as_tibble(
    dtaHhd_hhp$status[complete.cases(dtaHhd_hhp$status),])
  dtaHhd_hhp$status$age <-
    as.numeric(as.character(dtaHhd_hhp$status$age))

  dtaHhd_hhp$relationships <-
    matrix(apply(as.data.frame(t(dtaHhd_hhp$relationships)), 2,
                 FUN=fnGetRelationship, F),
           nrow = 10, ncol = 10, byrow = TRUE,
           dimnames = list(1:10,1:10))

  dtaHhd_hhp[['dailyacts']] <- as_tibble(
    subset(tblDay, serial==tblHhdRef[valHhd],
           select = c('pnum','tid','whatdoing',
                      'DiaryDate_Act','DVAge','WhereWhen',
                      'What_Oth1','What_Oth2','What_Oth3')))

  #.. Return household profile as a list
  return(dtaHhd_hhp)
}

fnGetIndices <- function(dtaTUS=uktus15_individual, path=path.TUS.out){

  dtaSel <- subset(dtaTUS,
    select = c(serial, NumAdult, NumChild, dtenure, dgorpaf, Accom))
  dtaSel <- dtaSel[!duplicated(dtaSel),]

  dtaSel$.dwtype <-
    ifelse(dtaSel$Accom=="House or bungalow","House",
    ifelse(dtaSel$Accom=="Flat or maisonette", "Apartment", NA))
  dtaSel$.region <-
    ifelse(dtaSel$dgorpaf=="North West (incl merseyside)","North West",
    ifelse(dtaSel$dgorpaf=="Yorkshire & Humberside","Yorkshire and the Humber",
    ifelse(dtaSel$dgorpaf=="East of England","Eastern England",
      as.character(dtaSel$dgorpaf))))
  dtaSel$.tenure <-
    ifelse(dtaSel$dtenure=="Owns outright" |
      dtaSel$dtenure=="Own with a mortgage","Private",
    ifelse(dtaSel$dtenure=="Rented (private) / rent free" |
      dtaSel$dtenure=="Rented (public)","Rented", NA))
  dtaSel$.hhsize <-
    paste(ifelse(dtaSel$NumAdult>4,'X',dtaSel$NumAdult),
    ifelse(dtaSel$NumChild>3,'more',dtaSel$NumChild), sep=".")

  dtaSel <- dtaSel[!is.na(dtaSel$.dwtype)|!is.na(dtaSel$.tenure),]
  dtaSel <-
    subset(dtaSel, select = c(serial,.dwtype,.region,.tenure,.hhsize))
  dtaSel <- as_tibble(dtaSel)

  file.out <- paste0(path, "/tbl_TUS_index.csv")
  write.csv(dtaSel, file.out)

  return(dtaSel)
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

  assign(x = 'dtaHhd_hh_profiles',
         value = uktus15_profiles, envir = .GlobalEnv)
  assign(x = 'uktus15_individual',
         value = uktus15_individual, envir = .GlobalEnv)
  assign(x = 'uktus15_household',
         value = uktus15_household, envir = .GlobalEnv)
  assign(x = 'uktus15_diary_ep_long',
         value = uktus15_diary_ep_long, envir = .GlobalEnv)
}
