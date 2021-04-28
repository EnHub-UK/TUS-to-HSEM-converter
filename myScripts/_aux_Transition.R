#' -----------------------------------------------------------------------------
#' TUS Converter                                                 {Data Parser}
#'
#' This contains auxiliary functions to ontain transitions
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'

fnGetActivitiesbySubset <- function(lstSubset, varPick, tblSubset,
  dtaInd=uktus15_individual, dtaDiary=uktus15_diary_ep_long, varExtra=FALSE){

    idPick <- as.character(tblSubset$Id[lstSubset])
    idPick.name <- as.character(tblSubset$Name[tblSubset$Id %in% idPick])

    dtaPick <- dtaInd[,varPick]
    dtaPick <- dtaPick %in% idPick.name
    dtaPick <- subset(dtaInd, dtaPick, select=serial)
    dtaPick <- unique(as.integer(unlist(dtaPick)))
    dtaPickSub <- NULL

    # Cases where no diary was collected are excluded here
    tblSubset <- dtaDiary[dtaDiary$serial %in% dtaPick, ]
    if(dim(tblSubset)[1]<1) print("Warning: No Data in subset")

    # add condition here to make subset by age categories
    if(varExtra==TRUE){
      tblSubset$ExtraFactor <-
        factor(ifelse(tblSubset$DVAge<17, ifelse(tblSubset$DVAge<10, 1, 2), 3),
               levels = 1:3, labels = tblExtraFactors)
      lstSubset <- list()
      for(i in 1:3){
        lstSubset[[i]] <- subset(tblSubset, ExtraFactor==tblExtraFactors[i])
        lstSubset[[i]]$ExtraFactor <- NULL
      }
      names(lstSubset) <- tblExtraFactors
      tblSubset <- lstSubset
    }

    return(tblSubset)
  }

fnGetTransitionTables <- function(dtaSource, varSlot="1hour",
                                  varOut=T, tblActs=tblActivities){

  fnMakeandSummariseSubset <- function(dtaTo, tblAcssFull, tblSlots,
                                       varCtrlA, varCtrlB){

    dtaTo <- subset(dtaTo, WhereWhen=="Home",
                    select=c('serial','pnum','tid','whatdoing',
                             'DiaryDate_Act','DVAge'))
    dtaTo.cols <- colnames(dtaTo)

    dtaTo <- suppressWarnings(
      left_join(dtaTo, tblAcssFull, by = c("whatdoing" = "Name")))
    dtaTo <- suppressWarnings(
      left_join(dtaTo, tblSlots, by = c("tid" = "tid.slot")))
    dtaTo <- dtaTo[,c(dtaTo.cols,"Group","tid.from")]

    dtaTo$Group <- gsub("-",".",dtaTo$Group)

    dtaTo$tid.from <- strtoi(
      as.difftime(dtaTo$tid.from, format = "%H:%M", units = "mins")) / varCtrlB
    dtaTo$tid.from <- floor(dtaTo$tid.from) / varCtrlA

    dtaTo <- dtaTo[order(dtaTo$serial, dtaTo$pnum, dtaTo$tid.from),]
    rownames(dtaTo) <- NULL

    dtaTo <- ddply(dtaTo, .(tid.from, Group), c("nrow"))
    dtaTo <- dtaTo[!is.na(as.character(dtaTo$Group)),]
    dtaTo <- dtaTo[!is.na(dtaTo$nrow),]

    return(dtaTo)
  }

  fnConvertTableToMatrix <- function(dtaTable){
    dtaTable <- dcast(dtaTable, tid ~ variable)
    rownames(dtaTable) <- dtaTable$tid
    dtaTable$tid <- NULL
    return(dtaTable)
  }

  fnValidateData <- function(dtaToValidate){

    fnImputeMatrix <- function(dtaTidGaps, dtaMaxVal){

      dtaToCheck <- tblTimeValid
      valRowActive <-
        as.integer(rownames(dtaToCheck[dtaToCheck$tid==dtaTidGaps,]))
      dtaValid <- NULL

      if(dtaTidGaps==0.0){
        dtaValid <- dtaMaxVal
      }else{
        dtaValid <- dtaToCheck[valRowActive-1,2:11]
      }
      dtaToCheck[valRowActive, 2:11] <- dtaValid
      assign("tblTimeValid", dtaToCheck, inherits=TRUE)
      rownames(dtaValid) <- NULL
      return(dtaValid)
    }

    tblActNames <- fnGetActivityGroup()

    tblTimeValid <- tblTimeValidd <-
      dcast(dtaToValidate, tid ~ variable, value.var="value")
    colnames(tblTimeValid) <- gsub("-",".", colnames(tblTimeValid))
    colnames(tblTimeValidd) <- gsub("-",".", colnames(tblTimeValidd))
    tblTimeValid.cols <- colnames(tblTimeValid)

    tblTimeValid.Ref <-
      as.data.frame(matrix(NA, nrow = dim(tblTimeValid)[1],
                           ncol = 1 + dim(tblActNames)[1]))
    colnames(tblTimeValid.Ref) <- c('tid', tblActNames$Name)
    tblTimeValid.Ref[,tblTimeValid.cols] <- tblTimeValid
    tblTimeValid <- tblTimeValid.Ref
    rm(tblTimeValid.Ref)

    tblTimeValid$tid <- as.numeric(as.character(tblTimeValid$tid))
    tblTimeValid[is.na(tblTimeValid)] <- 0
    tblTimeValid$events <- rowSums(tblTimeValid[,2:11], na.rm = TRUE)
    tblTimeValid$events <- ifelse(tblTimeValid$events>0, T,F)
    tblTimeValid$occurence <- tblTimeValid$events * tblTimeValid$tid
    valMaxEvent <- max(tblTimeValid$occurence)
    dtaMaxEvent <-
      tblTimeValid[tblTimeValid$occurence==valMaxEvent,2:11]
    dtaTimeGaps <-
      tblTimeValid$tid[tblTimeValid$events==FALSE]

    if(length(dtaTimeGaps)>0){
      dtaImputated <-
        ldply(lapply(dtaTimeGaps, fnImputeMatrix, dtaMaxEvent), data.frame)
      dtaImputated$tid <- dtaTimeGaps
      tblTimeValidd$tid <- as.numeric(as.character(tblTimeValidd$tid))
      tblTimeValidd <- tblTimeValidd[,tblTimeValid.cols]
      dtaImputated <- dtaImputated[,tblTimeValid.cols]
      tblTimeValidd[tblTimeValidd$tid%in%dtaTimeGaps,] <- dtaImputated
    }else{
      tblTimeValidd <- tblTimeValid
    }
    rm(tblTimeValid)

    tblTimeValidd <- melt(tblTimeValidd, id=1)
    tblTimeValidd$tid <- as.numeric(tblTimeValidd$tid)

    return(tblTimeValidd)
  }

  fnReScaleData <- function(dtaTo){
    dtaTo <- dcast(dtaTo, tid ~ variable)

    dtaTo.tid <- dtaTo$tid
    dtaTo$tid <- dtaTo$events <- dtaTo$occurence <- NULL

    dtaTo.cols <- colnames(dtaTo)
    dtaTo$tot <- rowSums(dtaTo)
    dtaTo <- dtaTo / dtaTo$tot
    dtaTo$tot <- NULL
    dtaTo$tid <- dtaTo.tid
    dtaTo <- dtaTo[,c('tid',dtaTo.cols)]
    dtaTo <- melt(dtaTo, id=1)

    return(dtaTo)
  }

  fnGetTableOccurrence <- function(dta.wTime, dtw.wValues){

    dta.wActs <- fnGetActivityGroup()

    dtaStd <- expand.grid(
      tid.from=as.character(unique(dta.wTime$tid.from.num)),
      Group=as.character(unique(dta.wActs$Name)))

    dtaStd$tid.from <- as.numeric(as.character(dtaStd$tid.from))
    dtaStd <- dtaStd[order(dtaStd$tid.from),]
    dtaStd$Group <- as.character(dtaStd$Group)

    dtaStd <- join(dtaStd, dtw.wValues, by=c('tid.from', 'Group'))
    colnames(dtaStd) <- c('tid','variable','value')
    dtaStd$value[is.na(dtaStd$value)] <- 0
    dtaStd <- fnValidateData(dtaStd)
    return(dtaStd)
  }

  fnGetTableProportion <- function(dtw.wValues){

    dta.wActs <- fnGetActivityGroup()

    dtaEvents <- dcast(dtw.wValues, tid ~ variable, value.var="value")
    dtaEvents.rows <- dtaEvents$tid
    rownames(dtaEvents) <- dtaEvents$tid
    dtaEvents <- dtaEvents[,-c(1)]

    dtaEvents <-
      apply(dtaEvents, 2, function(x) x / rowSums(dtaEvents, na.rm = T))
    dtaEvents <- as.data.frame(dtaEvents)
    dtaEvents$tid <- rownames(dtaEvents)
    dtaEvents <- melt(dtaEvents, id='tid')
    dtaEvents$tid <- as.numeric(dtaEvents$tid)
    dtaEvents <- dtaEvents[!is.na(dtaEvents$value),]

    dtaSubsetStd <-
      expand.grid(tid=as.character(unique(dtaEvents$tid)),
                  variable=as.character(unique(dta.wActs$Name)))
    dtaSubsetStd <- join(dtaSubsetStd, dtaEvents, by=c('tid', 'variable'))
    dtaSubsetStd$value[is.na(dtaSubsetStd$value)] <- 0
    dtaSubsetStd <- fnReScaleData(dtaSubsetStd)

    return(dtaSubsetStd)
  }

  fnGetTableProbable <- function(dta.wEvents, dtw.wValues){

    dta.wActs <- fnGetActivityGroup()

    dtaLog <- dcast(dta.wEvents, tid ~ variable, value.var="value")
    dtaLog.rows <- dtaLog$tid
    rownames(dtaLog) <- dtaLog$tid
    dtaLog <- dtaLog[,-c(1)]

    dtaLogA <- dtaLogB <- as.matrix(dtaLog)
    dtaLogA <- apply(dtaLogA, 2, function(x) x / rowSums(dtaLogA, na.rm = T))
    dtaLogA <- dtaLogA[c(dim(dtaLogA)[1],1:dim(dtaLogA)[1]-1),]

    dtaLogB <- t(apply(dtaLogB, 1, function(x) x / colSums(dtaLogB, na.rm = T)))

    dtaLogA[is.na(dtaLogA)] <- 0
    dtaLogB[is.na(dtaLogB)] <- 0

    dtaLog <- dtaLogA * dtaLogB

    dtaLog <- apply(dtaLog, 2, function(x) x / rowSums(dtaLog, na.rm = T))

    dtaLog <- as.data.frame(dtaLog)
    dtaLog$tid <- rownames(dtaLog)
    dtaLog <- melt(dtaLog, id='tid')
    dtaLog$tid <- as.numeric(dtaLog$tid)
    dtaLog <- dtaLog[!is.na(dtaLog$value),]

    dtaSub <- expand.grid(
      tid=as.character(unique(dta.wEvents$tid)),
      variable=as.character(unique(dta.wActs$Name)))
    dtaSub <- join(dtaSub, dtaLog, by=c('tid', 'variable'))
    dtaSub$value[is.na(dtaSub$value)] <- 0

    return(dtaSub)
  }

  fnCheckSteadiness <- function(tblCheck){

    tblLabels <- rownames(tblCheck)

    tblZeros <- rowSums(tblCheck, na.rm = T)
    tblCheckRoll <- rbind(tblCheck,tblCheck,tblCheck)
    tblZeros <- c(tblZeros,tblZeros,tblZeros)
    rownames(tblCheckRoll) <- NULL

    varSeqRows <- (dim(tblCheck)[1]+1):(dim(tblCheck)[1]*2)
    for(i in varSeqRows){
      if(tblZeros[i]==0){
        tblCheckRoll[i,] <- tblCheckRoll[i-1,]
      }
    }

    tblCheck <- tblCheckRoll[varSeqRows,]
    rownames(tblCheck) <- tblLabels

    return(tblCheck)
  }


  tblActNames <- fnGetActivityGroup()  # table of employed groups
  lstOut <- list()                     # list of processed tables and matrices


  # adjust factors to scale time-slot resolution
  if(varSlot=="1hour" | varSlot=="60min"){
    adj.a <- 1
    adj.b <- 60
  }else if(varSlot=="10min"){
    adj.a <- 10
    adj.b <- 6
  }else if(varSlot=="30min"){
    adj.a <- 2
    adj.b <- 30
  }else{
    adj.a <- 1
    adj.b <- 60
    message("Please check requested time-slot... 1 hour assigned")
  }

  # load reference table for time slots, and adjust requested resolution
  tblTimeSlot <- fnMakeTimeSlotTable()
  tblTimeSlot.cols <- colnames(tblTimeSlot)
  tblTimeSlot$tid.from.num <-
    strtoi(as.difftime(tblTimeSlot$tid.from, format = "%H:%M",
                       units = "mins")) /  adj.b
  tblTimeSlot$tid.from.num <- floor(tblTimeSlot$tid.from.num) / adj.a


  # process data based on given subset
  dtaSubset <-
    fnMakeandSummariseSubset(dtaSource, tblActs, tblTimeSlot, adj.a, adj.b)

  # subset data by considering inside conditions only
  dtaSubset_inOnly <- dtaSubset[!dtaSubset$Group=='out',]


  if(dim(dtaSubset)[1]>1){

    # prepare data to display number of occurrences
    lstOut[[1]] <- fnGetTableOccurrence(tblTimeSlot, dtaSubset)

    # prepare data to display proportion of occurrences
    lstOut[[2]] <- fnGetTableProportion(lstOut[[1]])

    # prepare data to display probability of occurrences based on previous state
    lstOut[[3]] <- fnGetTableProbable(lstOut[[2]], dtaSubset)

    # convert tables to matrices
    lstOut[[4]] <- fnConvertTableToMatrix(lstOut[[2]])
    lstOut[[5]] <- fnConvertTableToMatrix(lstOut[[3]])
    lstOut[[5]] <- fnCheckSteadiness(lstOut[[5]])

    tblTransition <- lstOut[[5]]
    tblTransition$tid <- row.names(tblTransition)
    tblTransition <- melt(tblTransition, id.vars = 'tid')
    lstOut[[3]] <- tblTransition

    # indoor subset only

    # prepare data to display number of occurrences
    lstOut[[6]] <- fnGetTableOccurrence(tblTimeSlot, dtaSubset_inOnly)

    # prepare data to display proportion of occurrences
    lstOut[[7]] <- fnGetTableProportion(lstOut[[6]])

    # prepare data to display probability of occurrences based on previous state
    lstOut[[8]] <- fnGetTableProbable(lstOut[[7]], dtaSubset_inOnly)

    # convert tables to matrices
    lstOut[[9]] <- fnConvertTableToMatrix(lstOut[[7]])
    lstOut[[10]] <- fnConvertTableToMatrix(lstOut[[8]])
    lstOut[[10]] <- fnCheckSteadiness(lstOut[[10]])

    tblTransition <- lstOut[[10]]
    tblTransition$tid <- row.names(tblTransition)
    tblTransition <- melt(tblTransition, id.vars = 'tid')
    lstOut[[8]] <- tblTransition

    lblOutData <- c('ocurrences',
                    'ocurrences_probability',
                    'ocurrences_probability_pre_state',
                    'matrix_transitions',
                    'matrix_transitions_pre_state')
    names(lstOut) <- c(lblOutData,paste0('insideOnly_',lblOutData))

  }else{
    lstOut <- "empty or invalid data"
  }

  return(lstOut)
}

fnExportTransitionObjects <- function(pathId, dtaExport, timeStep, pathToSave){

    if(pathId=="normal"){
      pathId=""
    }else{
      dtaExport <- dtaExport[[pathId]]
    }

    fnExpandTables <- function(dtaTime, dtaName){
      tblExpanded <- expand.grid(dtaTime, dtaName$Name)
      colnames(tblExpanded) <- c('tid', 'variable')
      tblExpanded$variable <- as.character(tblExpanded$variable)
      tblExpanded$tid <- as.factor(tblExpanded$tid)
      return(tblExpanded)
    }

    fnMakeStandardTable <- function(dtaTransition, dtaStructure){
      dtaStd <- join(dtaTransition, dtaStructure, by=c('tid', 'variable'))
      dtaStd$variable <- as.factor(dtaStd$variable)
      dtaStd$variable <-
        factor(dtaStd$variable, levels(dtaStd$variable)[tblActivitiesSorted])
      dtaStd <- dtaStd[order(dtaStd$variable),]

      return(dtaStd)
    }

    fnMakeStdData <- function(dtaToStd, varStep){

      colnames(dtaToStd) <- c('tid','variable','value')
      dtaToStd$variable <- as.character(dtaToStd$variable)
      dtaToStd <- dtaToStd[!is.na(dtaToStd$variable),]

      tblTimeSlot <- fnMakeTimeSlot(varStep)
      tblActNames <- fnGetActivityGroup()
      tblStdTransition <- fnExpandTables(tblTimeSlot, tblActNames)

      tblStdTransition <- fnMakeStandardTable(tblStdTransition, dtaToStd)
      tblStdTransition$value[is.na(tblStdTransition$value)] <- 0

      return(tblStdTransition)
    }

    fnMakeStdMatrix <- function(dtaToStd, varStep){

      dtaMatrix.tid <- rownames(dtaToStd)
      dtaMatrix <- suppressMessages(melt(dtaToStd))
      dtaMatrix <- dtaMatrix[!is.na(dtaMatrix$variable),]
      dtaMatrix$variable <- as.character(dtaMatrix$variable)
      dtaMatrix$tid <- dtaMatrix.tid

      tblTimeSlot <- fnMakeTimeSlot(varStep)
      tblActNames <- fnGetActivityGroup()
      tblStdTransition <- fnExpandTables(tblTimeSlot, tblActNames)

      tblTransition <- fnMakeStandardTable(tblTransition, dtaMatrix)
      tblTransition$value[is.na(tblTransition$value)] <- 0
      tblTransition <- dcast(tblTransition, tid ~ variable)
      tblTransition$tid <- NULL

      return(tblTransition)
    }

    #.. define/locate folder
    pathToSave <- paste(pathToSave, pathId, sep="/")

    #.. check if the tables have been estimated
    varPresence <- paste0(timeStep,'_MtrxTimeDpndnt')
    varPresence <-
      list.files(path=pathToSave, pattern = varPresence, ignore.case = T)

    #.. check if tables are not empty or present in folder already
    if(is.list(dtaExport) & length(varPresence)==0){

      #.. plot data
      g1 <- figTransMatrixPlot(fnMakeStdData(dtaExport[[1]], timeStep),"bar")
      g2 <- figTransMatrixPlot(fnMakeStdData(dtaExport[[2]], timeStep))
      g3 <- figTransMatrixPlot(fnMakeStdData(dtaExport[[3]], timeStep))
      g4 <- figTransMatrixPlot(fnMakeStdData(dtaExport[[6]], timeStep),"bar")
      g5 <- figTransMatrixPlot(fnMakeStdData(dtaExport[[7]], timeStep))
      g6 <- figTransMatrixPlot(fnMakeStdData(dtaExport[[8]], timeStep))

      #.. export data

      dir.create(pathToSave, showWarnings = F, recursive = T)

      fileExport <- paste0(pathToSave, "/a_", timeStep, "_RecordEvents.pdf")
      pdf(file = fileExport, width=9, height=7)
      suppressWarnings(print(g1 + theme.global))
      dev.off()

      fileExport <- paste0(pathToSave, "/b_", timeStep, "_RecordPrprtn.pdf")
      pdf(file = fileExport, width=9, height=7)
      suppressWarnings(print(g2 + theme.global))
      dev.off()

      fileExport <- paste0(pathToSave, "/c_", timeStep, "_RecordTimeDpndnt.pdf")
      pdf(file = fileExport, width=9, height=7)
      suppressWarnings(print(g3 + theme.global))
      dev.off()

      fileExport <- paste0(pathToSave, "/d_", timeStep, "_MtrxPrprtn.csv")
      write.csv(fnMakeStdMatrix(dtaExport[[4]], timeStep), fileExport)

      fileExport <- paste0(pathToSave, "/e_", timeStep,"_MtrxTimeDpndnt.csv")
      write.csv(fnMakeStdMatrix(dtaExport[[5]], timeStep), fileExport)

      fileExport <- paste0(pathToSave, "/f_", timeStep, "_RecordEvents.pdf")
      pdf(file = fileExport, width=9, height=7)
      suppressWarnings(print(g4 + theme.global))
      dev.off()

      fileExport <- paste0(pathToSave, "/g_", timeStep, "_RecordPrprtn.pdf")
      pdf(file = fileExport, width=9, height=7)
      suppressWarnings(print(g5 + theme.global))
      dev.off()

      fileExport <- paste0(pathToSave, "/h_", timeStep, "_RecordTimeDpndnt.pdf")
      pdf(file = fileExport, width=9, height=7)
      suppressWarnings(print(g6 + theme.global))
      dev.off()

    }else if(length(varPresence)>0){
      message(paste("Tables already exist →", pathToSave))
    }else{
      message(paste("No data found here →", pathToSave))
    }

  }

fnExportTransition <-
  function(lstSets, lstData, valFactor, timeRes="60min", varAgeFilter=F){

    pathToExport <- paste("outputs", '_variable', valFactor, lstSets, sep="/")
    dir.create(pathToExport, showWarnings = F, recursive = T)

    if(varAgeFilter==TRUE){
      valAgeCats <- names(lstData[[lstSets]])
      lstFactor <- lstData[[lstSets]]

      dtaStates <- pblapply(lstFactor, fnGetTransitionTables, timeRes)

      lapply(names(dtaStates), fnExportTransitionObjects,
             dtaStates, timeRes, pathToExport)

    }else{
      dtaFactor <- lstData[[lstSets]]

      # prepare data for transition matrix
      dtaStates <- fnGetTransitionTables(dtaFactor, timeRes)

      fnExportTransitionObjects("normal", dtaStates, timeRes, pathToExport)
    }

    # return null
    print(paste("Exported to :", pathToExport))
  }

fnExportTransitionSingle <-
  function(lstSets, lstData, valFactor, timeRes="60min", varAgeFilter=F){

    pathToExport <- paste("outputs", '_household', valFactor, lstSets, sep="/")

    if(varAgeFilter==FALSE){
      dtaFactor <- lstData
    }else{
      dtaFactor <- lstData[[lstSets]]
    }

    # prepare data for transition matrix
    dtaStates <- fnGetTransitionTables(dtaFactor, timeRes)

    if(is.list(dtaStates)){
      fnExportTransitionObjects("normal", dtaStates,
                                timeRes, pathToExport)
    }else{
      print(paste(dtaStates, pathToExport, sep=" // "))
    }

    # return null
    message(paste("Exported to →", pathToExport))
  }

fnGetActivitiesbyHousehold <- function(valHhd, dtaInd=uktus15_individual,
  dtaDiary=uktus15_diary_ep_long, varExtra=FALSE){

    # Cases where no diary was collected are excluded here
    dtaSubset <- dtaDiary[dtaDiary$serial %in% valHhd, ]

    if(dim(dtaSubset)[1]<1){
      dtaSubset <- print("Warning: No Data in subset")
    }else{
      # add condition here to make subset by age categories
      if(varExtra==TRUE){
        dtaSubset$ExtraFactor <-
          factor(ifelse(dtaSubset$DVAge<17,
                        ifelse(dtaSubset$DVAge<10, 1, 2), 3),
                 levels = 1:3, labels = tblExtraFactors)
        lstSubset <- list()
        for(i in 1:3){
          lstSubset[[i]] <- subset(dtaSubset, ExtraFactor==tblExtraFactors[i])
          lstSubset[[i]]$ExtraFactor <- NULL
        }
        names(lstSubset) <- tblExtraFactors
        dtaSubset <- lstSubset
      }
    }

    return(dtaSubset)
  }
