#' -----------------------------------------------------------------------------
#' TUS Converter                                                 {Data Parser}
#'
#' This contains auxiliary functions to load raw data and
#' process selected variables
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'


# Data   -----------------------------------------------------------------------

path.aux <- "/myData/AuxiliaryData"
file.act <- paste0(path.TUS, path.aux, "/activities/ActivitiesTable.csv")
file.fct <- paste0(path.TUS, path.aux, "/activities/ActivitiesFactors.csv")
file.rel <- paste0(path.TUS, path.aux, "/Relationships.csv")

tblActivities <- read.csv(file.act, stringsAsFactors = F, row.names = 1)
tblActivitiesRef <- read.csv(file.fct, stringsAsFactors = F)
tblActivities$Group <-
  factor(tblActivities$Group,
				 levels = tblActivitiesRef$Id, labels = tblActivitiesRef$Name)

tblActivitiesSorted <- c(9,8,1,4,3,2,5,10,6,7)

tblRelationships <- read.csv(file.rel, stringsAsFactors = F)

tblExtraFactors <- c("kids", "young", "adult")

dtaTimeSlot <- fnMakeTimeSlotTable()
dtaTimeSlot <- dtaTimeSlot[,c('tid','tid.slot')]




# Extra Processes --------------------------------------------------------------

if(var.TUSyear==2000){
  dta.uktus00.names <- as.data.frame(tblTUSRawSubsets, stringsAsFactors = F)
  dta.uktus00.names$newName <-
					paste("uktus00_",dta.uktus00.names$tblTUSRawSubsets,sep="")
  for(i in 1:length(tblTUSRawSubsets)){
    assign(dta.uktus00.names$newName[i],
					 get(dta.uktus00.names$tblTUSRawSubsets[i]), envir = .GlobalEnv)
    rm(list=dta.uktus00.names$tblTUSRawSubsets[i])
  }
  message("➥ databases have been relabelled")
}else{
  uktus15_diary_ep_long$tid <- factor(uktus15_diary_ep_long$tid,
                                      levels=dtaTimeSlot$tid,
                                      labels=dtaTimeSlot$tid.slot)

  uktus15_diary_ep_long$whatdoing <-
    as.character(uktus15_diary_ep_long$whatdoing)
  uktus15_diary_ep_long$What_Oth1 <-
    as.numeric(uktus15_diary_ep_long$What_Oth1)
  uktus15_diary_ep_long$What_Oth2 <-
    as.character(uktus15_diary_ep_long$What_Oth2)
  uktus15_diary_ep_long$What_Oth3 <-
    as.character(uktus15_diary_ep_long$What_Oth3)

  uktus15_diary_ep_long$whatdoing <- sapply(uktus15_diary_ep_long$whatdoing,
                                            FUN=fnGetActivity)
  uktus15_diary_ep_long$What_Oth2 <- sapply(uktus15_diary_ep_long$What_Oth2,
                                            FUN=fnGetActivity)
  uktus15_diary_ep_long$What_Oth3 <- sapply(uktus15_diary_ep_long$What_Oth3,
                                            FUN=fnGetActivity)
  uktus15_diary_ep_long$whatdoing <- factor(uktus15_diary_ep_long$whatdoing,
                                            levels=tblActivities$Id,
                                            labels=tblActivities$Name)
  uktus15_diary_ep_long$What_Oth1 <- factor(uktus15_diary_ep_long$What_Oth1,
                                            levels=tblActivities$Id,
                                            labels=tblActivities$Name)
  uktus15_diary_ep_long$What_Oth2 <- factor(uktus15_diary_ep_long$What_Oth2,
                                            levels=tblActivities$Id,
                                            labels=tblActivities$Name)
  uktus15_diary_ep_long$What_Oth3 <- factor(uktus15_diary_ep_long$What_Oth3,
                                            levels=tblActivities$Id,
                                            labels=tblActivities$Name)

  message("➥ Factors have been re-formatted")
}
