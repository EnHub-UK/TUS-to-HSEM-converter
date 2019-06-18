tblActivities_00 <- read.csv("Activities00.csv", stringsAsFactors = F)
tblActivities_15 <- read.csv("Activities15.csv", stringsAsFactors = F)
tblActivities <- tblActivities_15
tblActivities <- rbind(tblActivities, tblActivities_00[tblActivities_00$Id %nin% tblActivities_15$Id,])
tblActivities <- tblActivities[order(tblActivities$Id),]
rownames(tblActivities) <- NULL
tblActivities$in2000 <- tblActivities$Id %in% tblActivities_00$Id
tblActivities$in2015 <- tblActivities$Id %in% tblActivities_15$Id
tblActivities$inBoth <- ifelse(tblActivities$in2000==T & tblActivities$in2015==T, T, F)