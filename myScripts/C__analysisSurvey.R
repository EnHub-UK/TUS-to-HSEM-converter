#' ----------------------------------------------------------------------------
#' TUS Converter                                     {matrices of transition}
#'
#' @file `B__analysisSurvey.R` collects activities and generates matrices of
#'       transition for each (manually defined) household profile.
#'
#' @note This workflow is DEPRECATED.
#'       Its purpose is now purely pedagogical.
#'       As a revised and improved version has been integrated into EnHub
#'
#' ----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'
#' ----------------------------------------------------------------------------
#'
#' @flow
#'
#'  +-----------------------------------------------------+
#'  |1 / define variable (`varFactor`)                    |
#'  +-----+-----------------------------------------------+
#'        |
#'  +-----v-----------------------------------------------+
#'  |2 / extract factor combinations (`tblFactors`)       |
#'  +-----+-----------------------------------------------+
#'        |
#'  +-----v-----------------------------------------------+
#'  |3 / subset factor combinations (`varFctPick`)        |
#'  +-----+-----------------------------------------------+
#'        |
#'  +-----v-----------------------------------------------+
#'  |4 / define additional parameters (`varAge`)          |
#'  +-----+-----------------------------------------------+
#'        |
#'  +-----v-----------------------------------------------+
#'  |5 / generate transitions (`fnGetActivitiesbySubset`) |
#'  +-----+-----------------------------------------------+
#'        |
#'  +-----v-----------------------------------------------+
#'  |6 / save outputs (`fnExportTransition`)              |
#'  +-----------------------------------------------------+
#'


# Environment SETUP -----------------------------------------------------------
source('myScripts/A__householdData.R')
source('myScripts/_aux_Transition.R')
source('myScripts/_aux_PlotFunctions.R')




# ACTIVITY collection --------------------------------------------------------

#.. Standard example ----------------------------------------------------------

#... (steps 1,2 & 3) ----------------------------------------------------------

#..... subset by household ----
varFactor <- 'dhhtype'
(tblFactors <-
    data.frame(Id=LETTERS[1:nlevels(uktus15_individual[,varFactor])],
               Name=as.character(levels(uktus15_individual[,varFactor])),
               stringsAsFactors=F))
varFctPick <- list(single=1,
                   couple=c(2,3),
                   singleparent=c(4,5),
                   complex=c(6,7,8),
                   unrelated=8,
                   all=1:8)

#..... subset by tenure ----
varFactor <- 'Tenure'
(tblFactors <-
    data.frame(Id=LETTERS[1:nlevels(uktus15_individual[,varFactor])],
               Name=as.character(levels(uktus15_individual[,varFactor])),
               stringsAsFactors=F))
varFctPick <- list(buyer=c(6,7),
                   renter=c(8,9,10),
                   all=1:11)

#..... subset by education ----
varFactor <- 'dhiqual'
(tblFactors <-
    data.frame(Id=LETTERS[1:nlevels(uktus15_individual[,varFactor])],
               Name=as.character(levels(uktus15_individual[,varFactor])),
               stringsAsFactors=F))
varFctPick <- list(educationnan=1:5,
                   higher=c(6,7),
                   basic=c(8,9,10),
                   all=1:10)


#... (step 4) -----------------------------------------------------------------
# i.e. determining whether to use age and considering outside activity
varAge <- TRUE    # : Additional subset by age categories

#... (step 5) -----------------------------------------------------------------
dtaFactor <- pblapply(varFctPick,
                      fnGetActivitiesbySubset,
                      varFactor, tblFactors,
                      varExtra=varAge)

#... (step 6) -----------------------------------------------------------------
pblapply(names(dtaFactor), fnExportTransition,
         dtaFactor, varFactor, "10min", varAge)



# Example with new clusters ---------------------------------------------------

# + Here, before applying the steps described above, a new column is added
# to the dataset. For instance, an application for this step-0 might be the
# consideration/creation of MOSAIC categories (that is, 65 socio-demographic
# profiles of the UK population) or the consideration of dwelling typologies
# corresponding to No-MASS or EnHub categories.

# (#.) First, the TUS data set is duplicated. Note that this data.frame needs
# to be declared when calling `fnGetActivitiesbySubset`.
dtaHhd <- fnGetIndices()
uktus15_individual.der <- join(uktus15_individual, dtaHhd, by='serial')

# (#.) Second, mixed categories are created
uktus15_individual.der$cluster.A <-
  as.factor(paste(uktus15_individual.der$dhhtype,
                  uktus15_individual.der$dtenure, sep=" + "))

uktus15_individual.der$cluster.B <-
  as.factor(paste(uktus15_individual.der$HiQual,
                  uktus15_individual.der$deconact, sep=" + "))

uktus15_individual.der$cluster.C <-
  as.factor(tolower(paste(uktus15_individual.der$dhhtype,
                          uktus15_individual.der$dtenure,
                          uktus15_individual.der$deconact, sep=" + ")))

uktus15_individual.der$cluster.D <-
  as.factor(tolower(ifelse(uktus15_individual.der$DVAge<12,
                           "withKids","noKids")))

uktus15_individual.der$cluster.E <-
  as.factor(tolower(paste(uktus15_individual.der$.dwtype,
                          uktus15_individual.der$.region,
                          uktus15_individual.der$.tenure,
                          uktus15_individual.der$.hhsize, sep=" + ")))

#... (steps 1,2 & 3) ----------------------------------------------------------

#..... define cluster C / profiles ----

varFactor <- 'cluster.C'
(tblFactors <-
    data.frame(Id=1:nlevels(uktus15_individual.der[,varFactor]),
               Name=as.character(levels(uktus15_individual.der[,varFactor])),
               stringsAsFactors=F))
varFctPick <- list(example=1:4, seeker=98, student=seq(12,385, by=12))

#..... define cluster D / children presence ----

varFactor <- 'cluster.D'
(tblFactors <-
    data.frame(Id=1:nlevels(uktus15_individual.der[,varFactor]),
               Name=as.character(levels(uktus15_individual.der[,varFactor])),
               stringsAsFactors=F))
varFctPick <- list(kidsNo=1, kidsYes=2)

#..... define cluster E / household typologies ----

varFactor <- 'cluster.E'
(tblFactors <-
    data.frame(Id=1:nlevels(uktus15_individual.der[,varFactor]),
               Name=as.character(levels(uktus15_individual.der[,varFactor])),
               stringsAsFactors=F))
varFctPick <- list(flats=1:122, houses=123:511, rentedGORYrks=483:497)


#... (steps 4,5, & 6) ---------------------------------------------------------

varAge <- TRUE    # : Additional subset by age categories

dtaFactor <- pblapply(varFctPick,
                      fnGetActivitiesbySubset,
                      varFactor, tblFactors,
                      dtaInd=uktus15_individual.der,
                      dtaDiary=uktus15_diary_ep_long,
                      varExtra=varAge)

pblapply(names(dtaFactor), fnExportTransition,
         dtaFactor, varFactor, "10min", varAge)



#.. Selected household(s) / Alternative process -------------------------------

varAge <- TRUE    # : Additional subset by age categories

dtaIndividual <- uktus15_individual
(tblHhd <- sample(dtaIndividual$serial, 111))

dtaIndividual <-
  fnGetActivitiesbyHousehold(tblHhd, dtaIndividual,
                             uktus15_diary_ep_long, varAge)
varFctPick <- names(dtaIndividual)[c(dim(dtaIndividual$kids)[1]>0,
                                     dim(dtaIndividual$young)[1]>0,
                                     dim(dtaIndividual$adult)[1]>0)]
pblapply(varFctPick, fnExportTransitionSingle,
         dtaIndividual, 'some_hhds', "10min", varAge)


# END -------------------------------------------------------------------------
