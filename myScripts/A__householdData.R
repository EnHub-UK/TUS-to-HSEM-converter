#' ----------------------------------------------------------------------------
#' TUS Converter                                            {data collection}
#'
#' @file `A__householdData.R` collects TUS data and converts it to R objects.
#' ----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'


# Environment SETUP -----------------------------------------------------------
                                                        rm(list = ls(a = TRUE))
                                                            var.TUSyear <- 2015
                         source('myScripts/__setup_Project.R', verbose = FALSE)


# MAPPING ---------------------------------------------------------------------

#.. check location of variables in datasets -----------------------------------
findStrings("eligibility")
findStrings("Primary sampling unit")
findStrings("serial")
findStrings("evact1")
findStrings("type")


#.. display data structures ---------------------------------------------------
uktus15_diary_ep_long[1:4,]  # This dataset contains activities per time slot
uktus15_diary_wide[1:4,]     # Contains a column for each time slot & activity
uktus15_dv_time_vars[1:4,]   # Summarises minutes per activity
uktus15_household[1:4,]      # Household activities and profiles
uktus15_individual[1:4,]     # Individual activities and profiles
uktus15_wksched[1:4,]        # This dataset contains work schedules



# EXTRACTION ------------------------------------------------------------------

#.. extract household profiles

fnLoadTUSProcessed()


#.. generate summaries ----

lstHhd_hh_profiles <- fnGetInParameters(dtaHhd_hh_profiles)
 lstHhd_hh_profiles[[1]] # displays household demographics
 lstHhd_hh_profiles[[2]] # displays ownership of appliances



# NEXT > analysis, simulation, prediction -------------------------------------
# file.edit("myScripts/B__analysisSurvey.R")
# file.edit("myScripts/C__subsetSurvey.R")
# file.edit("myScripts/D__simulateMarkovChain.R")
# file.edit("myScripts/E__exportProfiles.R")

# END -------------------------------------------------------------------------
