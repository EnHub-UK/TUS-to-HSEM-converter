#' ----------------------------------------------------------------------------
#' TUS Converter                                            {data collection}
#'
#' This first stage collects TUS data and converts them to R objects.
#'
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

pblapply(c("serial","pnum","Accom","dtenure","dhhtype","dgorpaf",
           "Income", "deconact", "NumAdult","NumChild",
           "HiQual","SatisOv", "DMSex_P1","WorkSta_P1"), findStrings)


#.. display data structures ---------------------------------------------------
uktus15_diary_ep_long[1:4,]  # This dataset contains activities per time slot
uktus15_diary_wide[1:4,]     # Contains a column for each time slot & activity
uktus15_dv_time_vars[1:4,]   # Summarises minutes per activity
uktus15_household[1:4,]      # Household activities and profiles
uktus15_individual[1:4,]     # Individual activities and profiles
uktus15_wksched[1:4,]        # This dataset contains work schedules



# EXTRACTION ------------------------------------------------------------------

#.. 1) extract household profiles ----
#     one by one, to obtain household relationships

fnLoadTUSProcessed()


#.. 2) generate summaries ----

lstHhd_hh_profiles <- fnGetInParameters(dtaHhd_hh_profiles)
 lstHhd_hh_profiles[[1]] # displays household demographics
 lstHhd_hh_profiles[[2]] # displays ownership of appliances



# NEXT > analysis, simulation, prediction -------------------------------------
file.edit("myScripts/B__descriptive.R")
file.edit("myScripts/C__analysisSurvey.R")

# END -------------------------------------------------------------------------
