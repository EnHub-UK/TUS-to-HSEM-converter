#' ----------------------------------------------------------------------------
#' TUS Converter                                        {descriptive analysis}
#'
#' This second stage helps have an overview about the datasets.
#'
#' ----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'


# Environment SETUP -----------------------------------------------------------
#                       source('myScripts/A__householdData.R', verbose = FALSE)
                                                            .fnStartLibraries()
                                       source('myScripts/_aux_PlotFunctions.R')


# Household DIARY -------------------------------------------------------------

#.. what and when? -----

id.hhd <-  '11011202'
lst.hhd.profile <- dtaHhd_hh_profiles[[id.hhd]]

figActivitiesBars(lst.hhd.profile$dailyacts, 'WhereWhen', 'pnum')
figActivitiesBars(lst.hhd.profile$dailyacts, 'whatdoing', 'pnum')

figActivitiesPanel(lst.hhd.profile$dailyacts,
                   'tid', 'whatdoing', 'DiaryDate_Act', 'DVAge', "same")
figActivitiesPanel(lst.hhd.profile$dailyacts %>% filter(WhereWhen=='Home'),
                   'tid', 'whatdoing', 'DiaryDate_Act', 'DVAge', "same")

dcast(lst.hhd.profile$dailyacts, tid ~ whatdoing)



#.. what activities are made together? -----

#.. per household

id.hhd <-  '11011202'
lst.hhd.profile <- dtaHhd_hh_profiles[[id.hhd]]

(tbl.hhd.daily <- lst.hhd.profile$dailyacts %>%
  select(whatdoing, What_Oth1) %>%
  filter(!grepl('No answer/refused', What_Oth1)) %>%
  group_by(whatdoing, What_Oth1) %>%
  tally() %>% arrange(desc(n)) %>% droplevels())

ggplot(tbl.hhd.daily, aes(whatdoing, weight=n, fill=What_Oth1)) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d(option = "B") + coord_flip() + theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank())

#.. all households

tbl.hhd.daily <- lapply(dtaHhd_hh_profiles, `[[`, 'dailyacts')

(tbl.hhd.daily <- ldply(tbl.hhd.daily, data.frame) %>%
  select(whatdoing, What_Oth1) %>%
  filter(!grepl('No answer/refused', What_Oth1)) %>%
  group_by(whatdoing, What_Oth1) %>%
  tally() %>% arrange(desc(n)) %>% droplevels())

treemap(tbl.hhd.daily,
        index=c("whatdoing", "What_Oth1"),
        vSize="n", type="index",
        palette = viridis::viridis(36))


#.. pick an activity and check occurrence (beginning of) ---

id.hhd <-  '11011202'
tbl.hhd.diary <- uktus15_diary_ep_long %>% filter(serial==id.hhd)

tbl.diary.ref <- fnGetActionsTable(tbl.hhd.diary)

tbl.hhd.diary <- pblapply(tbl.diary.ref$Id,
                          fnGetActionsPerSurveyed,
                          tbl.hhd.diary)
names(tbl.hhd.diary) <- tbl.diary.ref$Name

tbl.hhd.act <-
  join(tbl.hhd.diary$`Computing - programming`, fnMakeTimeSlotTable(), by='tid')
tbl.hhd.act$time <- hm(tbl.hhd.act$tid.from)

ggplot(tbl.hhd.act, aes(time, value, na.rm = TRUE)) +
  geom_step() +
  facet_wrap(~ variable, ncol = 2) +
  ggtitle('eating') +
  scale_x_time() +
  theme_bw()
dcast(tbl.hhd.act, tid.from ~ variable)




# Household DEMOGRAPHICS ------------------------------------------------------

#.. get some population profiles ----

tbl.hhd.demo <- lstHhd_hh_profiles$household.demographics

(tbl.hhd.demo.sub <- tbl.hhd.demo %>% select(serial, .tenure, worksta_p1, income) %>%
  mutate(income=as.numeric(income)) %>%
  filter(income>0, income < max(income)) %>%
  arrange(desc(income)))

ggplot(tbl.hhd.demo.sub, aes(income, fill=worksta_p1)) +
  geom_histogram(position = "stack") +
  scale_x_log10(labels=scales::dollar_format(prefix = '£')) +
  facet_wrap(~ .tenure) +
  xlab("monthly income [ £ ] log.scale") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "top")


(tbl.hhd.demo.sub <- tbl.hhd.demo %>%
    select(serial, hiqual, dhhtype, income) %>%
    mutate(income=as.numeric(income)) %>%
    filter(income>0, income < max(income)) %>%
    arrange(desc(income)))

ggplot(tbl.hhd.demo.sub, aes(income, after_stat(count), fill=hiqual)) +
  geom_density(colour=NA, alpha=1, position = position_stack(reverse = F)) +
  scale_x_log10(labels=scales::dollar_format(prefix = '£')) +
  facet_wrap(~ dhhtype) +
  xlab("monthly income [ £ ] log.scale") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "top")


(tbl.hhd.demo.sub <- tbl.hhd.demo %>%
    select(serial, satisov, numrooms, numadult) %>%
    mutate(numrooms=as.numeric(numrooms)) %>%
    filter(numrooms>0) %>%
    arrange(desc(numrooms)))

ggplot(tbl.hhd.demo.sub, aes(numrooms, after_stat(count), fill=numadult)) +
  geom_histogram(colour=NA, alpha=1, position = "stack", binwidth = 1) +
  facet_wrap(~ satisov, scales = "free_y") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "top")


tbl.hhd.demo.sub <- tbl.hhd.demo %>%
  select(serial, hiqual, dhhtype, .dwtype, .region) %>%
  group_by(hiqual, dhhtype, .dwtype) %>%
  mutate(num=n()) %>%
  arrange(desc(num))

ggplot(tbl.hhd.demo.sub, aes(dhhtype)) +
  geom_bar() + coord_flip() +
  facet_grid(.dwtype ~ .region) +
  theme_minimal()




#.. what (ownership) -----

tbl.hhd.demo <- lstHhd_hh_profiles$appliances.ownership

ggplot(tbl.hhd.demo, aes(TVSet, fill=Cable)) +
  geom_bar() + scale_fill_viridis_d() + theme_minimal() + coord_fixed(0.00025)

ggplot(tbl.hhd.demo, aes(WashMach, fill=Tumble)) +
  geom_bar() + scale_fill_viridis_d() + theme_minimal() + coord_fixed(0.00025)








# NEXT > analysis, simulation, prediction -------------------------------------
# file.edit("myScripts/C__analysisSurvey.R")
# file.edit("myScripts/D__subsetSurvey.R")
# file.edit("myScripts/E__simulateMarkovChain.R")
# file.edit("myScripts/F__exportProfiles.R")

# END -------------------------------------------------------------------------
