library(tidyverse)
library(psych)
library(kableExtra)

# For Juan
d <- read_rds("../data/2019_1_22_c2016_syfus_inst_healthrec_ctra_no_t7_frosh_soph_junior.rds")

# For RAs
# getwd()
# d <- read_rds("../../../Box Sync/Research Assistants/data/2018_4_2_c2016_syfus_inst_healthrec_ctra_no_t7.rds")

source("custom_functions/custom_functions.R")

# d %>%
#   check_vars_by_keywords(c("org"))
# d %>% 
#   count(org1__type)

vars_check <-
  quos(
    mentor_faculty,
    mentor_grad,
    mentor_undergrad
    # mentor_admin,
    # mentor_staff,
    # mentor_athletic,
    # mentor_alum,
    # mentor_other
  )

d %>%
  select(!!!vars_check) %>%
  describe %>%
  as.data.frame() %>%
  rownames_to_column(var = "dv_name") %>%
  mutate_at(vars(-dv_name), funs(round(., digits = 2))) %>%
  kable_format

# after t1manipcheck, we need to add the other variables

#### Continuous variables (Linear Regressions) #### 

vars_cont <- 
  quos(
    # Institutional Data
    # iHSGpa,                         # need to review this variable since it's not in the right scale
    cumgpa1y,
    Y1_Spring_CumGPA, 
    cum_gpa_y1_spring,
    Y2_Sp_CumGPA,                
    cum_gpa_y2_spring,
    cum_gpa_y3_spring,
    cum_gpa_y4_spring,
    cum_stem_gpa_y4_spring,
    
    # Pre-Matriculation Intervention
    ## Opportunities
    t1opportunity_joincommunities,    # missing composite and graphs
    t1opportunity_giveback,            
    t1opportunity_exploreinterests,    
    t1opportunity_becomeindepthinker,  
    
    ## Belonging at Fall
    t1fall_fitin,                     # missing composite and graphs
    t1fall_belong,                     
    t1fall_feelathome,   
    
    ## Critical Feedback
    t1critfeedback_improvegrow,       # missing composite
    t1critfeedback_distinguishbtwnSs,
    
    ## Positive Emotions
    t1excited,                        # missing composite and graphs
    t1enjoy,
    t1fun,
    t1transitiondifficultatfirst,
    t1experiencedifficultiesatfirst,
    
    ## Feelings
    ### Positive Feelings
    posfeels_comp4,  
    t1posfeelings_profsTAs,  
    t1posfeelings_gettingtoknowotherSs, 
    t1posfeelings_beingawayfromhome, 
    t1posfeelings_receivingfeedback, 
    ### Negative Feelings
    negfeels_comp4, 
    t1negfeelings_profsTAs, 
    t1negfeelings_gettingtoknowotherSs, 
    t1negfeelings_beingawayfromhome, 
    t1negfeelings_receivingfeedback,
    ### Handling Stress
    t1handleit_profsTAs,              # missing composites 
    t1handleit_gettingtoknowotherSs,
    t1handleit_beingawayfromhome,
    t1handleit_receivingfeedback,
    
    # Senior Year Survey
    ## Success and Potential
    success_usethis_percent, # Success
    potential_usethis_percent, # Potential
    
    ## Health
    healthmosoverall_comp5, # General Health
    health_behavbingedrink, 
    health_behavtobacco, 
    health_behavsleepiness, 
    stressdaily,
    pss_comp4,
    health_behavrelax,
    mhealthgeneral,
    anxscreener_comp2,
    depscreener_comp2,
    overallmentalhealth_comp5_z,
    anxdepscreener_comp4,
    bmi_1y,
    bmi,
    bmi_chge_scr,
    weight, 
    weight_comp3, 
    weight_happy_rc, 
    weight_feelbad, 
    weight_trylose, 
    
    ## Well-Being
    happy_comp4,
    purpose_comp2,
    lifesat,
    
    ## Belonging and College Experience
    belong_comp6,
    belong_comp4,
    belong_belong,
    belong_fitin,
    belong_similar,
    belong_outsider_r_use_this,  
    belong_understand_r_use_this,
    belong_mystery_r_use_this,
    buwonder_r_use_this,
    overallpositivity,
    collexp_comp4,
    collhomeinteg_comp4,
    threat_nogen_comp8,       # review that we have all the submeasures plotted as well
    cthreat_comp2,
    cthreat_minorities,
    cthreat_self,
    stthreat_gender,
    stthreat_race,
    stthreat_ses,
    famrelate_comp2,
    integ_idgender,
    integ_idrace,
    integ_idclass,
    wisefeedbacktask_comp4,
    critf_distinguish_r,
    critf_grow,
    perf_comp2,
    perf_highexpect,
    perf_neversat,
    perf_notsatbest,
    id_gender,
    id_race,
    qualitiesind_comp3,
    qualitiesinter_2_comp4,
    igm_comp2,
    igmmajor1_2_3_comp3,
    pol_conslib,
    next_excited, 
    next_diffatfirst, 
    # donate_srgifttot, 
    donate_srgiftplan, 
    donate_srgift,
    
    ## Friends 
    friends_close, 
    closefriendscount,
    closenessfriends_comp7,
    friends_noshare_r,
    
    # Involvement
    org_howinvcount,
    activism_engaged,
    org_acad_inv,
    org_arts_inv,
    org_athletics_inv, 
    org_community_inv, 
    org_cult_inv,      
    org_games_inv,
    org_leader_inv,   
    org_media_inv,
    org_other_inv,    
    org_religion_inv,  
    org_social_inv
  ) 

tribble_dv <- tribble(
  ~dv,                                       ~dv_name,                                              ~scale,            ~limit,          ~position,            ~subheader,                                         ~decimal,
  #-----------------------------------------|------------------------------------------------------|-----------------|----------------|----------------------|----------------------------------------------------|---------
  # Institutional data
  # "iHSGpa",                                  "High School Cumulative GPA",                          seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               3,
  "cumgpa1y",                                "1-Year Cumulative GPA",                               seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  "Y1_Spring_CumGPA",                        "Other 1-Year Cumulative GPA",                         seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  "cum_gpa_y1_spring",                       "New 1-Year Cumulative GPA",                           seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  "Y2_Sp_CumGPA",                            "Old 2-Year Cumulative GPA",                           seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  "cum_gpa_y2_spring",                       "2-Year Cumulative GPA",                               seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  "cum_gpa_y3_spring",                       "3-Year Cumulative GPA",                               seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  "cum_gpa_y4_spring",                       "4-Year Cumulative GPA",                               seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  "cum_stem_gpa_y4_spring",                  "4-Year Cumulative GPA in STEM Classes",               seq(0, 4, 1),      c(0, 4),         0.5,                  "(Institutional Data)",                               2,
  
  # Pre-Matriculation Intervention
  "t1critfeedback_improvegrow",              "Crit. Feedback: Help Ss Grow",                        seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "t1critfeedback_distinguishbtwnSs",        "Critical Feedback: Distinguish Btwn Students",        seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "posfeels_comp4",                          "Overall Positivity of College Experience",            seq(1, 7, 1),      c(1, 7),         2,                    "(4 items; Pre-Matriculation Intervention)",          2,
  "t1posfeelings_profsTAs",                  "Positive Experiences w/ Profs. and TAs",              seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,  
  "t1posfeelings_gettingtoknowotherSs",      "Positive Feelings Getting to Know Other Ss",          seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "t1posfeelings_beingawayfromhome",         "Positive Feelings Being Away from Home",              seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "t1posfeelings_receivingfeedback",         "Positive Feelings About Receiving Feedback",          seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "negfeels_comp4",                          "Overall Negativity of College Experience",            seq(1, 7, 1),      c(1, 7),         2,                    "(4 items; Pre-Matriculation Intervention)",          2,
  "t1negfeelings_profsTAs",                  "Negative Experiences w/ Profs. and TAs",              seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "t1negfeelings_gettingtoknowotherSs",      "Negative Feelings Getting to Know Other Students",    seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "t1negfeelings_beingawayfromhome",         "Negative Feelings Being Away from Home",              seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  "t1negfeelings_receivingfeedback",         "Negative Feelings Receiving Feedback",                seq(1, 7, 1),      c(1, 7),         2,                    "(Pre-Matriculation Intervention)",                   2,
  
  
  # Senior Year Survey
  ## Success & Potential
  "success_usethis_percent",                 "Success: Percentile Ranking",                         seq(0, 100, 25),   c(0, 100),       10,                   "(Senior Year Survey)",                               2,       
  "potential_usethis_percent",               "Potential: Percentile Ranking",                       seq(0, 100, 25),   c(0, 100),       10,                   "(Senior Year Survey)",                               2,
  
  ## Health
  "healthmosoverall_comp5",                  "Composite:\nSelf-Reported General Health",            seq(1, 5, 1),      c(1, 5),         1.5,                  "(5 items; Senior Year Survey)",                      2,
  "health_behavbingedrink",                  "Frequency of Binge Drinking",                         seq(1, 5, 1),      c(1, 5),         1.5,                  "(Senior Year Survey)",                               2,
  "health_behavtobacco",                     "Tobacco Use During the Last Month",                   seq(0, 3, 1),      c(0, 3),         0.5,                  "(Senior Year Survey)",                               2,
  "health_behavsleepiness",                  "Daytime Sleepiness During the Last Month",            seq(1, 5, 1),      c(1, 5),         1.5,                  "(Senior Year Survey)",                               2,
  "stressdaily",                             "How Stressed",                                        seq(1, 10, 1),     c(1, 10),        2,                    "(Primary Appraisal; Senior Year Survey)",            2,
  "pss_comp4",                               "Composite:\nHow Overwhelmed by Stress",               seq(1, 5, 1),      c(1, 5),         1.5,                  "(4 items; Secondary Appraisal; Senior Year Survey)", 2,
  "health_behavrelax",                       "Time to Relax",                                       seq(1, 5, 1),      c(1, 5),         1.5,                  "(Senior Year Survey)",                               2,
  "mhealthgeneral",                          "Overall Mental Health",                               seq(1, 5, 1),      c(1, 5),         1.5,                  "(Single Item; Senior Year Survey)",                  2,
  "anxscreener_comp2",                       "Composite:\nAnxiety Screener",                        seq(1, 4, 1),      c(1, 4),         1.5,                  "(2 items; Senior Year Survey)",                      2,
  "depscreener_comp2",                       "Composite:\nDepression Screener",                     seq(1, 4, 1),      c(1, 4),         1.5,                  "(2 items; Senior Year Survey)",                      2,
  "overallmentalhealth_comp5_z",             "Composite:\nOverall Mental Health",                   seq(-1, 1, 0.5),   c(-1, 1),       -0.75,                 "(5 items; Average Standardized Score; Senior Year Survey)",   2,
  "anxdepscreener_comp4",                    "Composite:\nAnxiety and Depression Screeners",        seq(0, 3, 1),      c(0, 3),         0.5,                  "(4 Items; Senior Year Survey)",                      2,
  "bmi_1y",                                  "Body Mass Index",                                     seq(0, 40, 10),    c(0, 40),        5,                    "(BMI, First-Year Survey)",                           2,
  "bmi",                                     "Body Mass Index",                                     seq(0, 40, 10),    c(0, 40),        5,                    "(BMI, Senior Year In-Lab Follow-Up)",                          2,
  "bmi_chge_scr",                            "BMI Change Score",                                    seq(-3, 6, 1),      c(-3, 6),         -1.5,                  "(Senior Year In-Lab Follow-Up)",                               2,
  "weight",                                  "4-Year Weight",                                       seq(0, 200, 50),   c(0, 200),       25,                   "(Senior Year In-Lab Follow-Up)",                          2,
  "weight_comp3",                            "Total Weight Satisfaction",                           seq(1, 7, 1),      c(1, 7),         2,                    "(3 items; Senior Year In-Lab Follow-Up)",                          2,
  "weight_happy_rc",                         "Happiness with Weight",                               seq(1, 7, 1),      c(1, 7),         2,                    "(Senior Year In-Lab Follow-Up)",                          2,
  "weight_feelbad",                          "Feel Less Bad About Weight",                          seq(1, 7, 1),      c(1, 7),         2,                    "(Senior Year In-Lab Follow-Up)",                          2,
  "weight_trylose",                          "Trying to Lose Weight",                               seq(1, 7, 1),      c(1, 7),         2,                    "(Senior Year In-Lab Follow-Up)",                               2,
  
  
  ## Well-Being
  "happy_comp4",                             "Composite:\nHappiness",                               seq(1, 7, 1),       c(1, 7),        2,                    "(4 items; Senior Year Survey)",                      2,
  "purpose_comp2",                           "Composite:\nPurpose & Meaning",                       seq(1, 5, 1),       c(1, 5),        1.5,                  "(2 items; Senior Year Survey)",                      2,
  "lifesat",                                 "Life Satisfaction",                                   seq(1, 10, 1),      c(1, 10),       2.5,                  "(Senior Year Survey)",                               2,
  
  ## Belonging & College Experience
  "belong_comp6",                            "Composite:\nBelonging at College",        seq(1, 7, 1),       c(1, 7),        2,                    "(6 Items; Senior Year Survey)",                      2,
  "belong_comp4",                            "Composite:\nBelonging",                               seq(1, 7, 1),       c(1, 7),        2,                    "(4 Items; Senior Year Survey)",                      2,
  "belong_belong",                           "Belonging:\nBelong",                                  seq(1, 7, 1),       c(1, 7),        2,                    "(Single Item; Senior Year Survey)",                  2,
  "belong_fitin",                            "Belonging:\nFit In",                                  seq(1, 7, 1),       c(1, 7),        2,                    "(Single Item; Senior Year Survey)",                  2,
  "belong_similar",                          "Belonging:\nSimilar",                                 seq(1, 7, 1),       c(1, 7),        2,                    "(Single Item; Senior Year Survey)",                  2,
  "belong_outsider_r_use_this",              "Belonging:\nOutsider",                                seq(1, 7, 1),       c(1, 7),        2,                    "(Single Item, Reverse Coded; Senior Year Survey)",                  2,
  "belong_understand_r_use_this",            "Belonging:\nUnderstand",                              seq(1, 7, 1),       c(1, 7),        2,                    "(Single Item, Reverse Coded; Senior Year Survey)",                  2,
  "belong_mystery_r_use_this",               "Belonging:\nMystery",                                 seq(1, 7, 1),       c(1, 7),        2,                    "(Single Item, Reverse Coded; Senior Year Survey)",                  2,
  "buwonder_r_use_this",                     "Belonging Uncertainty",                               seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2,
  "overallpositivity",                       "Overall Positivity of College Experience",            seq(1, 10, 1),      c(1, 10),       2.5,                  "(Senior Year Survey)",                               2,
  "collexp_comp4",                           "Composite:\nPositive Perception of Stanford",         seq(1, 7, 1),       c(1, 7),        2,                    "(4 items; Senior Year Survey)",                      2,
  "collhomeinteg_comp4",                     "Composite:\nCollege-Home Integration",                seq(1, 5, 1),       c(1, 5),        1.5,                  "(4 items; Senior Year Survey)",                      2,
  "threat_nogen_comp8",                      "Composite:\nFeelings of Psychological Threat",        seq(-1, 1, 0.5),    c(-1, 1),       -0.75,                "(8 Items; Senior Year Survey)",                      2,
  "cthreat_minorities",                      "Contextual Threat:\nOthers",                          seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2,
  "cthreat_self",                            "Contextual Threat:\nSelf",                            seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2,
  "cthreat_comp2",                           "Contextual Threat:\nCombined",                        seq(1, 5, 1),       c(1, 5),        1.5,                  "(2 items; Senior Year Survey)",                      2,
  "stthreat_gender",                         "Stereotype Threat: Gender",                           seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2, 
  "stthreat_race",                           "Stereotype Threat: Race",                             seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2,
  "stthreat_ses",                            "Stereotype Threat: Social Class",                     seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2, 
  "famrelate_comp2",                         "Composite:\nHome-School Disconnect",                  seq(1, 6, 1),       c(1, 6),        2,                    "(2 items; Senior Year Survey)",                      2,
  "integ_idgender",                          "College-Identity Friction:\nGender",                  seq(1, 5, 1),       c(1, 5),        2,                    "(Senior Year Survey)",                               2,
  "integ_idrace",                            "College-Identity Friction:\nRace",                    seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2, 
  "integ_idclass",                           "College-Identity Friction:\nSocial Class",            seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2,
  "wisefeedbacktask_comp4",                  "Wise Feedback Task",                                  seq(1, 5, 1),       c(1, 5),        1.5,                  "(4 items; Senior Year Survey)",                      2,
  "critf_distinguish_r",                     "Critical Feedback:\nDistinguish Between Students",    seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2, 
  "critf_grow",                              "Critical Feedback:\nHelp Students Grow",              seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,
  "perf_highexpect",                         "Perfectionism:\nStandards",                           seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,
  "perf_comp2",                              "Perfectionism:\nNever Satisfied",                     seq(1, 7, 1),       c(1, 7),        2,                    "(2 items; Senior Year Survey)",                      2,
  "id_gender",                               "Identification:\nGender",                             seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2, 
  "id_race",                                 "Identification:\nRace",                               seq(1, 5, 1),       c(1, 5),        1.5,                  "(Senior Year Survey)",                               2,
  "qualitiesind_comp3",                      "Independence Valued",                                 seq(1, 5, 1),       c(1, 5),        1.5,                  "(3 items; Senior Year Survey)",                      2,
  "qualitiesinter_2_comp4",                  "Interdependence Valued",                              seq(1, 5, 1),       c(1, 5),        1.5,                  "(4 items; Senior Year Survey)",                      2,
  "igm_comp2",                               "Institutional Mindsets:\nGeneral",                    seq(1, 6, 1),       c(1, 6),        2,                    "(2 items; Senior Year Survey)",                      2,
  "igmmajor1_2_3_comp3",                     "Institutional Mindsets:\nMajor",                      seq(1, 6, 1),       c(1, 6),        2,                    "(3 items; Senior Year Survey)",                      2,
  "pol_conslib",                             "Politics:\nConservative-Liberal",                     seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,
  "next_excited",                            "Excitement for Post-college Plans",                   seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,
  "next_diffatfirst",                        "Next Plans: Difficult at First",                      seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,
  # "donate_srgifttot",                        "Amount of Money Donated",                             seq(0, 550, 25),   c(0, 550),       45,                   "(Senior Year Survey)",                             2,
  "donate_srgiftplan",                       "Plan to Donate",                                      seq(0, 150, 25),   c(0, 150),       15,                   "(Senior Year Survey)",                               2,
  "donate_srgift",                           "Amount of Money Donated",                             seq(0, 150, 25),   c(0, 150),       15,                   "(Senior Year Survey)",                               2, 
 
  ## Friends 
  "friends_close",                           "Made Close Friends at College",                       seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,                  
  "closefriendscount",                       "Number of Close Friends",                             seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,                  
  "closenessfriends_comp7",                  "Close Friends (Average Closeness)",                   seq(1, 7, 1),       c(1, 7),        2,                    "(Senior Year Survey)",                               2,                  
  "friends_noshare_r",                       "Can't Share Worries and Fears With Friends",          seq(1, 7, 1),       c(1, 7),        2.5,                    "(Senior Year Survey, reverse coded)",                2,                  
  
  ## Involvement
  "org_howinvcount",                         "Involvement in Extracurricular Activities",           seq(0, 25, 5),       c(0, 25),      2,                    "(Five Activities; Senior Year Survey)",               2,
  "activism_engaged",                        "Involvement in Service/Social Change",                seq(0, 7, 1),       c(0, 7),        2,                    "(Senior Year Survey)",                               2,
  "org_acad_inv",                            "Involvement in Organizations:\nAcademics",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_arts_inv",                            "Involvement in Organizations:\nArts",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_athletics_inv",                       "Involvement in Organizations:\nAthletics or Recreation",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_community_inv",                       "Involvement in Organizations:\nCommunity, Health,Education,\nConservation or Outreach",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_cult_inv",                            "Involvement in Organizations:\nCultural Organizations",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_games_inv",                           "Involvement in Organizations:\nGames",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_leader_inv",                          "Involvement in Organizations:\nLeadership or Politics",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_media_inv",                           "Involvement in Organizations:\nMedia or Communications",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_other_inv",                           "Involvement in Organizations:\nOther",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_religion_inv",                        "Involvement in Organizations:\nReligion or Philosophy",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2,
  "org_social_inv",                          "Involvement in Organizations:\nSocial (Including Greek Life)",            seq(0, 5, 1),       c(0, 5),        2,                    "(Senior Year Survey)",                               2
  )  

tribble_cond <- data.frame(matrix(c("treat_sc"), nrow = nrow(tribble_dv)),
                           matrix(c("cond_sc"), nrow = nrow(tribble_dv)))

names_cond <- c("treat_sc", "cond_sc")
colnames(tribble_cond) <- names_cond

tribble_cont <- bind_cols(tribble_dv, tribble_cond)

rm(tribble_dv, tribble_cond)

vars_ctra <- quos(ctrarank_comp, ctraz_comp)

#### Categorical Variables (Logistic Regressions) #### 
vars_categ <- 
  quos(
    # Institutional Data
    major_grad_4y,
    major_grad_5y,
    class_research_ever,
    exchange_ever,
    quintile_top,
    quintile_bottom,
    probation,
    major_dbl_hnrs_ctrm,
    major_double,
    major_grad_yes_honors,
    coterm_ever,
    major_bs_yes,
    major_ba_yes,
    major_cs_yes,
    major_school_engineering,
    major_school_environmental,
    major_school_humanitiesscience, 
    major_culture_2,
    major_socialscience, 
    major_humanities, 
    major_stem_all,
    major_stem_hard,
    
    # Pre-Matriculation Intervention
    mc_wf,
    
    # Senior Year Survey
    # major2_yes, # we need to recode this variable
    tobaccouse_yes,
    bingedrink_yes,
    next_plans2,
    next_plans8,
    srgift_donated, # donated senior gift
    srgift_plandonate, # plan to donate senior gift
    srgift_planordonated, # donated or plan to donate senior gift
    honors_yes_usethis,
    mentor_yes,
    mentor_faculty,
    mentor_grad,
    mentor_undergrad
  )

tribble_dv <- tribble(
  ~dv,                          ~dv_name,                                           ~scale,             ~limit,        ~position,     ~subheader,
  #---------------------------|----------------------------------------------------|-------------------|--------------|--------------|-----------------------------------------------------------------
  # Institutional data
  "major_grad_4y",             "% Graduating in Four Years",                        seq(0, 100, 25),    c(0, 100),     10,            "(Institutional Data)",
  "major_grad_5y",             "% Graduating in Five Years",                        seq(0, 100, 25),    c(0, 100),     10,            "(Institutional Data)",
  "class_research_ever",       "% Involved in Research",                            seq(0, 100, 25),    c(0, 100),     10,            "(Institutional Data)",
  "exchange_ever",             "% Off-campus experiences ever",                     seq(0, 100, 25),    c(0, 100),     10,            "(Institutional Data)",
  "quintile_top",              "% with GPA in Top Quintile of Class",               seq(0, 50, 10),     c(0, 50),      2.5,           "(Institutional Data)",
  "quintile_bottom",           "% with GPA in Bottom Quintile of Class",            seq(0, 75, 25),     c(0, 75),      4,             "(Institutional Data)",
  "probation",                 "% Ever on Probation",                               seq(0, 100, 25),    c(0, 100),     10,            "(GPA Less Than 2 and Term Units Not 0; Institutional Data)",
  "major_dbl_hnrs_ctrm",       "% with High Academic Commitment",                   seq(0, 75, 25),     c(0, 75),      4,             "(Double/Triple Major, Honors, or Co-term; Institutional Data)",
  "major_double",              "% Double or Triple Major",                          seq(0, 50, 10),     c(0, 50),      2.5,           "(Institutional Data)",
  "major_grad_yes_honors",     "% Graduated with Honors",                           seq(0, 50, 10),     c(0, 50),      2.5,           "(Institutional Data)",
  "coterm_ever",               "% Coterm Ever",                                     seq(0, 75, 25),     c(0, 75),      4,             "(Institutional Data)",
  "major_bs_yes",              "% Graduated with a Bachelor of Science",            seq(0, 100, 25),    c(0, 100),     5,             "(Institutional Data)",
  "major_ba_yes",              "% Graduated with a Bachelor of Arts",               seq(0, 100, 25),    c(0, 100),     5,             "(Institutional Data)",
  "major_cs_yes",              "% Graduated in Computer Science",                   seq(0, 50, 10),     c(0, 50),      2.5,           "(Institutional Data)",
  "major_school_engineering",  "% Graduated with an Engineering Major",             seq(0, 75, 25),     c(0, 75),      5,           "(Institutional Data)",
  "major_school_environmental","% Graduated with an Environmental Major",           seq(0, 25, 5),      c(0, 25),      2,             "(Institutional Data)",          
  "major_school_humanitiesscience","% Graduated with a\nHumanities and Science Major",seq(0, 100, 25),    c(0, 100),     5,             "(Institutional Data)",
  "major_culture_2",           "% Graduated with a Cultural Major",                 seq(0, 25, 5),      c(0, 25),      2,             "(Institutional Data)",          
  "major_socialscience",       "% Graduated with a Social Science Major",           seq(0, 50, 10),     c(0, 50),      2.5,           "(Institutional Data)",
  "major_humanities",          "% Graduated with a Humanities Major",               seq(0, 50, 10),     c(0, 50),      2.5,           "(Institutional Data)",
  "major_stem_all",            "% Graduated with a STEM Major",                     seq(0, 100, 25),    c(0, 100),     5,             "(All majors from “Math & Sciences” interest; Institutional Data)",
  "major_stem_hard",           "% Graduated with a STEM Major",                     seq(0, 100, 25),    c(0, 100),     5,             "(Without Biology and Human Biology; Institutional Data)",
  
  # Senior Year Survey
  "tobaccouse_yes",             "% Used Tobacco",                                   seq(0, 50, 10),     c(0, 50),      5,             "(Senior Year Survey)",
  "bingedrink_yes",             "% Binge Drank Alcohol Last Month",                 seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "next_plans2",                "% Taking a Job",                                   seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "next_plans8",                "% Pursuing Graduate Studies Next Year",            seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "srgift_planordonated",       "% Donated or Planned to Donate\nto Senior Gift",   seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "honors_yes_usethis",         "% Reporting Awards or Honors",                     seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "mentor_yes",                 "% with a Mentor",                                  seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "mentor_faculty",             "% with a Faculty Mentor",                          seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "mentor_grad",                "% with a Graduate Student Mentor",                 seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)",
  "mentor_undergrad",           "% with an Undergraduate Mentor",                   seq(0, 100, 25),    c(0, 100),     10,            "(Senior Year Survey)"
)

tribble_cond <- data.frame(matrix(c("treat_sc"), nrow = nrow(tribble_dv)),
                           matrix(c("cond_sc"), nrow = nrow(tribble_dv)))

names_cond <- c("treat_sc", "cond_sc")
colnames(tribble_cond) <- names_cond

tribble_categ <- bind_cols(tribble_dv, tribble_cond)

rm(tribble_dv, tribble_cond)

#### Count variables (Negative Binomial Regressions) ####

vars_count <- 
  quos(
    visits_per_day, 
    visits_per_day_mh, 
    visits_per_day_resp, 
    visits_per_day_no_resp,
    org_acad,
    org_athletics,
    # org_media,
    # org_community,
    org_cult,
    org_comm_cult
    # org_games,
    # org_leader,
    # org_arts,
    # org_religion,
    # org_social,
    # org_other
    ) 

tribble_dv <- tribble(
  ~dv,                          ~dv_name,                                                  ~scale,             ~limit,     ~position,   ~subheader,
  #----------------------------|----------------------------------------------------------|-------------------|-----------|-------------|-----------------------------
  "visits_per_day",             "# of Days Visited Health Center",                         seq(0, 32, 4),      c(0, 32),   4,            "(5 Years; Medical Records)",
  "visits_per_day_mh",          "# of Days Visited Related to\nMental Health",             seq(0, 12, 3),      c(0, 12),   2,            "(5 Years; Medical Records)",
  "visits_per_day_resp",        "# of Days Visited Related to\nRespiratory Illness",       seq(0, 5, 1),       c(0, 5),    1,            "(5 Years; Medical Records)",
  "visits_per_day_no_resp",     "# of Days Visited Related to\n Non-respiratory Illness",  seq(0, 32, 4),      c(0, 32),   4,            "(5 Years; Medical Records)",
  "org_acad",                   "# of Joined Organizations:\nAcademic or Professional",                seq(0, 1, 0.25),      c(0, 1),   0.1,            "(Senior Year Survey)",
  "org_athletics",              "# of Joined Organizations:\nAthletics or Recreation",                seq(0, 1, 0.25),      c(0, 1),   0.1,            "(Senior Year Survey)",
  # "org_media",                  "Organizations:\nAcademic or Professional",                seq(0, 1, 0.25),      c(0, 1),   0.25,            "(Senior Year Survey)",
  # "org_community",              "Organizations:\nAcademic or Professional",                seq(0, 1, 0.25),      c(0, 1),   0.25,            "(Senior Year Survey)",
  "org_cult",                   "# of Joined Organizations:\nCulture",                seq(0, 1, 0.25),      c(0, 1),   0.1,            "(Senior Year Survey)",
  "org_comm_cult",              "# of Joined Organizations:\nCulture, Community, Health,\nEducation, Conservation or Outreach",                seq(0, 1, 0.25),      c(0, 1),   0.1,            "(Senior Year Survey)"
  # "org_games",                  "Organizations:\nAcademic or Professional",                seq(0, 40, 4),      c(0, 40),   4,            "(Senior Year Survey)",
  # "org_leader",                 "Organizations:\nAcademic or Professional",                seq(0, 40, 4),      c(0, 40),   4,            "(Senior Year Survey)",
  # "org_arts",                   "Organizations:\nAcademic or Professional",                seq(0, 40, 4),      c(0, 40),   4,            "(Senior Year Survey)",
  # "org_religion",               "Organizations:\nAcademic or Professional",                seq(0, 40, 4),      c(0, 40),   4,            "(Senior Year Survey)",
  # "org_social",                 "Organizations:\nAcademic or Professional",                seq(0, 40, 4),      c(0, 40),   4,            "(Senior Year Survey)",
  # "org_other",                  "Organizations:\nAcademic or Professional",                seq(0, 40, 4),      c(0, 40),   4,            "(Senior Year Survey)"
)

# visits_per_day, visits_per_day_mh, visits_per_day_resp, visits_per_day_no_resp

tribble_cond <- data.frame(matrix(c("treat_sc"), nrow = nrow(tribble_dv)),
                           matrix(c("cond_sc"), nrow = nrow(tribble_dv)))

names_cond <- c("treat_sc", "cond_sc")
colnames(tribble_cond) <- names_cond

tribble_count <- bind_cols(tribble_dv, tribble_cond)

rm(tribble_dv, tribble_cond)

d$howinv_comp5
