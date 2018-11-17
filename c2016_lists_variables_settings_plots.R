#### Continuous variables (Linear Regressions) #### 

vars_cont <- 
  quos(
    # Institutional Data
    cum_gpa_y4_spring,
    cum_stem_gpa_y4_spring,
    
    # Pre-Matriculation Intervention
    ## Critical Feedback
    t1critfeedback_improvegrow,
    t1critfeedback_distinguishbtwnSs,
    ## Feelings
    posfeels_comp4,  
    t1posfeelings_profsTAs,  
    t1posfeelings_gettingtoknowotherSs, 
    t1posfeelings_beingawayfromhome, 
    t1posfeelings_receivingfeedback, 
    negfeels_comp4, 
    t1negfeelings_profsTAs, 
    t1negfeelings_gettingtoknowotherSs, 
    t1negfeelings_beingawayfromhome, 
    t1negfeelings_receivingfeedback,
    
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
    buwonder_r_use_this,
    overallpositivity,
    collexp_comp4,
    collhomeinteg_comp4,
    wisefeedbacktask_comp4,
    critf_distinguish_r,
    critf_grow,
    perf_highexpect,
    perf_comp2,
    id_gender,
    id_race,
    qualitiesind_comp3,
    qualitiesinter_2_comp4,
    igm_comp2,
    igmmajor1_2_3_comp3,
    pol_conslib,
    next_excited, 
    next_diffatfirst, 
    donate_srgifttot, 
    donate_srgiftplan, 
    donate_srgift, 
    threat_nogen_comp8,
    cthreat_minorities,
    cthreat_self,
    cthreat_comp2,
    stthreat_gender,
    stthreat_race,
    stthreat_ses,
    famrelate_comp2,
    integ_idgender,
    integ_idrace,
    integ_idclass
  ) 

tribble_dv <- tribble(
  ~dv,                            ~dv_name,                                              ~scale,      ~limit,         ~position,      ~subheader,                                            ~decimal,
  #------------------------------|------------------------------------------------------|--------------|--------
  # Institutional data
  "cum_gpa_y4_spring",            "4-Year Cumulative GPA",                               seq(0, 4, 1), c(0, 4),        0.5,            "(Institutional Data)",                                3,
  "cum_stem_gpa_y4_spring",       "4-Year Cumulative GPA in STEM Classes",               seq(0, 4, 1), c(0, 4),        0.5,            "(Institutional Data)",                                3,
  
  # Pre-Matriculation Intervention
  "t1critfeedback_improvegrow",   "Crit. Feedback: Help Ss Grow",                        seq(1, 7, 1), c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1critfeedback_distinguishbtwnSs", "Critical Feedback: Distinguish Btwn Students",    seq(1, 7, 1), c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "posfeels_comp4" , "Overall Positivity of College Experience",                         seq(1, 7, 1), c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1posfeelings_profsTAs", "Positive Experiences w/ Profs. and TAs",                    seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1posfeelings_gettingtoknowotherSs", "Positive Feelings Getting to Know Other Students", seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1posfeelings_beingawayfromhome", "Positive Feelings Being Away from Home",           seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1posfeelings_receivingfeedback", "Positive Feelings About Receiving Feedback",        seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "negfeels_comp4", "Overall Negativity of College Experience",                           seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1negfeelings_profsTAs", "Negative Experiences w/ Profs. and TAs",                     seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1negfeelings_gettingtoknowotherSs","Negative Feelings Getting to Know Other Students",seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1negfeelings_beingawayfromhome", "Negative Feelings Being Away from Home",            seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  "t1negfeelings_receivingfeedback", "Negative Feelings Receiving Feedback",              seq(1, 7, 1),
  c(1, 7),        2,              "(Pre-Matriculation Intervention)",                    3,
  
  
  # Senior Year Survey
  "success_usethis_percent",      "Success: Percentile Ranking",                         seq(0, 100, 10), 
  c(0, 100),      10,             "(Senior Year Survey)",                                4,       
  "potential_usethis_percent",    "Potential: Percentile Ranking",                       seq(0, 100, 10), 
  c(0, 100),      10,             "(Senior Year Survey)",                                4,
  
  # Health
  "healthmosoverall_comp5",       "Composite:\nSelf-Reported General Health",            seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "health_behavbingedrink",       "Frequency of Binge Drinking",                         seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "health_behavtobacco",          "Tobacco Use During the Last Month",                   seq(0, 3, 1),
  c(0, 3),        0.5,            "(Senior Year Survey)",                                3,
  "health_behavsleepiness",       "Daytime Sleepiness During the Last Month",            seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "stressdaily",                  "How Stressed",                                        seq(1, 10, 1),
  c(1, 10),       2,              "(Primary Appraisal; Senior Year Survey)",             3,
  "pss_comp4",                    "Composite:\nHow Overwhelmed by Stress",               seq(1, 5, 1),
  c(1, 5),        1.5,            "(Secondary Appraisal; Senior Year Survey)",           3,
  "health_behavrelax",            "Time to Relax",                                       seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "mhealthgeneral",               "Overall Mental Health",                               seq(1, 5, 1),
  c(1, 5),        1.5,            "(Single Item; Senior Year Survey)",                   3,
  "anxscreener_comp2",            "Composite:\nAnxiety Screener",                        seq(0, 3, 1),
  c(0, 3),        0.5,            "(Senior Year Survey)",                                3,
  "depscreener_comp2",            "Composite:\nDepression Screener",                     seq(0, 3, 1),
  c(0, 3),        0.5,            "(Senior Year Survey)",                                3,
  "overallmentalhealth_comp5_z",  "Composite:\nOverall Mental Health",                   seq(-1, 1, 0.5),
  c(-1, 1),       -0.75,          "(Average Standardized Score; Senior Year Survey)",    1,
  "anxdepscreener_comp4",         "Composite:\nAnxiety and Depression Screeners",        seq(0, 3, 1),
  c(0, 3),        0.5,            "(4 Items; Senior Year Survey)",                       3,
  "bmi",                          "Body Mass Index",                                     seq(0, 40, 10),
  c(0, 40),       5,              "(BMI, Senior Year Survey)",                           4,
  "bmi_chge_scr",                 "BMI Change Score",                                    seq(0, 3, 1),
  c(0, 3),        0.5,            "(Senior Year Survey)",                                3,
  "weight",                       "4th-year Weight",                                     seq(0, 200, 50),
  c(0, 200),      25,             "(BMI, Senior Year Survey)",                           5,
  "weight_comp3",                 "Total Weight Satisfaction",                           seq(1, 7, 1),
  c(1, 7),        2,              "(BMI, Senior Year Survey)",                           3,
  "weight_happy_rc",              "Happiness with Weight",                               seq(1, 7, 1),
  c(1, 7),        2,               "(BMI, Senior Year Survey)",                          3,
  "weight_feelbad",               "Feel Less Bad About Weight",                          seq(1, 7, 1),
  c(1, 7),        2,               "(BMI, Senior Year Survey)",                          3,
  "weight_trylose",               "Trying to Lose Weight",                               seq(1, 7, 1),
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  
  
  
  "happy_comp4",                  "Composite:\nHappiness",                               seq(1, 7, 1),
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  "purpose_comp2",                "Composite:\nPurpose & Meaning",                       seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  
  "lifesat",                      "Life Satisfaction",                                   seq(1, 10, 1),
  c(1, 10),       2.5,            "(Senior Year Survey)",                                3,
  
  "belong_comp6",                 "Composite:\nBelonging and College Experience",        seq(1, 7, 1),
  c(1, 7),        2,              "(6 Items; Senior Year Survey)",                       3,
  "belong_comp4",                 "Composite:\nBelonging",                               seq(1, 7, 1),
  c(1, 7),        2,              "(4 Items; Senior Year Survey)",                       3,
  "buwonder_r_use_this",          "Belonging Uncertainty",                               seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "overallpositivity",            "Overall Positivity of College Experience",            seq(1, 10, 1),
  c(1, 10),     2.5,              "(Senior Year Survey)",                                3,
  "collexp_comp4",                "Composite:\nPositive Evaluations",                    seq(1, 7, 1),
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  "collhomeinteg_comp4",          "Composite:\nCollege-Home Integration",                seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "threat_nogen_comp8",           "Threat Composite",                                    seq(-1, 1, 0.5),
  c(-1, 1),     -0.75,            "(8 Items; Senior Year Survey)",                       2,
  "cthreat_minorities",           "Contextual Threat:\nOthers",                          seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "cthreat_self",                 "Contextual Threat:\nSelf",                            seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "cthreat_comp2",                "Contextual Threat:\nCombined",                        seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "stthreat_gender",              "Stereotype Threat: Gender",                           seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3, 
  "stthreat_race",                "Stereotype Threat: Race",                             seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3,
  "stthreat_ses",                 "Stereotype Threat: Social Class",                     seq(1, 5, 1),
  c(1, 5),        1.5,            "(Senior Year Survey)",                                3, 
  "famrelate_comp2",              "Home-School Disconnect:\nFamily-College Disconnect",  seq(1, 6, 1),
  c(1, 6),        2,              "(Senior Year Survey)",                                3,
  "integ_idgender",               "College-Identity Friction:\nGender",                  seq(0, 3, 1),
  c(0, 3),       0.5,             "(Senior Year Survey)",                                3,
  "integ_idrace",                 "College-Identity Friction:\nRace",                    seq(0, 3, 1),
  c(0, 3),       0.5,             "(Senior Year Survey)",                                3, 
  "integ_idclass",                "College-Identity Friction:\nSocial Class",            seq(0, 3, 1),
  c(0, 3),       0.5,             "(Senior Year Survey)",                                3,
  "wisefeedbacktask_comp4",       "Wise Feedback Task",                                  seq(1, 5, 1),
  c(1, 5),       1.5,              "(Senior Year Survey)",                               3,
  "critf_distinguish_r",          "Critical Feedback:\nDistinguish Between Students",    seq(1, 7, 1),
  c(1, 7),        2,              "(Senior Year Survey)",                                3, 
  "critf_grow",                   "Critical Feedback:\nHelp Students Grow",              seq(1, 7, 1), 
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  "perf_highexpect",              "Perfectionism:\nStandards",                           seq(1, 7, 1),
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  "perf_comp2",                   "Perfectionism:\nNever Satisfied",                     seq(1, 7, 1), 
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  "id_gender",                    "Identification:\nGender",                             seq(1, 5, 1),
  c(1, 5),      1.5,              "(Senior Year Survey)",                                3, 
  "id_race",                      "Identification:\nRace",                               seq(1, 5, 1),
  c(1, 5),      1.5,              "(Senior Year Survey)",                                3,
  "qualitiesind_comp3",           "Independence Valued",                                 seq(1, 5, 1),
  c(1, 5),      1.5,              "(Senior Year Survey)",                                3,
  "qualitiesinter_2_comp4",       "Interdependence Valued",                              seq(1, 5, 1),
  c(1, 5),      1.5,              "(Senior Year Survey)",                                3,
  "igm_comp2",                    "Institutional Mindsets:\nGeneral",                    seq(1, 6, 1),
  c(1, 6),        2,              "(Senior Year Survey)",                                3,
  "igmmajor1_2_3_comp3",          "Institutional Mindsets:\nMajor",                      seq(1, 6, 1),
  c(1, 6),        2,              "(Senior Year Survey)",                                3,
  "pol_conslib",                  "Politics:\nConservative-Liberal",                     seq(1, 7, 1),
  c(1, 6),        2,              "(Senior Year Survey)",                                3,
  "next_excited",                  "Excitement for Post-college Plans",                   seq(1, 7, 1),
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  "next_diffatfirst",               "Next Plans: Difficult at First",                      seq(1, 7, 1), 
  c(1, 7),        2,              "(Senior Year Survey)",                                3,
  "donate_srgifttot",             "Amount of Money Donated",                             seq(-10, 80, 10), 
  c(-10, 80),     0,              "(Senior Year Survey)",                                4,
  "donate_srgiftplan",             "Plan to Donate",                                      seq(-10, 80, 10), 
  c(-10, 80),     10,             "(Senior Year Survey)",                                4,
  "donate_srgift",                 "Amount of Money Donated",                             seq(-10, 80, 10), 
  c(-10, 80),     0,              "(Senior Year Survey)",                                4,
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
    class_research_ever,
    quintile_bottom,
    major_dbl_hnrs_ctrm,
    honors_yes_usethis,
    
    # Pre-Matriculation Intervention
    mc_wf,
    
    # Senior Year Survey
    # major2_yes, # we need to recode this variable
    tobaccouse_yes,
    bingedrink_yes,
    next_plans2,
    next_plans8,
    srgift_planordonated
  )

tribble_dv <- tribble(
  ~dv,                        ~dv_name,                                             ~scale,       ~limit,     ~position,
  ~subheader,
  #---------------------------|---------------------------------------|-------------------|--------
  # Institutional data
  "major_grad_4y",             "% Graduating in Four Years",                        seq(0, 100, 20),    c(0, 100),  10,
  "(Institutional Data)",
  "class_research_ever",       "% Involved in Research",                            seq(0, 100, 20),    c(0, 100),  10,
  "(Institutional Data)",
  "quintile_bottom",           "% with GPA in Bottom Quintile of Class",            seq(0, 100, 20),  c(0, 100),  5,
  "(Institutional Data)",
  "major_dbl_hnrs_ctrm",       "% with High Academic Commitment",                   seq(0, 100, 20),    c(0, 100),  10,
  "(Double/Triple Major, Honors, or Co-term; Institutional Data)",
  "honors_yes_usethis",        "% Reporting Awards or Honors",                      seq(0, 100, 20),    c(0, 100),  10,
  "(Institutional Data)",
  
  # Senior Year Survey
  "tobaccouse_yes",             "% Used Tobacco",                                   seq(0, 50, 10),     c(0, 50),   5,
  "(Senior Year Survey)",
  "bingedrink_yes",             "% Drank Alcohol",                                  seq(0, 100, 20),    c(0, 100),  10,
  "(Senior Year Survey)",
  "next_plans2",                "% Taking a Job",                                   seq(0, 100, 20),    c(0, 100),  10,
  "(Senior Year Survey)",
  "next_plans8",                "% Pursuing Graduate Studies Next Year",            seq(0, 100, 20),    c(0, 100),  10,
  "(Senior Year Survey)",
  "srgift_planordonated",       "% Donated or Planned to Donate\nto Senior Gift",       seq(0, 100, 20),    c(0, 100),  10,
  "(Senior Year Survey)"
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
    visits_per_day_no_resp
    ) 

tribble_dv <- tribble(
  ~dv,                          ~dv_name,                                       ~scale,             ~limit,  ~position, ~subheader,
  #----------------------------|-----------------------------------------------|-------------------|--------|-----------
  "visits_per_day",            "# of Days Visited Health Center",    seq(0, 20, 2),     c(0, 20), 2, "(5 Years; Medical Records)",
  "visits_per_day_mh",         "# of Days Visited Related to\nMental Health",seq(0, 8, 1),      c(0, 8),  0.5, "(5 Years; Medical Records)",
  "visits_per_day_resp",       "# of Days Visited Related to\nRespiratory Illness",seq(0, 3, 0.5),      c(0, 3),  0.3, "(5 Years; Medical Records)",
  "visits_per_day_no_resp",    "# of Days Visited Related to\n Non-respiratory Illness",seq(0, 18, 3),     c(0, 18), 1, "(5 Years; Medical Records)"
)

# visits_per_day, visits_per_day_mh, visits_per_day_resp, visits_per_day_no_resp

tribble_cond <- data.frame(matrix(c("treat_sc"), nrow = nrow(tribble_dv)),
                           matrix(c("cond_sc"), nrow = nrow(tribble_dv)))

names_cond <- c("treat_sc", "cond_sc")
colnames(tribble_cond) <- names_cond

tribble_count <- bind_cols(tribble_dv, tribble_cond)

rm(tribble_dv, tribble_cond)