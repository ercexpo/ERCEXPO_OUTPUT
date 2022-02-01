# ==============================================================================
# file name: compliance.R
# authors: Bernhard Clemm 
# purpose: 
# ==============================================================================

# SETUP ========================================================================

# DATA =========================================================================

# SELF-REPORTED COMPLIANCE =====================================================

# us ####



# pl ####

pl_compliance <- read_sav(paste0(datadir, "pl_survey_post.sav")) %>% # replace with recoded data set
  mutate(exp1 = exp1 - 1,
         exp2 = exp2 - 1) %>%
  select(
    # wave2kontrolna_pre, wave2eksperymentalna_pre, # add later
    respondent_id, exp1, exp2, starts_with("exp3q8_h1_r")) %>%
  left_join(., read.csv(paste0(datadir, "person_ids.csv")) %>%
              select(respondent_id, person_id), by = "respondent_id") %>%
  left_join(., read_sav(paste0(datadir, "pl_survey_check1.sav")),
            by = "respondent_id") %>%
  left_join(., read_sav(paste0(datadir, "pl_survey_check2.sav")),
            by = "respondent_id") %>%
  left_join(., read_sav(paste0(datadir, "pl_survey_check3.sav")),
            by = "respondent_id") %>%
  left_join(., read_sav(paste0(datadir, "pl_survey_check4.sav")),
            by = "respondent_id") %>%
  left_join(., read_sav(paste0(datadir, "pl_survey_check5.sav")),
            by = "respondent_id") %>%
  left_join(., read_sav(paste0(datadir, "pl_survey_check6.sav")),
            by = "respondent_id") %>%
  select(person_id, exp1, exp2, starts_with("exp3q8_h1_r"),
         starts_with("y1eksp"), starts_with("y2eksp")) %>%
  mutate(across(c(starts_with("y1eksp"), starts_with("y2eksp")),
                ~ case_when(. == 1 ~ 1, . == 2 ~ 0))) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  mutate(comp_self_all = rowSums(
    select(., c(exp1, exp2, starts_with("exp3q8_h1_r"),
                starts_with("y1eksp"), starts_with("y2eksp"))), na.rm = T),
    comp_self_post = rowSums(
      select(., c(exp1, exp2, starts_with("exp3q8_h1_r"))))) %>%
  select(person_id, comp_self_all, comp_self_post)

# BEHAVIORAL COMPLIANCE ========================================================

# PAP: "we will create an individual-level variable measuring com-pliance:  the percentage increase in the average number of unique news URLs accessedbefore the wave 2 survey, and between the completion of the pre- and post-surveys"

pl_traces <- read.csv(paste0(datadir, "pl_traces_summary.csv"))

pl_compliance <- pl_compliance %>%
  left_join(., pl_traces)

  
  


  

