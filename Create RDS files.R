#############################################################
##
## Project: Literature Review - Unmeasured Confounders
## Programmer: Chase
##
## Purpose: Create RMD files from the Excel files where
## data are recorded by reviewers.
##
############################################################



###########################################################
##
## Load packages.
##
###########################################################

# Needed for piping
library(tidyverse)

# Needed for reading the Excel files.
library(readxl)

# Needed for writing the Excel files
library(writexl)

###########################################################
##
## Upload the datasets and clean them.
##
###########################################################

## First upload the Excel sheet with the articles
## from the medical journals

medical <- as_tibble(read_excel("/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Task 1/Lit Review Material/Search #2 - Manuscripts/Sensitivity Analyses - 2020-22_Second Review ONLY_ALL.xlsx", sheet = 1)) %>%
  mutate(journal_type = "med",
         # Fix a typo
         journal = ifelse(journal == "Annals of Internal Medicine", "Annals of Internal Med", journal),
         # Combine the e-value and e-value precursor (Ding & Vanderweele 2016)
         eval_ind = ifelse(pre_eval_ind == 1, 1, eval_ind),
         # Combine the cca in a linked and non-linked dataset
         cca_2_ind = ifelse(cca_ind == 1 | linked_dataset_ind == 1,
                            1,
                            0),
         # Make the indicator variable for if a specific analysis
         cca_2_specific = ifelse(cca_specific == 1 | linked_dataset_ind == 1,
                                 1,
                                 0)
         ) %>% 
  # Select the variables that we want
  select(c(title, include, reviewer, second_reviewer, authors, journal_type, journal, year,
           sens_analyses_list, 
           ends_with("_ind"), ends_with("_info"), ends_with("_specific"), conf_limitation_quote)) %>% 
  # Exclude those that we don't want from those selected
  select(-c(matching_ind, matching_info, tmle_ind, tmle_info, g_form_ind, g_form_info,
            ccw_ind, ccw_info, outcome_adj_ind, strat_anal_ind, explicit_conf_sens_anal_ind,
            sens_anal_info, unnamed_conf_analysis_ind, unnamed_conf_analysis_info,
            specific_confounder_analysis_ind,
            pseudo_treat_info, pseudo_treat_specific,
            parial_id_ind, partial_id_info, partial_id_specific,
            ros_rub_ind, ros_rub_info, ros_rub_specific, rosenbaum_ind, rosenbaum_info,
            rosenbaum_specific, twin_reg_ind, twin_reg_info, twin_reg_specific,
            reg_diss_ind, reg_diss_info, did_ind, did_info, missing_cause_ind, missing_cause_info,
            trend_in_trend_ind, trend_in_trend_info, perturbation_ind, perturbation_info,
            adj_additional_var_ind, adj_additional_var_info)) %>% 
  # Removed these variables because not the same type across the datasets but not needed
  subset(include == 1) %>% 
  rowwise() %>% 
  # Make some summary variables
  mutate(sum_sensitivity_anal = sum(cca_2_ind, restrict_ind, ps_trim_ind,
                                    equipoise_ind, pos_cntrl_ind, neg_cntrl_outcome_ind,
                                    neg_cntrl_exposure_ind, iv_ind, mi_ind, psc_ind,
                                    dist_calibration_ind, eval_ind, rule_out_ind,
                                    array_ind, lin_psaty_kronmal_ind, lin_psaty_kronmal_ps_ind,
                                    qba_ind, pba_ind, cov_balance_ind,
                                    na.rm=TRUE),
         gt1_sens_anal = ifelse(sum_sensitivity_anal > 1,
                            1,
                            0),
         ge1_sens_anal = ifelse(sum_sensitivity_anal > 0,
                            1,
                            0)
         ) %>% 
  ungroup()



## First upload the Excel sheet with the articles
## from the epidemiology journals

epi <- as_tibble(read_excel("/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Task 1/Lit Review Material/Search #2 - Manuscripts/Sensitivity Analyses - 2020-22_Second Review ONLY_ALL.xlsx", sheet = 2)) %>%
  mutate(journal_type = "epi",
         journal = ifelse(journal == "International Journal of Epidemiology ",
                          "International Journal of Epidemiology",
                          journal),
         journal = ifelse(journal == "Pharmacoepi and drug safety",
                          "Pharmacoepidemiology and Drug Safety",
                          journal),
         eval_ind = ifelse(pre_eval_ind == 1, 1, eval_ind),
         # Combine the cca in a linked and non-linked dataset
         cca_2_ind = ifelse(cca_ind == 1 | linked_dataset_ind == 1,
                            1,
                            0),
         #Make the indicator variable for if it's a specific analysis
         cca_2_specific = ifelse(cca_specific == 1 | linked_dataset_ind == 1,
                                 1,
                                 0)) %>% 
  # Select the variables that we want
  select(c(title, include, reviewer, second_reviewer, authors, journal_type, journal, year,
           sens_analyses_list, 
           ends_with("_ind"), ends_with("_info"), ends_with("_specific"), conf_limitation_quote)) %>% 
  # Exclude those that we don't want from those selected
  select(-c(matching_ind, matching_info, tmle_ind, tmle_info, g_form_ind, g_form_info,
            ccw_ind, ccw_info, outcome_adj_ind, strat_anal_ind, explicit_conf_sens_anal_ind,
            sens_anal_info, unnamed_conf_analysis_ind, unnamed_conf_analysis_info,
            specific_confounder_analysis_ind,
            pseudo_treat_info, pseudo_treat_specific,
            parial_id_ind, partial_id_info, partial_id_specific,
            ros_rub_ind, ros_rub_info, ros_rub_specific, rosenbaum_ind, rosenbaum_info,
            rosenbaum_specific, twin_reg_ind, twin_reg_info, twin_reg_specific,
            reg_diss_ind, reg_diss_info, did_ind, did_info, missing_cause_ind, missing_cause_info,
            trend_in_trend_ind, trend_in_trend_info, perturbation_ind, perturbation_info)) %>% 
  subset(include == 1) %>% 
  rowwise() %>%
  # Make some summary variables
  mutate(sum_sensitivity_anal = sum(cca_2_ind, restrict_ind, ps_trim_ind,
                                    equipoise_ind, pos_cntrl_ind, neg_cntrl_outcome_ind,
                                    neg_cntrl_exposure_ind, iv_ind, mi_ind, psc_ind,
                                    dist_calibration_ind, eval_ind, rule_out_ind,
                                    array_ind, lin_psaty_kronmal_ind, lin_psaty_kronmal_ps_ind,
                                    qba_ind, pba_ind, cov_balance_ind,
                                    na.rm=TRUE),
         gt1_sens_anal = ifelse(sum_sensitivity_anal > 1,
                                1,
                                0),
         ge1_sens_anal = ifelse(sum_sensitivity_anal > 0,
                                1,
                                0)
  ) %>% 
  ungroup()


###########################################################
##
## Stack the two files on top of each other.
##
###########################################################

combined <- bind_rows(list(medical, epi))


###########################################################
##
## Export the dataset as a RMD file for Upload to GitHub.
##
###########################################################

saveRDS(combined,
        file = "/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Manuscripts/Literature Review/combined_data.RDS")

#View(combined)


###########################################################
##
## Create columns that we're going to use for revise
## and resubmit analyses.
##
###########################################################

combined2 <- combined %>%
  mutate(linked_dataset_assump_ind = "",
         linked_dataset_assump = "",
         cca_assump_ind = "",
         cca_assump = "",
         restrict_assump_ind = "",
         restrict_assump = "",
         ps_trim_assump_ind = "",
         ps_trim_assump = "",
         equipoise_assump_ind = "",
         equipoise_assump = "",
         pos_cntrl_assump_ind = "",
         pos_cntrl_assump = "",
         neg_cntrl_assump_ind = "",
         neg_cntrl_assump = "",
         iv_assump_ind = "",
         iv_assump = "",
         mi_assump_ind = "",
         mi_assump = "",
         psc_assump_ind = "",
         psc_assump = "",
         dist_calibration_assump_ind = "",
         dist_calibration_assump = "",
         eval_assump_ind = "",
         eval_assump = "",
         rule_out_assump_ind = "",
         rule_out_assump = "",
         array_assump_ind = "",
         array_assump = "",
         lin_psaty_kronmal_assump_ind = "",
         lin_psaty_kronmal_assump = "",
         qba_assump_ind = "",
         qba_assump = "",
         pba_assump_ind = "",
         pba_assump = "",
         cov_balance_assump_ind = "",
         cov_balance_assump = "",
         num_primary_confounders = "",
         when_measured_confounders = "",
         baseline_trt_confounders = "",
         baseline_outcome_confounders = "") 

# Output an Excel file with these columns.
write_xlsx(combined2,
           "/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Manuscripts/Literature Review/combined_data_RandR.xlsx")



###########################################################
##
## Upload the new combined Excel sheet and make it into
## an RDS file. These edits were made manually.
##
###########################################################

## Upload the revised Excel file for R&R.
combined_randr <- read_xlsx("/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Manuscripts/Literature Review/combined_data_RandR.xlsx") %>% 
  #Filter out those that were marked as 'do not include' after first submission
  subset(include == 1)

# Save it as an RDS file
saveRDS(combined_randr,
        file = "/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Manuscripts/Literature Review/combined_data_RandR.RDS")

#View(combined)













