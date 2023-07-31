#############################################################
##
## Project: Literature Review - Unmeasured Confounders
## Programmer: Chase
##
## Purpose: Analyze the literature review data.
##
############################################################



###########################################################
##
## Load packages.
##
###########################################################

library(tidyverse)


###########################################################
##
## Conduct calculations used throughout the results.
##
###########################################################

## Count the number of articles identified by journal type (med, epi)
## and year (2018-2019 and 2020-2022)
combined %>% 
  mutate(batch = ifelse(year >= 2017 & year <= 2019 , 
                        "2017-2019", 
                        "2020-2022")) %>% 
  group_by(batch, journal_type) %>% 
  summarize(sum = n(),
            .groups = "keep")

## Count the number and proportion of articles that used PSs in the primary analysis.
combined %>% 
  summarize(ps = sum(ps_ind),
            ps_percent = ps / n() * 100)

## Count the number of articles that conducted ≥1 sensitivity analysis of-interest
## by journal type
combined %>% 
  group_by(journal_type) %>% 
  summarize(atleast_1 = sum(ge1_sens_anal))


## Count the number of articles with more than 1 sens analysis of interest
combined %>% 
  group_by(journal_type) %>% 
  summarize(gt1 = sum(gt1_sens_anal))

## Count the number that identified residual confounding as a limitation
## By journal type
combined %>% 
  group_by(journal_type) %>%
  summarize(conf_limit = sum(conf_limitation_ind),
            total = n(),
            percentage = conf_limit/total * 100)


## Count the number that conducted an sensitivity analysis for a known
## unmeasured confounder by journal type

combined %>% 
  group_by(journal_type) %>% 
  summarize(
    n_anal = sum(ge1_sens_anal),
    n_anal_specific = sum(specific_confounder_analysis_ind),
    percent = n_anal_specific/n_anal * 100
  )


###########################################################
##
## Figure 2 analyses
##
###########################################################

## Count the number of included articles by journal
combined %>% 
  group_by(journal) %>% 
  summarize(total = n())

## Count the number of articles that listed residual or uncontroll
## confounding as a potential limitation of their study.
combined %>% 
  group_by(journal) %>% 
  summarize(sum = sum(conf_limitation_ind),
            .groups = "keep")

## Count the number of articles that conducted at least one sens analysis
## by journal
combined %>% 
  group_by(journal_type, journal) %>% 
  summarize(atleast_1 = sum(ge1_sens_anal),
            total = n(),
            percent = atleast_1 / total * 100,
            .groups = "keep")



###########################################################
##
## Table 2 analyses
## Number of each sensitivity analysis by journal type and
## whether it addressed concern from a known unmeasured
## confounder.
##
###########################################################

table2 <- combined %>% 
  group_by(journal_type) %>% 
  summarize(linked_dataset = sum(linked_dataset_ind),
            ld_specific = sum(linked_dataset_specific),
            cca = sum(cca_ind),
            cca_specific = sum(cca_specific),
            restrict = sum(restrict_ind),
            restrict_specific = sum(restrict_specific),
            ps_trim = sum(ps_trim_ind),
            trim_specific = sum(ps_trim_specific),
            equipoise = sum(equipoise_ind, na.rm = TRUE),
            equipoise_s = sum(equipoise_specific),
            pos_cntrl = sum(pos_cntrl_ind),
            pos_s = sum(pos_cntrl_specific),
            n_cntrl = sum(neg_cntrl_ind),
            n_cntrl_outcome = sum(neg_cntrl_outcome_ind),
            n_cntrl_exposure = sum(neg_cntrl_exposure_ind),
            n_cntrl_s = sum(neg_cntrl_specific),
            iv = sum(iv_ind),
            iv_s = sum(iv_specific),
            mi = sum(mi_ind),
            mi_s = sum(mi_specific),
            psc = sum(psc_ind),
            psc_s = sum(psc_specific),
            dist_c = sum(dist_calibration_ind),
            dist_s = sum(dist_calibration_specific),
            e_value = sum(eval_ind),
            e_value_s = sum(eval_specific),
            p_e_value = sum(pre_eval_ind, na.rm = TRUE),
            p_e_value_s = sum(pre_eval_specific, na.rm = TRUE),
            rule_out = sum(rule_out_ind),
            rule_out_s = sum(rule_out_specific),
            array = sum(array_ind),
            array_s = sum(array_specific),
            lpk = sum(lin_psaty_kronmal_ind),
            lpk_s = sum(lin_psaty_kronmal_specific),
            lpk_ps = sum(lin_psaty_kronmal_ps_ind),
            lpk_ps_s = sum(lin_psaty_kronmal_ps_specific),
            qba = sum(qba_ind, na.rm=TRUE),
            qba_s = sum(qba_specific, na.rm=TRUE),
            pba = sum(pba_ind),
            pba_s = sum(pba_specific),
            cov_bal = sum(cov_balance_ind, na.rm = TRUE),
            cov_bal_s = sum(cov_balance_specific, na.rm = TRUE)
            )

## Look at the table for transferring to the Manuscript
View(table2)

## Look at negative control outcomes and exposures separately

## Look at the number of each done for specific analyses

#### Negative control outcome
combined %>% 
  subset(neg_cntrl_outcome_ind == 1) %>% 
  group_by(journal_type) %>% 
  summarize(total = sum(neg_cntrl_outcome_ind),
            specific = sum(neg_cntrl_specific))

#### Negative control exposure
combined %>% 
  subset(neg_cntrl_exposure_ind == 1) %>% 
  group_by(journal_type) %>% 
  summarize(total = sum(neg_cntrl_exposure_ind),
            percent = round(total / n() * 100),
            specific = sum(neg_cntrl_specific))



## Determine which article(s) implemented each analysis so that 
## Table 2 can include appropriate references

# Negative control - overall
n_cntrl <- subset(combined, neg_cntrl_ind == 1)
View(n_cntrl)
# Negative control - outcome
n_cntrl_outcome <- subset(combined, neg_cntrl_outcome_ind == 1)
View(n_cntrl_outcome)
# negative control - Exposure
n_cntrl_exposure <- subset(combined, neg_cntrl_exposure_ind == 1)
View(n_cntrl_exposure)
# Restriction
restrict <- subset(combined, restrict_ind == 1) %>% 
  select(title,journal, year, authors)
View(restrict)
# E-value
e_val <- subset(combined, eval_ind == 1 | pre_eval_ind == 1) %>% 
  select(title,journal, year, authors)
View(e_val)
# PS Trimming
trim <- subset(combined, ps_trim_ind == 1) %>% 
  select(title,journal, year, authors)
View(trim)
# Cov Balance
balance <- subset(combined, cov_balance_ind == 1) %>% 
  select(title,journal, year, authors)
View(balance)
# QBA
qba <- subset(combined, qba_ind == 1) %>% 
  select(title,journal, year, authors)
View(qba)
# Complete case analysis
cca <- subset(combined, cca_ind == 1) %>% 
  select(title,journal, year, authors)
View(cca)
# Empirical distribution calibration
cal <- subset(combined, dist_calibration_ind == 1) %>% 
  select(title,journal, year, authors)
View(cal)
# Rule-out approach
rule_out <- subset(combined, rule_out_ind == 1) %>% 
  select(title,journal, year, authors)
View(rule_out)
# Linked Dataset
linked_dataset <- subset(combined, linked_dataset_ind == 1) %>% 
  select(title,journal, year, authors)
View(linked_dataset)
# Positive Control
pos_cntrl <- subset(combined, pos_cntrl_ind == 1) %>% 
  select(title,journal, year, authors)
View(pos_cntrl)
# Empirical Equipoise
equipoise <- subset(combined, equipoise_ind == 1) %>% 
  select(title,journal, year, authors)
View(equipoise)
# Array Approach
array <- subset(combined, array_ind == 1) %>% 
  select(title,journal, year, authors)
View(array)
# Lin, Psaty, Kronmal Approach
lpk <- subset(combined, lin_psaty_kronmal_ind == 1) %>% 
  select(title,journal, year, authors)
View(lpk)
# Instrumental Variable
iv <- subset(combined, iv_ind == 1) %>% 
  select(title,journal, year, authors)
View(iv)
# Probabilistic Bias Analysis
pba <- subset(combined, pba_ind == 1) %>% 
  select(title,journal, year, authors)
View(pba)
# Propensity Score Calibration
psc <- subset(combined, psc_ind == 1) %>% 
  select(title,journal, year, authors)
View(psc)
# Lin, Psaty, Kronmal w PSs
lpk_ps <- subset(combined, lin_psaty_kronmal_ps_ind == 1) %>% 
  select(title,journal, year, authors)
View(lpk_ps)
# Multiple imputation
mi <- subset(combined, mi_ind == 1) %>% 
  select(title,journal, year, authors)
View(mi)



###########################################################
##
## Supplemental Table 1
## Identify those medical journal articles that implement 
## more than one sensitivity analysis of-interest. 
## Determine which analyses are conducted.
##
###########################################################

## Medical journal articles

med_gt1 <- subset(combined,
                  journal_type == "med" & 
                    gt1_sens_anal == 1)

med_gt1_2 <- med_gt1 %>% 
  select(journal, authors, year, gt1_sens_anal, ends_with("_ind")) 

View(med_gt1_2)

## Epi journal sens analyses when >1

epi_gt1 <- subset(combined,
                  journal_type == "epi" & 
                    gt1_sens_anal == 1)

epi_gt1_2 <- epi_gt1 %>% 
  select(journal, authors, year, gt1_sens_anal, ends_with("_ind"))

View(epi_gt1_2)


###########################################################
##
## Supplemental Table 3
## Output the summary counts of methods by type of
## information required to conduct the analysis.
##
###########################################################


#(1) Researcher-specified bias parameters
combined %>% 
  group_by(journal_type) %>% 
  summarize(rule_out = sum(rule_out_ind),
            array = sum(array_ind),
            lpk = sum(lin_psaty_kronmal_ind),
            qba = sum(qba_ind, na.rm=TRUE),
            pba = sum(pba_ind))

# Gather percentage of articles that implemented at least one of these
combined %>% 
  mutate(ge1 = ifelse(rule_out_ind == 1 | array_ind == 1 | lin_psaty_kronmal_ind == 1 | qba_ind == 1 | pba_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ge1, na.rm = T),
            percent = sum / n() * 100)

#(2) Rule for restricting the primary study population
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    restrict = sum(restrict_ind),
    ps_trim = sum(ps_trim_ind),
    equipoise = sum(equipoise_ind, na.rm = TRUE),
    cca = sum(cca_ind)
  )

#(3) Individual-level patient data for variables a variable that was not of-interest in the primary analysis (e.g., different outcome)
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    neg_outcome = sum(neg_cntrl_outcome_ind),
    neg_exposure = sum(neg_cntrl_exposure_ind),
    pos = sum(pos_cntrl_ind),
    dist_cal = sum(dist_calibration_ind),
    cov_balance = sum(cov_balance_ind),
    iv = sum(iv_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  mutate(ge1_ind = ifelse(neg_cntrl_ind == 1 | pos_cntrl_ind == 1 | dist_calibration_ind == 1 | cov_balance_ind == 1,
                          1,
                          0)) %>% 
  group_by(journal_type) %>% 
  summarize(total = sum(ge1_ind),
            prop = total / n())

# (4) Validation study or sub-sample with data on exposures and confounders; Does not require individual outcome data
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    cov_balance = sum(cov_balance_ind),
    psc = sum(psc_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  mutate(ind = ifelse(cov_balance_ind == 1 | psc_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ind, na.rm = T),
            percent = sum / n() * 100)


# (5) Validation study or sub-sample with data on exposures and confounders; Requires individual outcome data 
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    cca = sum(cca_ind),
    linked = sum(linked_dataset_ind),
    cov_balance = sum(cov_balance_ind),
    lpk_ps = sum(lin_psaty_kronmal_ps_ind),
    qba = sum(qba_ind, na.rm=T),
    pba = sum(pba_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  mutate(ind = ifelse(cca_ind == 1 | linked_dataset_ind == 1 | cov_balance_ind == 1 | 
                        lin_psaty_kronmal_ps_ind == 1 | qba_ind == 1 | pba_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ind, na.rm=T),
            percent = sum(ind, na.rm = T) / n() * 100)


###########################################################
##
## Supplemental Table 4
## Output the summary counts of methods by type of
## metric output to assess uncontrolled confounding by an
## unmeasured variable.
##
###########################################################


# (1) Indicator value for researchers to assess potential role of residual confounding
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    dist_cal = sum(dist_calibration_ind),
    equipoise = sum(equipoise_ind, na.rm = TRUE),
    eval = sum(eval_ind) + sum(pre_eval_ind),
    rule_out = sum(rule_out_ind),
    array = sum(array_ind),
    lpk = sum(lin_psaty_kronmal_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  mutate(ind = ifelse(dist_calibration_ind == 1 | equipoise_ind == 1 | eval_ind == 1 |
                        rule_out_ind == 1 | array_ind == 1 | lin_psaty_kronmal_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ind, na.rm = T),
            percent = sum / n() * 100)

# (2) Investigator assessment of comparator group equipoise through covariate balance 
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    cov_balance = sum(cov_balance_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(cov_balance_ind, na.rm = T),
            percent = sum / n() * 100)

# (3) New study estimate that can be used by the investigator to understand residual confounding 
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    neg_outcome = sum(neg_cntrl_outcome_ind),
    neg_exposure = sum(neg_cntrl_exposure_ind),
    pos_control = sum(pos_cntrl_ind),
    restrict = sum(restrict_ind),
    ps_trim = sum(ps_trim_ind),
    equipoise = sum(equipoise_ind, na.rm=T),
    cca = sum(cca_ind),
    linked = sum(linked_dataset_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  mutate(ind = ifelse(neg_cntrl_ind == 1 | pos_cntrl_ind == 1 | restrict_ind == 1 |
                        ps_trim_ind == 1 | equipoise_ind == 1 | cca_ind == 1 |
                        linked_dataset_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ind, na.rm = T),
            percent = sum / n() * 100)

# (4) Original study estimate “corrected” for confounding by the unmeasured variable
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    lpk_ps = sum(lin_psaty_kronmal_ps_ind),
    qba = sum(qba_ind, na.rm=T),
    pba = sum(pba_ind),
    iv = sum(iv_ind),
    psc = sum(psc_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  mutate(ind = ifelse(lin_psaty_kronmal_ps_ind == 1 | qba_ind == 1 | pba_ind == 1 |
                        iv_ind == 1 | psc_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ind, na.rm = T),
            percent = sum / n() * 100)




###########################################################
##
## Sub-Analysis
## Exclude E-values as a qualifying sensitivity analysis.
##
###########################################################

## Create a new indicator variable for conducting ≥1 sensitivity
## analysis of-interest, excluding the E-value.

combined2 <- combined %>% 
  mutate(sum_sensitivity_anal_no_e = sum_sensitivity_anal - eval_ind,
         ge1_analysis_not_e = ifelse(sum_sensitivity_anal_no_e > 0, 1, 0))


## Calculate the number of articles that conducted sensitivity analyses 
## of-interest by journal type
## Use these numbers to re-make Figure 2 for the supplement.

combined2 %>% 
  group_by(journal_type, journal) %>% 
  summarize(sum = sum(ge1_analysis_not_e))





