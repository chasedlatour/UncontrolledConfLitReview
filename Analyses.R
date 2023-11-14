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

# Needed for piping
library(tidyverse)

# Needed for reading the Excel files.
library(readxl)

# Needed for writing the Excel files
library(writexl)


###########################################################
##
## Upload the data
##
###########################################################

combined <- readRDS("/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Manuscripts/Literature Review/combined_data_RandR.RDS") %>% 
  # Create month-year variables
  mutate(month_yr = case_when(
    month %in% 1:6 & year == 2017 ~ 1,
    month %in% 7:12 & year == 2017 ~ 2,
    month %in% 1:6 & year == 2018 ~ 3,
    month %in% 7:12 & year == 2018 ~ 4,
    month %in% 1:6 & year == 2019 ~ 5,
    month %in% 7:12 & year == 2019 ~ 6,
    month %in% 1:6 & year == 2020 ~ 7,
    month %in% 7:12 & year == 2020 ~ 8,
    month %in% 1:6 & year == 2021 ~ 9,
    month %in% 7:12 & year == 2021 ~ 10,
    month %in% 1:6 & year == 2022 ~ 11
  )) %>% 
  # Remove those where this value is missing
  subset(!is.na(month_yr))

#combined <- readRDS("/Users/chaselatour/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents - EPI.DETECTe/Manuscripts/Literature Review/combined_data.RDS")




###########################################################
##
## Conduct calculations used throughout the results.
##
###########################################################

## Count the number of manuscripts that we included in the review
combined %>% 
  summarize(total = n())

## Count the number of articles identified by journal type (med, epi)
combined %>% 
  group_by(journal_type) %>% 
  summarize(sum = n(),
            .groups = "keep")


## Count the number of articles identified by journal type (med, epi)
## and year (2018-2019 and 2020-2022)
combined %>% 
  mutate(batch = ifelse(year >= 2017 & year <= 2019 , 
                        "2017-2019", 
                        "2020-2022")) %>% 
  group_by(batch, journal_type) %>% 
  summarize(sum = n(),
            .groups = "keep")

## Count the number & percent of articles that conducted ≥1 sensitivity analysis of-interest
## overall
combined %>% 
  summarize(atleast_1 = sum(ge1_sens_anal),
            percent = round(100*atleast_1/n(),0))


## Count the number & percent of articles that conducted ≥1 sensitivity analysis of-interest
## by journal type
combined %>% 
  group_by(journal_type) %>% 
  summarize(atleast_1 = sum(ge1_sens_anal),
            percent = round(100*atleast_1/n(),0))

## Count the number and proportion of articles that used PSs in the primary analysis.
combined %>% 
  summarize(ps = sum(ps_ind),
            ps_percent = ps / n() * 100)




## Count the number of articles with more than 1 sens analysis of interest
combined %>% 
  group_by(journal_type) %>% 
  summarize(gt1 = sum(gt1_sens_anal))


## Count the number that conducted an sensitivity analysis for a known
## unmeasured confounder by journal type

combined %>% 
  mutate(specific_confounder_analysis_ind = ifelse(restrict_specific == 1 | eval_specific == 1 |
                                                     neg_cntrl_specific == 1 | ps_trim_specific == 1 |
                                                     cca_2_specific == 1 | cov_balance_specific == 1 |
                                                     qba_specific == 1 | dist_calibration_specific == 1 |
                                                     rule_out_specific == 1 | pos_cntrl_specific == 1 |
                                                     equipoise_specific == 1 | array_specific == 1 |
                                                     lin_psaty_kronmal_specific == 1 | iv_specific == 1 |
                                                     pba_specific == 1 | psc_specific == 1 | 
                                                     lin_psaty_kronmal_ps_specific == 1 | mi_specific == 1,
                                                   1,
                                                   0
                                                   )
         ) %>% 
  group_by(journal_type) %>% 
  summarize(
    n_anal = sum(ge1_sens_anal),
    n_anal_specific = sum(specific_confounder_analysis_ind, na.rm = T),
    percent = n_anal_specific/n_anal * 100,
    total = n(),
    percent_total = n_anal_specific / total * 100
  )


## Count the number that identified residual confounding as a limitation
## Overall
combined %>% 
  summarize(conf_limit = sum(conf_limitation_ind),
            total = n(),
            percentage = conf_limit/total * 100)



## Count the number that identified residual confounding as a limitation
## By journal type
combined %>% 
  group_by(journal_type) %>%
  summarize(conf_limit = sum(conf_limitation_ind),
            total = n(),
            percentage = conf_limit/total * 100)




###########################################################
##
## Summarize the number of confounders in primary analyses.
##
###########################################################

# Identify the number without any recorded confounders (i.e., couldn't ID)
nrow(subset(combined, is.na(num_primary_confounders)))

# Identify those that used IV in their primary analyses
nrow(subset(combined, iv_primary_ind == 1))


# Calculate the median and IQR on counts of covariates
combined %>% 
  # Subset to those with recorded confounders
  subset(!is.na(num_primary_confounders)) %>% 
  summarize(median = median(num_primary_confounders),
            q1 = quantile(num_primary_confounders, 0.25),
            q3 = quantile(num_primary_confounders, 0.75))

# Calculate categorical distribution
combined %>% 
  # Subset to those with recorded confounders
  subset(!is.na(num_primary_confounders)) %>% 
  mutate(num_conf = case_when(num_primary_confounders < 15 ~ "<15",
                              num_primary_confounders >= 15 & num_primary_confounders < 30 ~ "15-29",
                              num_primary_confounders >= 30 & num_primary_confounders < 100 ~ "30-99",
                              num_primary_confounders >= 100 ~ ">99")) %>% 
  group_by(num_conf) %>% 
  summarize(count = n(),
            percent = round(n()/(nrow(combined)-1)*100,0))

# Calculate number that adjusted for after exposure
combined %>% 
  # Subset to those with recorded confounders
  subset(!is.na(num_primary_confounders)) %>% 
  summarize(after_exp = sum(conf_after_exposure),
            percent = after_exp/n()*100)

# How many after exposure were also pregnancy studies
combined %>% 
  # Subset to those with recorded confounders
  subset(!is.na(num_primary_confounders)) %>%
  subset(conf_after_exposure == 1) %>% 
  summarize(preg = sum(pregnancy))

# Number that incorporated descriptors of the exposure into the confounder set:
combined %>% 
  # Subset to those with recorded confounders
  subset(!is.na(num_primary_confounders)) %>%
  summarize(count = sum(baseline_trt_confounders),
            percent = round(count/n()*100,0))


###########################################################
##
## Make time-trend figure
##
###########################################################

variable_names <- list(
  "epi" = "Epidemiology",
  "med" = "Medical"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}


## Plot the number of articles reviewed
combined %>% 
  group_by(month_yr, journal_type, ge1_sens_anal) %>% 
  summarize(total = n(),
            .groups = "drop") %>% 
  mutate(ge1 = as.factor(ge1_sens_anal)) %>% 
  ggplot(aes(fill=ge1, y=total, x=month_yr)) +
  geom_bar(position="stack", stat="identity") +
  ggtitle("Number of active comparator articles reviewed") +
  labs(fill = "1+ Sens Analysis") +
  scale_fill_discrete(limits = c("1","0"),
                      labels = c("Yes", "No")) +
  xlab("") +
  ylab("Count") +
  scale_x_discrete(limit = c('1','2','3','4','5','6','7','8','9','10','11'),
                   labels = c('Jan-Jun 2017', 'Jul-Dec 2017', 'Jan-Jun 2018',
                              'Jul-Dec 2018', 'Jan-Jun 2019', 'Jul-Dec 2019',
                              'Jan-Jun 2020', 'Jul-Dec 2020', 'Jan-Jun 2021',
                              'Jul-Dec 2021', 'Jan-Jun 2022')) +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(.~journal_type, ncol=1, strip.position = "left", labeller=variable_labeller)








###########################################################
##
## Figure 1 analyses
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
  summarize(#linked_dataset = sum(linked_dataset_ind),
            #ld_specific = sum(linked_dataset_specific),
            #cca = sum(cca_ind),
            #cca_specific = sum(cca_specific),
            cca2 = sum(cca_2_ind),
            cca2_specific = sum(cca_2_specific),
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
#n_cntrl <- subset(combined, neg_cntrl_ind == 1)
#View(n_cntrl)
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
cca <- subset(combined, cca_2_ind == 1) %>% 
  select(title,journal, year, authors, cca_ind, linked_dataset_ind, cca_info)
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
## Supplemental Table 2
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
## Supplemental Table 6
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
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
  mutate(ge1 = ifelse(rule_out_ind == 1 | array_ind == 1 | lin_psaty_kronmal_ind == 1 | qba_ind == 1 |
                        pba_ind == 1,
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
    cca = sum(cca_2_ind),
    ps_trim = sum(ps_trim_ind),
    equipoise = sum(equipoise_ind, na.rm = TRUE)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
  mutate(ge1 = ifelse(restrict_ind == 1 | ps_trim_ind == 1 | equipoise_ind == 1 |
                        cca_2_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ge1, na.rm = T),
            percent = sum / n() * 100)

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
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
  mutate(ge1_ind = ifelse(neg_cntrl_outcome_ind == 1 | neg_cntrl_exposure_ind == 1 | 
                            pos_cntrl_ind == 1 | dist_calibration_ind == 1 | 
                            cov_balance_ind == 1 | iv_ind == 1,
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
    qba = sum(qba_ind, na.rm = T),
    pba = sum(pba_ind),
    psc = sum(psc_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
  mutate(ind = ifelse(cov_balance_ind == 1 | psc_ind == 1 | 
                        qba_ind == 1 | pba_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ind, na.rm = T),
            percent = sum / n() * 100)


# (5) Validation study or sub-sample with data on exposures and confounders; Requires individual outcome data 
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    cca = sum(cca_2_ind),
    qba = sum(qba_ind, na.rm=T),
    mi = sum(mi_ind),
    lpk_ps = sum(lin_psaty_kronmal_ps_ind),
    pba = sum(pba_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
  mutate(ind = ifelse(cca_2_ind == 1 | lin_psaty_kronmal_ps_ind == 1 | qba_ind == 1 | 
                        pba_ind == 1 | mi_ind == 1,
                      1,
                      0)) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(ind, na.rm=T),
            percent = sum(ind, na.rm = T) / n() * 100)


###########################################################
##
## Supplemental Table 6
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
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
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
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
  group_by(journal_type) %>% 
  summarize(sum = sum(cov_balance_ind, na.rm = T),
            percent = sum / n() * 100)

# (3) New study estimate that can be used by the investigator to understand residual confounding 
combined %>% 
  group_by(journal_type) %>% 
  summarize(
    restrict = sum(restrict_ind),
    neg_outcome = sum(neg_cntrl_outcome_ind),
    cca = sum(cca_2_ind),
    ps_trim = sum(ps_trim_ind),
    neg_exposure = sum(neg_cntrl_exposure_ind),
    pos_control = sum(pos_cntrl_ind),
    equipoise = sum(equipoise_ind, na.rm=T)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
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
    qba = sum(qba_ind, na.rm=T),
    mi = sum(mi_ind),
    lpk_ps = sum(lin_psaty_kronmal_ps_ind),
    pba = sum(pba_ind),
    iv = sum(iv_ind),
    psc = sum(psc_ind)
  )

# Gather percentage of articles that implemented at least one of these
combined %>% 
  # Subset to those that implemented at least 1 sens analysis of interest
  subset(ge1_sens_anal == 1) %>% 
  mutate(ind = ifelse(lin_psaty_kronmal_ps_ind == 1 | qba_ind == 1 | pba_ind == 1 |
                        iv_ind == 1 | psc_ind == 1 | mi_ind == 1,
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
## Decided not to include because uninformative. - Almost
## always implemented with another sens analysis.
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






###########################################################
##
## Secondary Analysis
## Restrict primary analyses to those in line with prior
## literature reviews on the topic.
##
###########################################################


# Figure 1 in the supplement - Other analyses can be easily
# extrapolated since the specific sensitivity analyes are reported.

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


## Make a new sensitivity analysis indicator variable
combined %>% 
  rowwise() %>% 
  # Make sum for at least 1 of the relevant sens analyses
  mutate(sum_sens_anal_secondary = sum(neg_cntrl_exposure_ind, neg_cntrl_outcome_ind,
                                       iv_ind, mi_ind, psc_ind, dist_calibration_ind,
                                       eval_ind, rule_out_ind, array_ind, lin_psaty_kronmal_ind,
                                       lin_psaty_kronmal_ps_ind, qba_ind, pba_ind, na.rm = TRUE),
         ge1_sens_anal_secondary = ifelse(sum_sens_anal_secondary > 0, 1, 0)) %>% 
  # Make indicator variable based on that sum
  ungroup() %>% 
  # Count the number of articles that conducted at least one sens analysis, by journal
  group_by(journal_type, journal) %>% 
  summarize(atleast_1 = sum(ge1_sens_anal_secondary),
            total = n(),
            percent = atleast_1 / total * 100,
            .groups = "keep")


## Restrict the above to only look by journal type
## Make a new sensitivity analysis indicator variable
combined %>% 
  rowwise() %>% 
  # Make sum for at least 1 of the relevant sens analyses
  mutate(sum_sens_anal_secondary = sum(neg_cntrl_exposure_ind, neg_cntrl_outcome_ind,
                                       iv_ind, mi_ind, psc_ind, dist_calibration_ind,
                                       eval_ind, rule_out_ind, array_ind, lin_psaty_kronmal_ind,
                                       lin_psaty_kronmal_ps_ind, qba_ind, pba_ind, na.rm = TRUE),
         ge1_sens_anal_secondary = ifelse(sum_sens_anal_secondary > 0, 1, 0)) %>% 
  # Make indicator variable based on that sum
  ungroup() %>% 
  # Count the number of articles that conducted at least one sens analysis, by journal
  group_by(journal_type) %>% 
  summarize(atleast_1 = sum(ge1_sens_anal_secondary),
            total = n(),
            percent = atleast_1 / total * 100,
            .groups = "keep")



###########################################################
##
## Secondary Analysis
## Stratify by journals with and without sens analysis
## recommendations.
##
###########################################################

# Count the number of articles at each of the journals that have recommendations
combined %>% 
  subset(journal %in% c("Annals of Internal Med",
                        "NEJM", "Epidemiology",
                        "Pharmacoepidemiology and Drug Safety")) %>% 
  group_by(journal) %>% 
  summarise(count = n())


# Make indicator variable for journal recommendations - primary sens analysis definition
combined %>% 
  mutate(journal_rec = ifelse(journal %in% c("Annals of Internal Med",
                                            "NEJM", "Epidemiology",
                                            "Pharmacoepidemiology and Drug Safety"),
                              1,
                              0)) %>% 
  # Count the number/proportion of sens analyses by type
  group_by(journal_rec) %>% 
  summarize(ge1 = sum(ge1_sens_anal),
            count = n(),
            percent = ge1/n()*100)

# Count number of articles with at least 1 sens analysis
# per the secondary sensitivity analysis definition
combined %>%
  mutate(journal_rec = ifelse(journal %in% c("Annals of Internal Med",
                                             "NEJM", "Epidemiology",
                                             "Pharmacoepidemiology and Drug Safety"),
                              1,
                              0)) %>%
  rowwise() %>%
  # Make sum for at least 1 of the relevant sens analyses
  mutate(sum_sens_anal_secondary = sum(neg_cntrl_exposure_ind, neg_cntrl_outcome_ind,
                                       iv_ind, mi_ind, psc_ind, dist_calibration_ind,
                                       eval_ind, rule_out_ind, array_ind, lin_psaty_kronmal_ind,
                                       lin_psaty_kronmal_ps_ind, qba_ind, pba_ind, na.rm = TRUE),
         ge1_sens_anal_secondary = ifelse(sum_sens_anal_secondary > 0, 1, 0)) %>%
  # Make indicator variable based on that sum
  ungroup() %>%
  # Count the number/proportion of sens analyses by type
  group_by(journal_rec) %>%
  summarize(ge1 = sum(ge1_sens_anal_secondary),
            count = n(),
            percent = ge1/n()*100)



#Look at which analyses were implemented the most

table <- combined %>% 
  mutate(journal_rec = ifelse(journal %in% c("Annals of Internal Med",
                                             "NEJM", "Epidemiology",
                                             "Pharmacoepidemiology and Drug Safety"),
                              1,
                              0)) %>% 
  group_by(journal_rec) %>% 
  summarize(#linked_dataset = sum(linked_dataset_ind),
    #ld_specific = sum(linked_dataset_specific),
    #cca = sum(cca_ind),
    #cca_specific = sum(cca_specific),
    restrict = sum(restrict_ind),
    e_value = sum(eval_ind),
    n_cntrl_outcome = sum(neg_cntrl_outcome_ind),
    ps_trim = sum(ps_trim_ind),
    cca2 = sum(cca_2_ind),
    cov_bal = sum(cov_balance_ind, na.rm = TRUE),
    rule_out = sum(rule_out_ind),
    n_cntrl_exposure = sum(neg_cntrl_exposure_ind),
    pos_cntrl = sum(pos_cntrl_ind),
    qba = sum(qba_ind, na.rm=TRUE),
    dist_c = sum(dist_calibration_ind),
    equipoise = sum(equipoise_ind, na.rm = TRUE),
    array = sum(array_ind),
    lpk = sum(lin_psaty_kronmal_ind),
    iv = sum(iv_ind),
    pba = sum(pba_ind),
    psc = sum(psc_ind),
    lpk_ps = sum(lin_psaty_kronmal_ps_ind),
    mi = sum(mi_ind)
  )

## Look at the table for transferring to the Manuscript
View(table)



