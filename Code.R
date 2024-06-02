#==============================================================================
# Authors: Ivankovic F, Johnson S, Shen J, Scharf J, Mathews C
#
# Title:   Optimization of Self- or Parent-Reported Psychiatric Phenotypes in 
#          Longitudinal Studies
#
# Journal: Journal of Child Psychology and Psychiatry
#
# Corresponding author: F.I. @ franjo@franjo.us, fivankovic@broadinstitute.org
# Alternative contact:  C.M. @ carolmathews@ufl.edu
#
#==============================================================================
# TABLE OF CONTENTS
#------------------------------------------------------------------------------
# PART 1. ENVIRONMENT SETUP
# 1.1. SET WORKING DIRECTORY
# 1.2. LOAD NECESSARY LIBRARIES
# 1.3. IMPORT DATASETS
# 1.4. SPECIFY VERSION FOLDER
#
# PART 2. DEMOGRAPHICS ANALYSIS
# 2.1. OVERALL SAMPLE SIZES
# 2.2. AGES
# 2.3. SEX ASSIGNED AT BIRTH
# 2.4. GENDER IDENTITY
# 2.5. RACE
# 2.6. ETHNICITY
# 2.7. EDUCATIONAL ATTAINMENT
# 2.8. HOUSEHOLD INCOME
#
# PART 3: CALCULATE DIAGNOSES
# 3.1. PREPARE HOLDING DATA_FRAME (skip)
# 3.2. DEPRESSIVE DISORDERS MODULE (skip)
# 3.3. BIPOLAR DISORDERS MODULE
# 3.4. DISRUPTIVE MOOD DYSREGULATION DISORDERS MODULE (skip)
# 3.5. PSYCHOTIC DISORDERS MODULE
# 3.6. PANIC DISORDER MODULE
# 3.7. AGORAPHOBIA MODULE (skip)
# 3.8. SEPARATION ANXIETY DISORDER MODULE
# 3.9. SOCIAL ANXIETY DISORDER MODULE
# 3.10. SPECIFIC PHOBIA MODULE
# 3.11. GENERALIZED ANXIETY DISORDER MODULE
# 3.12. OBSESSIVE-COMPULSIVE DISORDER MODULE
# 3.13. ENURESIS AND ENCOPRESIS DISORDER MODULE (skip)
# 3.14. EATING DISORDERS MODULE
# 3.15. ATTENTION DEFICIT / HYPERACTIVITY DISORDER MODULE (skip)
# 3.16. OPPOSITIONAL DEFIANT DISORDER DISORDER MODULE
# 3.17. CONDUCT DISORDER MODULE
# 3.18. TIC DISORDERS MODULE
# 3.19. AUTISM SPECTRUM DISORDER MODULE  (skip)
# 3.20. ALCOHOL USE DISORDER MODULE  (skip)
# 3.21. DRUG USE DISORDER MODULE  (skip)
# 3.22. POST-TRAUMATIC STRESS DISORDER MODULE
# 3.23. SLEEP PROBLEMS MODULE (skip)
# 3.24. SUICIDALITY MODULE (skip)
# 3.25. HOMICIDALITY MODULE (skip)
# 3.26. SELECTIVE MUTISM MODULE (skip)
#
# PART 4. ANALYZE DIAGNOSES
# 4.1. DEFINE BROAD AND NARROW DATA FRAMES
# 4.2. CREATE PREVALENCE AND COMORBIDITY TABLES
#
# PART 5. ANALYZE CBCL DATA
# 5.1. PREPARE CBCL DATASET
# 5.2. RUN LOGISTIC REGRESSIONS ON CBCL DATA
# 
# PART 6. PLOT FIGURE 2
# 6.1. PREPARE DATA FOR PLOTTING
# 6.2. PLOT FIGURE 2A
# 6.3. PLOT FIGURE 2B
# 6.4. PLOT FIGURE 2C
# 6.5. ORGANIZE AND SAVE FIGURE 2
# 
# PART 7. PLOT FIGURE S2
# 7.1. PLOT DATA
# 7.2. SAVE PLOT
# 
# PART 8. PLOT FIGURE S3
# 8.1. PREPARE DATA FOR PLOTTING
# 8.2. PLOT DATA
# 8.3. SAVE PLOT
#
# PART 9. EXPORT DATA
# 9.1. EXPORT PREVALENCE AND COMORBIDITY TABLES
#==============================================================================

###############################
## PART 1. ENVIRONMENT SETUP ##
###############################

# 1.1. SET WORKING DIRECTORY
setwd("C:/.../ABCD_Phenotype")

# 1.2. LOAD NECESSARY LIBRARIES
library(readr)
library(readxl)
library(tidyverse)
library(caret)
library(cowplot)
library(scales)
library(xlsx)

# 1.3. IMPORT DATASETS
CBCL <- read_delim("DATA/ABCDv4/abcd_cbcl01.txt", delim="\t", 
                 escape_double=F, col_types="c", trim_ws=T)[-1,]
K1Y <- read_delim("DATA/ABCDv4/abcd_ksad501.txt", delim="\t", 
                  escape_double=F, col_types="c", trim_ws=T)[-1,]
K1P <- read_delim("DATA/ABCDv4/abcd_ksad01.txt", delim="\t", 
                  escape_double=F, col_types="c", trim_ws=T)[-1,]
K2Y <- read_delim("DATA/ABCDv4/ksads2daic_use_only01.txt", delim="\t", 
                  escape_double=F, col_types="c", trim_ws=T)[-1,]
K2P <- read_delim("DATA/ABCDv4/ksads2daic_use_only_p01.txt", delim="\t", 
                  escape_double=F, col_types="c", trim_ws=T)[-1,]
DB <- read_delim("DATA/ABCDv4/pdem02.txt", delim="\t", 
                  escape_double=F, col_types="c", trim_ws=T)[-1,]
DL <- read_delim("DATA/ABCDv4/abcd_lpds01.txt", delim="\t", 
                    escape_double=F, col_types="c", trim_ws=T)[-1,]


REF_PREV <- read_excel("SupplementalData.xlsx", 
                       sheet = "REF_PREV", range = "A1:E33")
REF_COM <- read_excel("SupplementalData.xlsx", 
                      sheet = "REF_COM", range = "A1:E45")

# 1.4. GET TIMEPOINT TOTALS
N_BL <- length(unique(
          filter(K1P, eventname=="baseline_year_1_arm_1") %>% 
            pull(src_subject_id)))
N_F1 <- length(unique(
          filter(K1P, eventname=="1_year_follow_up_y_arm_1") %>% 
            pull(src_subject_id)))
N_F2 <- length(unique(
          filter(K1P, eventname=="2_year_follow_up_y_arm_1") %>% 
            pull(src_subject_id)))
N_F3 <- length(unique(
          filter(K2P, eventname=="3_year_follow_up_y_arm_1") %>% 
            pull(src_subject_id)))

# 1.4. SPECIFY VERSION FOLDER
VER <- "SECOND_RESUBMISSION"

###################################
## PART 2. DEMOGRAPHICS ANALYSIS ##
###################################

# 2.1. OVERALL SAMPLE SIZES
table(K1P$eventname)
table(K1Y$eventname)
table(K2P$eventname)
table(K2Y$eventname)
table(CBCL$eventname)

# 2.2. AGES
DB %>%
  full_join(DL, by=c("src_subject_id"="src_subject_id", 
                     "eventname"="eventname")) %>%
  rowwise() %>%
  mutate(AGE=mean(c(as.numeric(demo_brthdat_v2),
                    as.numeric(demo_brthdat_v2_l)), na.rm=T)) %>% 
  group_by(eventname) %>%
  summarize(mean=mean(AGE, na.rm=T),
            sd=sd(AGE, na.rm=T)) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits=1)))

# 2.3. SEX ASSIGNED AT BIRTH
ID_BL <- pull(filter(DB, eventname=="baseline_year_1_arm_1"), 
              src_subject_id)
ID_F1 <- pull(filter(DL, eventname=="1_year_follow_up_y_arm_1"), 
              src_subject_id)
ID_F2 <- pull(filter(DL, eventname=="2_year_follow_up_y_arm_1"), 
              src_subject_id)
ID_F3 <- pull(filter(DL, eventname=="3_year_follow_up_y_arm_1"), 
              src_subject_id)

DB %>% 
  filter(src_subject_id %in% ID_BL) %>%
  pull(demo_sex_v2) %>%
  table(.) %>%
  as.data.frame(.) %>%
  mutate(Prop=round(Freq/N_BL*100, digits=1))

DB %>% 
  filter(src_subject_id %in% ID_F1) %>%
  pull(demo_sex_v2) %>%
  table(.) %>%
  as.data.frame(.) %>%
  mutate(Prop=round(Freq/N_F1*100, digits=1))

DB %>% 
  filter(src_subject_id %in% ID_F2) %>%
  pull(demo_sex_v2) %>%
  table(.) %>%
  as.data.frame(.) %>%
  mutate(Prop=round(Freq/N_F2*100, digits=1))

DB %>% 
  filter(src_subject_id %in% ID_F3) %>%
  pull(demo_sex_v2) %>%
  table(.) %>%
  as.data.frame(.) %>%
  mutate(Prop=round(Freq/N_F3*100, digits=1))

# 2.4. GENDER IDENTITY
DB %>%
  full_join(DL, by=c("src_subject_id"="src_subject_id", 
                     "eventname"="eventname",
                     "subjectkey"="subjectkey")) %>%
  mutate(across(starts_with("demo_gender"),
                ~replace(., which(. %in% c(777,999, 6)|is.na(.)), 0))) %>%
  mutate(across(starts_with("demo_gender"),
                ~replace(., which(. %in% c(1,2)|is.na(.)), 1))) %>%
  mutate(across(starts_with("demo_gender"),
                ~replace(., which(. %in% c(3,4,5)|is.na(.)), 2))) %>%
  mutate(across(starts_with("demo_gender"),
                ~as.numeric(.))) %>%
  mutate(X=ifelse(demo_gender_id_v2==demo_gender_id_v2_l,
                  demo_gender_id_v2,
                  demo_gender_id_v2+demo_gender_id_v2_l)) %>%
  group_by(eventname, X) %>%
  summarise(N=n()) %>%
  as.data.frame(.) %>%
  select(c(eventname, X, N)) %>%
  mutate(NEV=case_when(
    eventname=="baseline_year_1_arm_1" ~ N_BL,
    eventname=="1_year_follow_up_y_arm_1" ~ N_F1,
    eventname=="2_year_follow_up_y_arm_1" ~ N_F2,
    eventname=="3_year_follow_up_y_arm_1" ~ N_F3,
    TRUE ~ NA_real_)) %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

# 2.5. RACE
RACE <- DB %>%
  select(c(src_subject_id, eventname, demo_race_a_p___10, demo_race_a_p___11, 
           demo_race_a_p___12, demo_race_a_p___13, demo_race_a_p___14, 
           demo_race_a_p___15, demo_race_a_p___16, demo_race_a_p___17, 
           demo_race_a_p___18, demo_race_a_p___19, demo_race_a_p___20, 
           demo_race_a_p___21, demo_race_a_p___22, demo_race_a_p___23,
           demo_race_a_p___24, demo_race_a_p___25)) %>%
  pivot_longer(c(demo_race_a_p___10:demo_race_a_p___25), names_to="RACE",
               values_to="V") %>%
  filter(V>0) %>%
  select(-V) %>%
  mutate(RACE=gsub("demo_race_a_p___","",RACE)) %>%
  mutate(RACE_CAT=case_when(
    RACE=="10" ~ 1,
    RACE=="11" ~ 10,
    RACE %in% c("12", "13") ~ 100,
    RACE %in% c("14", "15", "16", "17") ~ 1000,
    RACE %in% c("18", "19", "20", "21", "22", "23", "24") ~ 10000,
    RACE=="25" ~ 100000,
    TRUE ~ NA_real_)) %>%
  select(c(src_subject_id, eventname, RACE_CAT)) %>%
  distinct() %>%
  group_by(src_subject_id, eventname) %>%
  summarize(RACE=sum(RACE_CAT)) %>%
  mutate(MULTIRACIAL = str_count(RACE, "1")) %>%
  mutate(RACE=ifelse(MULTIRACIAL>1, 100000, RACE)) %>%
  mutate(RACE_CAT=case_when(
    RACE==1 ~ "WHITE",
    RACE==10 ~ "BLACK",
    RACE==100 ~ "NATAM",
    RACE==1000 ~ "PACIS",
    RACE==10000 ~ "ASIAN",
    RACE==100000 ~ "MULTOT",
    TRUE ~ NA_character_))
  
RACE %>%
  filter(src_subject_id %in% ID_BL) %>%
  group_by(eventname, RACE_CAT) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=RACE_CAT, values_from=N) %>%
  mutate(NEV=N_BL) %>%
  mutate(MISS=NEV-WHITE-BLACK-NATAM-PACIS-ASIAN-MULTOT) %>%
  pivot_longer(c(WHITE, BLACK, ASIAN, NATAM, PACIS, MULTOT, MISS), 
               names_to="RACE", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

RACE %>%
  filter(src_subject_id %in% ID_F1) %>%
  group_by(eventname, RACE_CAT) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=RACE_CAT, values_from=N) %>%
  mutate(NEV=N_F1) %>%
  mutate(MISS=NEV-WHITE-BLACK-NATAM-PACIS-ASIAN-MULTOT) %>%
  pivot_longer(c(WHITE, BLACK, ASIAN, NATAM, PACIS, MULTOT, MISS), 
               names_to="RACE", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

RACE %>%
  filter(src_subject_id %in% ID_F2) %>%
  group_by(eventname, RACE_CAT) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=RACE_CAT, values_from=N) %>%
  mutate(NEV=N_F2) %>%
  mutate(MISS=NEV-WHITE-BLACK-NATAM-PACIS-ASIAN-MULTOT) %>%
  pivot_longer(c(WHITE, BLACK, ASIAN, NATAM, PACIS, MULTOT, MISS), 
               names_to="RACE", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

RACE %>%
  filter(src_subject_id %in% ID_F3) %>%
  group_by(eventname, RACE_CAT) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=RACE_CAT, values_from=N) %>%
  mutate(NEV=N_F3) %>%
  mutate(MISS=NEV-WHITE-BLACK-NATAM-PACIS-ASIAN-MULTOT) %>%
  pivot_longer(c(WHITE, BLACK, ASIAN, NATAM, PACIS, MULTOT, MISS), 
               names_to="RACE", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

# 2.6. ETHNICITY
ETHNICITY <- DB %>%
  filter(!is.na(demo_ethn_v2) & eventname=="baseline_year_1_arm_1") %>%
  select(c(src_subject_id, demo_ethn_v2)) %>%
  mutate(across(starts_with("demo_ethn_v2"),
                ~replace(., which(. %in% c(777,999)), NA)))

ETHNICITY$demo_ethn_v2 <- as.character(ETHNICITY$demo_ethn_v2)

ETHNICITY$demo_ethn_v2[ETHNICITY$demo_ethn_v2=="1"] <- "YES"
ETHNICITY$demo_ethn_v2[ETHNICITY$demo_ethn_v2=="2"] <- "NO"

ETHNICITY %>%
  filter(src_subject_id %in% ID_BL) %>%
  group_by(demo_ethn_v2) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=demo_ethn_v2, values_from=N) %>%
  mutate(NEV=N_BL) %>%
  mutate(MISS=NEV-YES-NO) %>%
  pivot_longer(c(YES, NO, MISS), 
               names_to="ETHNICITY", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

ETHNICITY %>%
  filter(src_subject_id %in% ID_F1) %>%
  group_by(demo_ethn_v2) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=demo_ethn_v2, values_from=N) %>%
  mutate(NEV=N_F1) %>%
  mutate(MISS=NEV-YES-NO) %>%
  pivot_longer(c(YES, NO, MISS), 
               names_to="ETHNICITY", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

ETHNICITY %>%
  filter(src_subject_id %in% ID_F2) %>%
  group_by(demo_ethn_v2) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=demo_ethn_v2, values_from=N) %>%
  mutate(NEV=N_F2) %>%
  mutate(MISS=NEV-YES-NO) %>%
  pivot_longer(c(YES, NO, MISS), 
               names_to="ETHNICITY", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

ETHNICITY %>%
  filter(src_subject_id %in% ID_F3) %>%
  group_by(demo_ethn_v2) %>%
  summarize(N=n()) %>%
  pivot_wider(names_from=demo_ethn_v2, values_from=N) %>%
  mutate(NEV=N_F3) %>%
  mutate(MISS=NEV-YES-NO) %>%
  pivot_longer(c(YES, NO, MISS), 
               names_to="ETHNICITY", values_to="N") %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

# 2.7. EDUCATIONAL ATTAINMENT
EDUAT <- DB %>%
  full_join(DL, by=c("src_subject_id"="src_subject_id", 
                     "eventname"="eventname",
                     "subjectkey"="subjectkey")) %>%
  select(c(src_subject_id, eventname, demo_prnt_ed_v2,
           demo_prnt_ed_v2_l, demo_prtnr_ed_v2, demo_prtnr_ed_v2_l, 
           demo_prnt_ed_v2_2yr_l, demo_prtnr_ed_v2_2yr_l)) %>%
  mutate(across(starts_with("demo_"),
                ~replace(., which(. %in% c(777,999)), NA))) %>%
  mutate(across(starts_with("demo_"),
                ~as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ case_when(
    .x<13 ~ 1,                                    # Less than highschool
    .x>12 & .x<15 ~ 10,                           # highschool ged
    (.x>15 & .x<18) | (.x>21 & .x<25) ~ 100,      # some college
    .x==18 ~ 1000,                                # undergraduate degree
    .x>18 & .x<22 ~ 10000,                        # graduate degreee
    TRUE ~ NA_real_))) %>%
  pivot_longer(cols=starts_with("demo")) %>% 
  filter(!is.na(value)) %>%
  select(src_subject_id, eventname, value) %>%
  distinct() %>%
  group_by(src_subject_id, eventname) %>%
  summarise(EDAT=sum(value)) %>%
  mutate(EDAT=str_count(EDAT, "[0-9]")) %>%
  mutate(EDCAT=case_when(
    EDAT==1 ~ "< HS",
    EDAT==2 ~ "HS / GED",
    EDAT==3 ~ "SOME COLLEGE",
    EDAT==4 ~ "UNDERGRADUATE DEGREE",
    EDAT==5 ~ "GRADUATE DEGREE",
    TRUE ~ NA_character_),
    .keep="unused") %>%
  group_by(eventname, EDCAT) %>%
  summarize(N=n()) %>%
  group_by(eventname) %>%
  mutate(NOBS=sum(N)) %>%
  ungroup() %>%
  mutate(NEV=case_when(
    eventname=="baseline_year_1_arm_1" ~ N_BL,
    eventname=="1_year_follow_up_y_arm_1" ~ N_F1,
    eventname=="2_year_follow_up_y_arm_1" ~ N_F2,
    eventname=="3_year_follow_up_y_arm_1" ~ N_F3,
    TRUE ~ NA_real_)) 

EDUAT %>% 
  select(eventname, EDCAT, N, NEV) %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused") %>%
  print(n=25)

EDUAT %>% 
  select(eventname, NOBS, NEV) %>%
  distinct() %>%
  mutate(N=NEV-NOBS,
         P=round((NEV-NOBS)/NEV*100, digits=1),
         .keep="unused") %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

rm(EDUAT)

# 2.8. HOUSEHOLD INCOME
HINC <- DB %>%
  full_join(DL, by=c("src_subject_id"="src_subject_id", 
                     "eventname"="eventname",
                     "subjectkey"="subjectkey")) %>%
  select(c(src_subject_id, eventname, 
           demo_comb_income_v2, demo_comb_income_v2_l)) %>%
  mutate(across(starts_with("demo_comb"),
                ~replace(., which(. %in% c(777,999)), NA))) %>%
  mutate(income=ifelse(!is.na(demo_comb_income_v2), demo_comb_income_v2,
                       demo_comb_income_v2_l),
         .keep="unused") %>%
  mutate(income=as.numeric(income)) %>%
  mutate(INC_CAT=case_when(
    income<5 ~ "A. 0 - 25000",
    income>4 & income<8 ~ "B. 25000 - 75000",
    income>7 & income<10 ~ "C. 75000 - 200000",
    income==10 ~ "D. 200000+",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(INC_CAT)) %>%
  group_by(eventname, INC_CAT) %>%
  summarize(N=n()) %>%
  group_by(eventname) %>%
  mutate(NOBS=sum(N)) %>%
  ungroup() %>%
  mutate(NEV=case_when(
    eventname=="baseline_year_1_arm_1" ~ N_BL,
    eventname=="1_year_follow_up_y_arm_1" ~ N_F1,
    eventname=="2_year_follow_up_y_arm_1" ~ N_F2,
    eventname=="3_year_follow_up_y_arm_1" ~ N_F3,
    TRUE ~ NA_real_)) 

HINC %>% 
  select(eventname, INC_CAT, N, NEV) %>%
  mutate(P=round(N/NEV*100, digits=1)) %>%
  select(-NEV) %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused") %>%
  print(n=25)

HINC %>% 
  select(eventname, NOBS, NEV) %>%
  distinct() %>%
  mutate(N=NEV-NOBS,
         P=round((NEV-NOBS)/NEV*100, digits=1),
         .keep="unused") %>%
  mutate(Stat=paste0(N, " (", P,")"),
         .keep="unused")

rm(HINC)

#################################
## PART 3. CALCULATE DIAGNOSES ##
#################################

# 3.1. PREPARE HOLDING DATA_FRAME
DAT <- full_join(select(K1P, c(src_subject_id)),
                 select(K1Y, c(src_subject_id)),
                 by="src_subject_id",
                 relationship="many-to-many") %>%
  distinct(.)

# 3.2. DEPRESSIVE DISORDERS MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, data removed due to programming errors.
#------------------------------------------------------------------------------

# 3.3. BIPOLAR DISORDERS MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_2_830_p - BDI Current Manic [F31.1x]
# ksads_2_831_p - BDI Current Depressed [F31.3x]
# ksads_2_832_p - BDI Current Hypomanic [F31.0]
# ksads_2_833_p - BDI Most Recent Past Manic [F31.1x]
# ksads_2_834_p - BDI Most Recent Past Depressed [F31.3x]
# ksads2_2_798_p - BDI Current Manic [F31.1x]
# ksads2_2_799_p - BDI Current Depressed [F31.3x]
# ksads2_2_800_p - BDI Most Recent Manic [F31.9]
#
# ksads_2_830_t - BDI Current Manic [F31.1x]
# ksads_2_831_t - BDI Current Depressed [F31.3x]
# ksads_2_832_t - BDI Current Hypomanic [F31.0]
# ksads_2_833_t - BDI Most Recent Past Manic [F31.1x]
# ksads_2_834_t - BDI Most Recent Past Depressed [F31.3x]
# ksads2_2_798_t - BDI Current Manic [F31.1x]
# ksads2_2_799_t - BDI Current Depressed [F31.3x]
# ksads2_2_800_t - BDI Most Recent Manic [F31.9]
#
# Abbreviations:
# BDI - Bipolar Disorder Type I
#------------------------------------------------------------------------------

BDIP <- full_join(K1P, K2P, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_2_830_p,        # V1 BDI Current Manic
           ksads_2_831_p,        # V1 BDI Current Depressed
           ksads_2_832_p,        # V1 BDI Current Hypomanic
           ksads_2_833_p,        # V1 BDI Most Recent Past Manic
           ksads_2_834_p,        # V1 BDI Most Recent Past Depressed
           ksads2_2_798_p,       # V2 BDI Current Manic
           ksads2_2_799_p,       # V2 BDI Current Depressed
           ksads2_2_800_p))      # V2 BDI Most Recent Manic

BDIY <- full_join(K1Y, K2Y, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_2_830_t,        # V1 BDI Current Manic
           ksads_2_831_t,        # V1 BDI Current Depressed
           ksads_2_832_t,        # V1 BDI Current Hypomanic
           ksads_2_833_t,        # V1 BDI Most Recent Past Manic
           ksads_2_834_t,        # V1 BDI Most Recent Past Depressed
           ksads2_2_798_t,       # V2 BDI Current Manic
           ksads2_2_799_t,       # V2 BDI Current Depressed
           ksads2_2_800_t))      # V2 BDI Most Recent Manic

table(stack(select(BDIP, starts_with("ksads")))$values, 
      useNA="always")

table(stack(select(BDIY, starts_with("ksads")))$values, 
      useNA="always")

BDIP_V <- BDIY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BDIY_V <- BDIY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BDIP <- BDIP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BDIP_V, by="src_subject_id")

BDIY <- BDIY %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BDIY_V, by="src_subject_id")

BDI <- full_join(BDIP, BDIY, by="src_subject_id") %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(TC=TC.x+TC.y,
         AS=AS.x+AS.y,
         .keep="unused") %>%
  mutate(nBDI=as.numeric((TC>(AS/2)) & (AS>1)),
         bnBDI=as.numeric(TC>0),
         .keep="unused")

table(Narrow=BDI$nBDI, Broad=BDI$bnBDI)

DAT <- DAT %>%
  left_join(BDI, by="src_subject_id")

rm(BDI, BDIP, BDIY, BDIP_V, BDIY_V)

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_2_835_p - BDII Current Hypomanic [F31.81]
# ksads_2_836_p - BDII Current Depressed [F31.81]
# ksads_2_837_p - BDII Most Recent Past Hypomanic [F31.81]
# ksads2_2_801_p - BDII Current Hypomanic [F31.81]
# ksads2_2_802_p - BDII Most Recent Past Depressed [F31.81]
# ksads2_2_931_p - BDII Current Depressed [F31.81]
#
# ksads_2_835_t - BDII Current Hypomanic [F31.81]
# ksads_2_836_t - BDII Current Depressed [F31.81]
# ksads_2_837_t - BDII Most Recent Past Hypomanic [F31.81]
# ksads2_2_801_t - BDII Current Hypomanic [F31.81]
# ksads2_2_802_t - BDII Most Recent Past Depressed [F31.81]
# ksads2_2_931_t - BDII Current Depressed [F31.81]
#
# Abbreviations:
# BDII - Bipolar Disorder Type II
#------------------------------------------------------------------------------

BDIIP <- full_join(K1P, K2P, 
                   by=c("src_subject_id"="src_subject_id",
                        "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_2_835_p,        # V1 BDII Current Hypomanic
           ksads_2_836_p,        # V1 BDII Current Depressed
           ksads_2_837_p,        # V1 BDII Most Recent Past Hypomanic
           ksads2_2_801_p,       # V2 BDII Current Hypomanic
           ksads2_2_802_p,       # V2 BDII Most Recent Past Depressed
           ksads2_2_931_p))      # V2 BDII Current Depressed

BDIIY <- full_join(K1Y, K2Y, 
                   by=c("src_subject_id"="src_subject_id",
                        "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_2_835_t,        # V1 BDII Current Hypomanic
           ksads_2_836_t,        # V1 BDII Current Depressed
           ksads_2_837_t,        # V1 BDII Most Recent Past Hypomanic
           ksads2_2_801_t,       # V2 BDII Current Hypomanic
           ksads2_2_802_t,       # V2 BDII Most Recent Past Depressed
           ksads2_2_931_t))      # V2 BDII Current Depressed


table(stack(select(BDIIP, starts_with("ksads")))$values, 
      useNA="always")

table(stack(select(BDIIY, starts_with("ksads")))$values, 
      useNA="always")

BDIIP_V <- BDIIY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BDIIY_V <- BDIIY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BDIIP <- BDIIP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BDIIP_V, by="src_subject_id")

BDIIY <- BDIIY %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BDIIY_V, by="src_subject_id")

BDII <- full_join(BDIIP, BDIIY, by="src_subject_id") %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(TC=TC.x+TC.y,
         AS=AS.x+AS.y,
         .keep="unused") %>%
  mutate(nBDII=as.numeric((TC>(AS/2)) & (AS>1)),
         bnBDII=as.numeric(TC>0),
         .keep="unused")

table(Narrow=BDII$nBDII, Broad=BDII$bnBDII)

DAT <- DAT %>%
  left_join(BDII, by="src_subject_id")

rm(BDII, BDIIP, BDIIY, BDIIP_V, BDIIY_V)

#------------------------------------------------------------------------------
# NOTE:
# Bipolar group will be determined as a presence of either BPI or BP2, either
# at narrow level (nBDI | nBDII) or broad+narrow level (bnBDI | bnBDII)
#------------------------------------------------------------------------------

DAT <- DAT %>%
  mutate(nBD=as.numeric(nBDI|nBDII),
         bnBD=as.numeric(bnBDI|bnBDII))

# 3.4. DISRUPTIVE MOOD DYSREGULATION DISORDERS MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, data removed due to programming errors.
#------------------------------------------------------------------------------

# 3.5. PSYCHOTIC DISORDERS MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_4_826_p - Hallucinations Present
# ksads_4_827_p - Hallucinations Past
# ksads_4_828_p - Delusions Present
# ksads_4_829_p - Delusions Past
# ksads_4_849_p - Associated Psychotic Symptoms Current
# ksads_4_850_p - Associated Psychotic Symptoms Past
# ksads2_4_805_p - Schizophrenia [F20.9]
# ksads2_4_806_p - Schizophreniform Disorder [F20.81]
# ksads2_4_807_p - Schizoaffective Disorder [F25.0/1] 
#
# Abbreviations:
# PSY - psychotic disorders
#------------------------------------------------------------------------------

PSYP <- full_join(K1P, K2P, 
            by=c("src_subject_id"="src_subject_id",
                 "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_4_826_p,        # V1 Hallucinations Present
           ksads_4_827_p,        # V1 Hallucinations Past
           ksads_4_828_p,        # V1 Delusions Present
           ksads_4_829_p,        # V1 Delusions Past
           ksads_4_849_p,        # V1 Associated Psychotic Symptoms Current
           ksads_4_850_p,        # V1 Associated Psychotic Symptoms Past
           ksads2_4_805_p,       # V2 Schizophrenia
           ksads2_4_806_p,       # V2 Schizophreniform Disorder
           ksads2_4_807_p))      # V2 Schizoaffective Disorder

table(stack(select(PSYP, starts_with("ksads")))$values, 
      useNA="always")

PSYP_V <- PSYP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "1_year_follow_up_y_arm_1",
             "2_year_follow_up_y_arm_1",
             "3_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

PSYP <- PSYP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(PSYP_V, by="src_subject_id")

PSY <- PSYP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nPSY=as.numeric((TC>(AS/2)) & (AS>1)),
         bnPSY=as.numeric(TC>0),
         .keep="unused")

table(Narrow=PSY$nPSY, Broad=PSY$bnPSY)

DAT <- DAT %>%
  left_join(PSY, by="src_subject_id")

rm(PSY, PSYP, PSYP_V)

# 3.6. PANIC DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_5_857_p - PD Present [F41.0]
# ksads_5_858_p - PD Past [F41.0]
# ksads2_5_814_p - PD Present [F41.0]
# ksads2_5_815_p - PD Present in Partial Remission [F41.0]
# ksads2_5_816_p - PD Past [F41.0]
#
# Abbreviations:
# PD - panic disorder
#------------------------------------------------------------------------------

PDP <- full_join(K1P, K2P, 
                 by=c("src_subject_id"="src_subject_id",
                      "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_5_857_p,        # V1 PD Present
           ksads_5_858_p,        # V1 PD Past
           ksads2_5_814_p,       # V2 PD Present
           ksads2_5_815_p,       # V2 PD Present in Partial Remission
           ksads2_5_816_p))      # V2 PD Past

table(stack(select(PDP, starts_with("ksads")))$values, 
      useNA="always")

PDP_V <- PDP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

PDP <- PDP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(PDP_V, by="src_subject_id")

PD <- PDP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nPD=as.numeric((TC>(AS/2)) & (AS>1)),
         bnPD=as.numeric(TC>0),
         .keep="unused")

table(Narrow=PD$nPD, Broad=PD$bnPD)

DAT <- DAT %>%
  left_join(PD, by="src_subject_id")

rm(PD, PDP, PDP_V)

# 3.7. AGORAPHOBIA MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, data removed due to programming errors.
#------------------------------------------------------------------------------

# 3.8. SEPARATION ANXIETY DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_7_861_p - SAD Present [F93.00]
# ksads_7_862_p - SAD Past [F93.00]
# ksads2_7_819_p - SAD Present [F93.00]
# ksads2_7_820_p - SAD Past [F93.00]
#
# Abbreviations:
# SAD - separation anxiety disorder
#------------------------------------------------------------------------------

SADP <- full_join(K1P, K2P, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_7_861_p,        # V1 SAD Present
           ksads_7_862_p,        # V1 SAD Past
           ksads2_7_819_p,       # V2 SAD Present
           ksads2_7_820_p))      # V2 SAD Past

table(stack(select(SADP, starts_with("ksads")))$values, 
      useNA="always")

SADP_V <- SADP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

SADP <- SADP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(SADP_V, by="src_subject_id")

SAD <- SADP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nSAD=as.numeric((TC>(AS/2)) & (AS>1)),
         bnSAD=as.numeric(TC>0),
         .keep="unused")

table(Narrow=SAD$nSAD, Broad=SAD$bnSAD)

DAT <- DAT %>%
  left_join(SAD, by="src_subject_id")

rm(SAD, SADP, SADP_V)

# 3.9. SOCIAL ANXIETY DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_8_864_p - SOPH Present [F40.10]
# ksads_8_863_p - SOPH Past [F40.10]
# ksads2_8_821_p - SOPH Present [F40.10]
# ksads2_8_822_p - SOPH Past [F40.10]
#
# ksads_8_864_t - SOPH Present [F40.10]
# ksads_8_863_t - SOPH Past [F40.10]
# ksads2_8_821_t - SOPH Present [F40.10]
# ksads2_8_822_t - SOPH Past [F40.10]
#
# Abbreviations:
# SOPH - social phobia (also social anxiety disorder)
#------------------------------------------------------------------------------

SOPHP <- full_join(K1P, K2P, 
                   by=c("src_subject_id"="src_subject_id",
                        "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_8_864_p,        # V1 SOPH Present
           ksads_8_863_p,        # V1 SOPH Past
           ksads2_8_821_p,       # V2 SOPH Present
           ksads2_8_822_p))      # V2 SOPH Past

SOPHY <- full_join(K1Y, K2Y, 
                   by=c("src_subject_id"="src_subject_id",
                        "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_8_864_t,        # V1 SOPH Present
           ksads_8_863_t,        # V1 SOPH Remission
           ksads2_8_821_t,       # V2 SOPH Present
           ksads2_8_822_t))      # V2 SOPH Past

table(stack(select(SOPHP, starts_with("ksads")))$values, 
      useNA="always")

table(stack(select(SOPHY, starts_with("ksads")))$values, 
      useNA="always")

SOPHP_V <- SOPHP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

SOPHY_V <- SOPHY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

SOPHP <- SOPHP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(SOPHP_V, by="src_subject_id")

SOPHY <- SOPHY %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(SOPHY_V, by="src_subject_id")

SOPH <- full_join(SOPHP, SOPHY, by="src_subject_id") %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(TC=TC.x+TC.y,
         AS=AS.x+AS.y,
         .keep="unused") %>%
  mutate(nSOPH=as.numeric((TC>(AS/2)) & (AS>1)),
         bnSOPH=as.numeric(TC>0),
         .keep="unused")

table(Narrow=SOPH$nSOPH, Broad=SOPH$bnSOPH)

DAT <- DAT %>%
  left_join(SOPH, by="src_subject_id")

rm(SOPH, SOPHP, SOPHY, SOPHP_V, SOPHY_V)

# 3.10. SPECIFIC PHOBIA MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_9_867_p - SP Present [F40.2XX]
# ksads_9_868_p - SP Past [F40.2XX]
# ksads2_9_825_p - SP Present [F40.2XX]
# ksads2_9_826_p - SP Past [F40.2XX]
#
# Abbreviations:
# SP - specific phobia
#------------------------------------------------------------------------------

SPP <- full_join(K1P, K2P, 
                 by=c("src_subject_id"="src_subject_id",
                      "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_9_867_p,        # V1 SP Present
           ksads_9_868_p,        # V1 SP Past
           ksads2_9_825_p,       # V2 SP Present
           ksads2_9_826_p))      # V2 SP Past

table(stack(select(SPP, starts_with("ksads")))$values, 
      useNA="always")

SPP_V <- SPP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

SPP <- SPP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(SPP_V, by="src_subject_id")

SP <- SPP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nSP=as.numeric((TC>(AS/2)) & (AS>1)),
         bnSP=as.numeric(TC>0),
         .keep="unused")

table(Narrow=SP$nSP, Broad=SP$bnSP)

DAT <- DAT %>%
  left_join(SP, by="src_subject_id")

rm(SP, SPP, SPP_V)

# 3.11. GENERALIZED ANXIETY DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_10_869_p - GAD Present [F41.1]
# ksads_10_870_p - GAD Past [F41.1] 
# ksads2_10_827_p - GAD Present [F41.1] 
# ksads2_10_827_p - GAD Past [F41.1]
#
# ksads_10_869_t - GAD Present [F41.1]
# ksads_10_870_t - GAD Past [F41.1]
# ksads2_10_827_t - GAD Present [F41.1]
# ksads2_10_827_t - GAD Past [F41.1]
#
# Abbreviations:
# GAD - generalized anxiety disorder
#------------------------------------------------------------------------------

GADP <- full_join(K1P, K2P, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_10_869_p,       # V1 GAD Present
           ksads_10_870_p,       # V1 GAD Past
           ksads2_10_827_p,      # V2 GAD Present
           ksads2_10_827_p))     # V2 GAD Past

GADY <- full_join(K1Y, K2Y, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_10_869_t,       # V1 GAD Present
           ksads_10_870_t,       # V1 GAD Past
           ksads2_10_827_t,      # V2 GAD Present
           ksads2_10_827_t))     # V2 GAD Past

table(stack(select(GADP, starts_with("ksads")))$values, 
      useNA="always")

table(stack(select(GADY, starts_with("ksads")))$values, 
      useNA="always")

GADP_V <- GADP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

GADY_V <- GADY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

GADP <- GADP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(GADP_V, by="src_subject_id")

GADY <- GADY %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(GADY_V, by="src_subject_id")

GAD <- full_join(GADP, GADY, by="src_subject_id") %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(TC=TC.x+TC.y,
         AS=AS.x+AS.y,
         .keep="unused") %>%
  mutate(nGAD=as.numeric((TC>(AS/2)) & (AS>1)),
         bnGAD=as.numeric(TC>0),
         .keep="unused")

table(Narrow=GAD$nGAD, Broad=GAD$bnGAD)

DAT <- DAT %>%
  left_join(GAD, by="src_subject_id")

rm(GAD, GADP, GADY, GADP_V, GADY_V)

# 3.12. OBSESSIVE-COMPULSIVE DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_11_917_p - OCD Present [F42]
# ksads_11_918_p - OCD Past [F42]
# ksads2_11_877_p - OCD Present [F42]
# ksads2_11_878_p - OCD Past [F42]
#
# Abbreviations:
# OCD - obsessive-compulsive disorder
#------------------------------------------------------------------------------

OCDP <- full_join(K1P, K2P, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_11_917_p,       # V1 OCD Present
           ksads_11_918_p,       # V1 OCD Past
           ksads2_11_877_p,      # V2 OCD Present
           ksads2_11_878_p))     # V2 OCD Past

table(stack(select(OCDP, starts_with("ksads")))$values, 
      useNA="always")

OCDP_V <- OCDP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

OCDP <- OCDP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(OCDP_V, by="src_subject_id")

OCD <- OCDP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nOCD=as.numeric((TC>(AS/2)) & (AS>1)),
         bnOCD=as.numeric(TC>0),
         .keep="unused")

table(Narrow=OCD$nOCD, Broad=OCD$bnOCD)

DAT <- DAT %>%
  left_join(OCD, by="src_subject_id")

rm(OCDP, OCDP_V)

# 3.13. ENURESIS AND ENCOPRESIS DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, because module was not administered to participants at any point.
#------------------------------------------------------------------------------

# 3.14. EATING DISORDERS MODULE

#------------------------------------------------------------------------------
# NOTE:
# Anorexia nervosa skipped, data removed due to programming errors.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_13_935_p - BN Present [F50.2]
# ksads_13_936_p - BN Past [F50.2]
# ksads_13_937_p - BN Remission [F50.2]
# ksads2_13_896_p - BN Present [F50.2]
# ksads2_13_897_p - BN Past [F50.2]
# ksads2_13_898_p - BN Remission [F50.2]
#
# ksads_13_935_t - BN Present [F50.2]
# ksads_13_936_t - BN Past [F50.2]
# ksads_13_937_t - BN Remission [F50.2]
# ksads2_13_896_t - BN Present [F50.2]
# ksads2_13_897_t - BN Past [F50.2]
# ksads2_13_898_t - BN Remission [F50.2]
#
# Abbreviations:
# BN - bulimia nervosa
#------------------------------------------------------------------------------

BNP <- full_join(K1P, K2P, 
                 by=c("src_subject_id"="src_subject_id",
                      "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_13_935_p,       # V1 BN Present
           ksads_13_936_p,       # V1 BN Past
           ksads_13_937_p,       # V1 BN Remission
           ksads2_13_896_p,      # V2 BN Present
           ksads2_13_897_p,      # V2 BN Past
           ksads2_13_898_p))     # V2 BN Remission

BNY <- full_join(K1Y, K2Y, 
                 by=c("src_subject_id"="src_subject_id",
                      "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_13_935_t,       # V1 BN Present
           ksads_13_936_t,       # V1 BN Past
           ksads_13_937_t,       # V1 BN Remission
           ksads2_13_896_t,      # V2 BN Present
           ksads2_13_897_t,      # V2 BN Past
           ksads2_13_898_t))     # V2 BN Remission

table(stack(select(BNP, starts_with("ksads")))$values, 
      useNA="always")

table(stack(select(BNY, starts_with("ksads")))$values, 
      useNA="always")

BNP_V <- BNP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "1_year_follow_up_y_arm_1",
             "2_year_follow_up_y_arm_1",
             "3_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BNY_V <- BNY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BNP <- BNP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BNP_V, by="src_subject_id")

BNY <- BNY %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BNY_V, by="src_subject_id")

BN <- full_join(BNP, BNY, by="src_subject_id") %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(TC=TC.x+TC.y,
         AS=AS.x+AS.y,
         .keep="unused") %>%
  mutate(nBN=as.numeric(TC>(AS/2)),
         bnBN=as.numeric(TC>0),
         .keep="unused")

table(Narrow=BN$nBN, Broad=BN$bnBN)

DAT <- DAT %>%
  left_join(BN, by="src_subject_id")

rm(BN, BNP, BNY, BNP_V, BNY_V)

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_13_938_p - BED Present [F50.8]
# ksads_13_939_p - BED Remission [F50.8] 
# ksads_13_940_p - BED Past [F50.8]
# ksads2_13_899_p - BED Present [F50.8]
# ksads2_13_900_p - BED Past [F50.8]
#
# ksads_13_938_t - BED Present [F50.8]
# ksads_13_939_t - BED Remission [F50.8] 
# ksads_13_940_t - BED Past [F50.8]
# ksads2_13_899_t - BED Present [F50.8]
# ksads2_13_900_t - BED Past [F50.8]
#
# Abbreviations:
# BED - binge-eating disorder
#------------------------------------------------------------------------------

BEDP <- full_join(K1P, K2P, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_13_938_p,       # V1 BED Present
           ksads_13_939_p,       # V1 BED Remission
           ksads_13_940_p,       # V1 BED Past
           ksads2_13_899_p,      # V2 BED Present
           ksads2_13_900_p))     # V2 BED Past

BEDY <- full_join(K1Y, K2Y, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_13_938_t,       # V1 BED Present
           ksads_13_939_t,       # V1 BED Remission
           ksads_13_940_t,       # V1 BED Past
           ksads2_13_899_t,      # V2 BED Present
           ksads2_13_900_t))     # V2 BED Past

table(stack(select(BEDP, starts_with("ksads")))$values, 
      useNA="always")

table(stack(select(BEDY, starts_with("ksads")))$values, 
      useNA="always")

BEDP_V <- BEDP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "1_year_follow_up_y_arm_1",
             "2_year_follow_up_y_arm_1",
             "3_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BEDY_V <- BEDY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

BEDP <- BEDP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BEDP_V, by="src_subject_id")

BEDY <- BEDY %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(BEDY_V, by="src_subject_id")

BED <- full_join(BEDP, BEDY, by="src_subject_id") %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(TC=TC.x+TC.y,
         AS=AS.x+AS.y,
         .keep="unused") %>%
  mutate(nBED=as.numeric(TC>(AS/2)),
         bnBED=as.numeric(TC>0),
         .keep="unused")

table(Narrow=BED$nBED, Broad=BED$bnBED)

DAT <- DAT %>%
  left_join(BED, by="src_subject_id")

rm(BED, BEDP, BEDY, BEDP_V, BEDY_V)

# 3.15. ATTENTION DEFICIT / HYPERACTIVITY DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, data removed due to programming errors.
#------------------------------------------------------------------------------

# 3.16. OPPOSITIONAL DEFIANT DISORDER DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_15_901_p - ODD Present [F90]
# ksads_15_902_p - ODD Past [F90]
# ksads2_15_859_p - ODD Present [F90]
# ksads2_15_860_p - ODD Past [F90]
#
# Abbreviations:
# ODD - oppositional defiant disorder
#------------------------------------------------------------------------------

ODDP <- full_join(K1P, K2P, 
                  by=c("src_subject_id"="src_subject_id",
                       "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_15_901_p,       # V1 ODD Present
           ksads_15_902_p,       # V1 ODD Past
           ksads2_15_859_p,      # V2 ODD Present
           ksads2_15_860_p))     # V2 ODD Past

table(stack(select(ODDP, starts_with("ksads")))$values, 
      useNA="always")

ODDP_V <- ODDP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "1_year_follow_up_y_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

ODDP <- ODDP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(ODDP_V, by="src_subject_id")

ODD <- ODDP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nODD=as.numeric((TC>(AS/2)) & (AS>1)),
         bnODD=as.numeric(TC>0),
         .keep="unused")

table(Narrow=ODD$nODD, Broad=ODD$bnODD)

DAT <- DAT %>%
  left_join(ODD, by="src_subject_id")

rm(ODD, ODDP, ODDP_V)

# 3.17. CONDUCT DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_16_897_p - CD Present Childhood Onset [F91.1]
# ksads_16_898_p - CD Present Adolescent Onset [F91.2]
# ksads_16_899_p - CD Past Childhood Onset [F91.1]
# ksads_16_900_p - CD Past Adolescent Onset [F91.2]
# ksads2_16_855_p - CD Present Childhood Onset [F91.1]
# ksads2_16_856_p - CD Present Adolescent Onset [F91.2]
# ksads2_16_857_p - CD Past Childhood Onset [F91.1]
# ksads2_16_858_p - CD Past Adolescent Onset [F91.2]
#
# ksads_16_897_t - CD Present Childhood Onset [F91.1]
# ksads_16_898_t - CD Present Adolescent Onset [F91.2]
# ksads_16_899_t - CD Past Childhood Onset [F91.1]
# ksads_16_900_t - CD Past Adolescent Onset [F91.2]
# ksads2_16_855_t - CD Present Childhood Onset [F91.1]
# ksads2_16_856_t - CD Present Adolescent Onset [F91.2]
# ksads2_16_857_t - CD Past Childhood Onset [F91.1]
# ksads2_16_858_t - CD Past Adolescent Onset [F91.2]
#
# Abbreviations:
# CD - conduct disorder
#------------------------------------------------------------------------------

CDP <- full_join(K1P, K2P, 
                 by=c("src_subject_id"="src_subject_id",
                      "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_16_897_p,       # V1 CD Present Childhood
           ksads_16_898_p,       # V1 CD Present Adolescent
           ksads_16_899_p,       # V1 CD Past Childhood
           ksads_16_900_p,       # V1 CD Past Adolescent
           ksads2_16_855_p,      # V2 CD Present Childhood
           ksads2_16_856_p,      # V2 CD Present Adolescent
           ksads2_16_857_p,      # V2 CD Past Childhood
           ksads2_16_858_p))     # V2 CD Past Adolescent

CDY <- full_join(K1Y, K2Y, 
                 by=c("src_subject_id"="src_subject_id",
                      "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads_16_897_t,       # V1 CD Present Childhood
           ksads_16_898_t,       # V1 CD Present Adolescent
           ksads_16_899_t,       # V1 CD Past Childhood
           ksads_16_900_t,       # V1 CD Past Adolescent
           ksads2_16_855_t,      # V2 CD Present Childhood
           ksads2_16_856_t,      # V2 CD Present Adolescent
           ksads2_16_857_t,      # V2 CD Past Childhood
           ksads2_16_858_t))     # V2 CD Past Adolescent

table(stack(select(CDP, starts_with("ksads")))$values, 
      useNA="always")

table(stack(select(CDY, starts_with("ksads")))$values, 
      useNA="always")

CDP_V <- CDP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "1_year_follow_up_y_arm_1",
             "2_year_follow_up_y_arm_1",
             "3_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

CDY_V <- CDY %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

CDP <- CDP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(CDP_V, by="src_subject_id")

CDY <- CDY %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(CDY_V, by="src_subject_id")

CD <- full_join(CDP, CDY, by="src_subject_id") %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(TC=TC.x+TC.y,
         AS=AS.x+AS.y,
         .keep="unused") %>%
  mutate(nCD=as.numeric(TC>(AS/2)),
         bnCD=as.numeric(TC>0),
         .keep="unused")

table(Narrow=CD$nCD, Broad=CD$bnCD)

DAT <- DAT %>%
  left_join(CD, by="src_subject_id")

rm(CD, CDP, CDY, CDP_V, CDY_V)

# 3.18. TIC DISORDERS MODULE

#------------------------------------------------------------------------------
# NOTE:
# There are no available diagnoses in this modules. Instead, we will construct 
# a tic disorder diagnosis as follows:
# presence of motor or vocal tics = broad diagnosis
# presence of motor and vocal tics = narrow diagnosis
# 
# Available symptoms include:
# ksads2_17_99_p - Motor Tics, Present
# ksads2_17_100_p - Motor Tics, Past
# ksads2_17_101_p - Phonic Tics, Present
# ksads2_17_102_p - Phonic Tics, Past
#
# Abbreviations:
# TD - tic disorder
#------------------------------------------------------------------------------

TDP <- K2P %>%
  select(c(src_subject_id,       # Subject ID
           eventname,            # Event name
           ksads2_17_99_p,       # V2 Motor Tics Present
           ksads2_17_100_p,      # V2 Motor Tics Past
           ksads2_17_101_p,      # V2 Phonic Tics Present
           ksads2_17_102_p))     # V2 Phonic Tics Past

table(stack(select(TDP, starts_with("ksads")))$values, 
      useNA="always")

TDP <- TDP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=sum(value)) 

TD <- TDP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nTD=as.numeric(LC>1),
         bnTD=as.numeric(LC>0),
         .keep="unused")

table(Narrow=TD$nTD, Broad=TD$bnTD)

DAT <- DAT %>%
  left_join(TD, by="src_subject_id")

rm(TD, TDP, TDP_V)

# 3.19. AUTISM SPECTRUM DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, data removed due to programming errors.
#------------------------------------------------------------------------------

# 3.20. ALCOHOL USE DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, out of purview.
#------------------------------------------------------------------------------

# 3.21. DRUG USE DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, out of purview.
#------------------------------------------------------------------------------

# 3.22. POST-TRAUMATIC STRESS DISORDER MODULE

#------------------------------------------------------------------------------
# NOTE:
# Available diagnoses include:
# ksads_21_921_p - PTSD Present [F94.1]
# ksads_21_922_p - PTSD Past [F94.1]
# ksads2_21_881_p - PTSD Present [F94.1]
# ksads2_21_882_p - PTSD Remission [F94.1]
# ksads2_21_883_p - PTSD Past [F94.1]
#
# Abbreviations:
# PTSD - post-traumatic stress disorder
#------------------------------------------------------------------------------

PTSDP <- full_join(K1P, K2P, 
                   by=c("src_subject_id"="src_subject_id",
                        "eventname"="eventname")) %>%
  select(c(src_subject_id,       # Subject ID
           eventname,             # Event name
           ksads_21_921_p,        # V1 PTSD Present
           ksads_21_922_p,        # V1 PTSD Past
           ksads2_21_881_p,       # V2 PTSD Present
           ksads2_21_882_p,       # V2 PTSD Remission
           ksads2_21_883_p))      # V2 PTSD Past

table(stack(select(PTSDP, starts_with("ksads")))$values, 
      useNA="always")

PTSDP_V <- PTSDP %>%
  select(c(src_subject_id, eventname)) %>%
  filter(eventname %in%
           c("baseline_year_1_arm_1",
             "2_year_follow_up_y_arm_1")) %>%
  group_by(src_subject_id) %>%
  summarise(AS=n())

PTSDP <- PTSDP %>%
  mutate(across(starts_with("ksads"),
                ~replace(., which(. %in% c(555, 888)), NA))) %>%
  pivot_longer(cols=starts_with("ksads")) %>% 
  filter(!is.na(value)) %>%
  mutate(value=as.numeric(value)) %>%
  group_by(src_subject_id, eventname) %>%
  summarise(LC=as.numeric(sum(value)>0)) %>%
  group_by(src_subject_id) %>%
  summarise(TC=sum(LC)) %>%
  full_join(PTSDP_V, by="src_subject_id")

PTSD <- PTSDP %>%
  mutate(across(where(is.numeric),
                ~replace(., which(is.na(.)), 0))) %>%
  mutate(nPTSD=as.numeric((TC>(AS/2)) & (AS>1)),
         bnPTSD=as.numeric(TC>0),
         .keep="unused")

table(Narrow=PTSD$nPTSD, Broad=PTSD$bnPTSD)

DAT <- DAT %>%
  left_join(PTSD, by="src_subject_id")

rm(PTSD, PTSDP, PTSDP_V)

# 3.23. SLEEP PROBLEMS MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, out of purview.
#------------------------------------------------------------------------------

# 3.24. SUICIDALITY MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, out of purview.
#------------------------------------------------------------------------------

# 3.25. HOMICIDALITY MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, out of purview.
#------------------------------------------------------------------------------

# 3.26. SELECTIVE MUTISM MODULE

#------------------------------------------------------------------------------
# NOTE:
# Skipped, because module was not administered to participants at any point.
#------------------------------------------------------------------------------

###############################
## PART 4. ANALYZE DIAGNOSES ##
###############################

# 4.1. DEFINE BROAD AND NARROW DATA FRAMES
Broad <- DAT %>%
  select(c(src_subject_id, starts_with("b")))

Narrow <- DAT %>%
  select(c(src_subject_id, starts_with("n")))

# 4.2. CREATE PREVALENCE AND COMORBIDITY TABLES
NarrowNames <- colnames(Narrow[,-1])
BroadNames <- colnames(Broad[,-1])

Types <- c("Narrow", "Broad")

Results <- list()

Strata <- c("Full")

StrataID <- list()

StrataID[["Full"]] <- Broad$src_subject_id

for(z in Strata){
  Prevs <- data.frame(
    Disorder=as.character(NULL),
    Type=as.character(NULL),
    Count=as.character(NULL),
    P=as.numeric(NULL),
    L95=as.numeric(NULL),
    U95=as.numeric(NULL),
    stringsAsFactors=FALSE)

  Comos <- data.frame(
    Primary=as.character(NULL),
    Secondary=as.character(NULL),
    Type=as.character(NULL),
    Count=as.character(NULL),
    C=as.numeric(NULL),
    L95=as.numeric(NULL),
    U95=as.numeric(NULL),
    stringsAsFactors=FALSE)

  KEEP <- StrataID[[z]]

  for(i in Types){
    
    if(i=="Narrow") {
      Names <- NarrowNames
      Data <- Narrow %>%
        filter(src_subject_id %in% KEEP)
    } else if(i=="Broad") {
      Names <- BroadNames
      Data <- Broad %>%
        filter(src_subject_id %in% KEEP)
    }
    
    for(j in Names){
      
      DatA <- factor(pull(Data, j), levels=c(0,1))
      
      VecT <- table(Primary=DatA)
      
      N <- sum(VecT[1:2])
      T <- sum(VecT[2])
      
      pEST <- T / N
      pUP <- pEST + 1.96 * sqrt((pEST*(1-pEST))/N)
      pDO <- pEST - 1.96 * sqrt((pEST*(1-pEST))/N)
      
      pRow <- data.frame(
        Disorder=j,
        Type=i,
        Count=as.character(ifelse(T>=10, T, 0)),
        P=round(pEST*100, digits=1),
        L95=round(pDO*100, digits=1),
        U95=round(pUP*100, digits=1),
        stringsAsFactors=FALSE)
      
      Prevs <- rbind(Prevs, pRow)
      
      for(k in Names){
        
        DatB <- factor(pull(Data, k), levels=c(0, 1))
        
        ConT <- table(Primary=DatA, Secondary=DatB)
        
        M <- sum(ConT[2,1:2])
        D <- sum(ConT[2,2])
        
        cEST <- D / M
        cUP <- cEST + 1.96 * sqrt((cEST*(1-cEST))/M)
        cDO <- cEST - 1.96 * sqrt((cEST*(1-cEST))/M) 
        
        cRow <- data.frame(
          Primary=j,
          Secondary=k,
          Type=i,
          Count=as.character(ifelse(D>=10, D, 0)),
          C=round(cEST*100, digits=1),
          L95=round(cDO*100, digits=1),
          U95=round(cUP*100, digits=1),
          stringsAsFactors=FALSE)
        
        Comos <- rbind(Comos, cRow)
      }
    }
  }

  Prevs <- Prevs %>%
    mutate(across(where(is.numeric),
                  ~replace(., which(.<0), 0))) %>%
    mutate(across(where(is.numeric),
                  ~replace(., which(.>100), 100))) %>%
    mutate(across(where(is.numeric),
                  ~replace(., which(is.nan(.)), 0)))

  Comos <- Comos %>%
    mutate(across(where(is.numeric),
                  ~replace(., which(.<0), 0))) %>%
    mutate(across(where(is.numeric),
                  ~replace(., which(.>100), 100))) %>%
    mutate(across(where(is.numeric),
                  ~replace(., which(is.nan(.)), 0)))

  Results[[z]][["Prevalences"]] <- Prevs
  Results[[z]][["Comorbidities"]] <- Comos
}

# 4.3. OUTPUT UNSTRATIFIED RESULTS

Prevs <- Results[["Full"]][["Prevalences"]] 
Comos <- Results[["Full"]][["Comorbidities"]]

filter(Prevs, Type=="Narrow") %>%
  select(-Type) %>%
  mutate("Prev (CI95)"=paste0(P, " (", L95, ", ", U95,")"),
         .keep="unused") 

filter(Prevs, Type=="Broad") %>%
  select(-Type) %>%
  mutate("Prev (CI95)"=paste0(P, " (", L95, ", ", U95,")"),
         .keep="unused")

filter(Comos, Type=="Narrow" & Primary=="nOCD") %>%
  select(-Type) %>%
  mutate("Como (CI95)"=paste0(C, " (", L95, ", ", U95,")"),
         .keep="unused") 

filter(Comos, Type=="Narrow" & Secondary=="nOCD")  %>%
  select(-Type) %>%
  mutate("Como (CI95)"=paste0(C, " (", L95, ", ", U95,")"),
         .keep="unused") 

filter(Comos, Type=="Broad" & Primary=="bnOCD")  %>%
  select(-Type) %>%
  mutate("Como (CI95)"=paste0(C, " (", L95, ", ", U95,")"),
         .keep="unused") 

filter(Comos, Type=="Broad" & Secondary=="bnOCD")  %>%
  select(-Type) %>%
  mutate("Como (CI95)"=paste0(C, " (", L95, ", ", U95,")"),
         .keep="unused")

###############################
## PART 5. ANALYZE CBCL DATA ##
###############################

# 5.1. PREPARE CBCL DATASET
ID_KEEP <- intersect(intersect(ID_BL, ID_F1), ID_F2)

CBCL_OC <- CBCL %>%
  filter(src_subject_id %in% ID_KEEP) %>%
  filter(eventname!="3_year_follow_up_y_arm_1") %>%
  select(c(src_subject_id,
           cbcl_q09_p,   # Can't get their mind off certain thoughts; obsessions
           cbcl_q31_p,   # Fears they might think or do something bad
           cbcl_q52_p,   # Feels too guilty
           cbcl_q66_p,   # Repeats certain acts over and over; compulsions
           cbcl_q85_p,   # Strange ideas
           cbcl_q112_p)) # Worries

table(stack(select(CBCL_OC, starts_with("cbcl")))$values, 
      useNA="always")

ID_DROP <- CBCL_OC %>%
  filter(if_any(everything(), ~ is.na(.))) %>%
  pull(src_subject_id) %>%
  unique(.)

CBCL_OC <- CBCL_OC %>%
  filter(!src_subject_id %in% ID_DROP) %>%
  mutate(across(starts_with("cbcl_q"),
                ~as.numeric(.))) %>%
  mutate(OCS=cbcl_q09_p+cbcl_q66_p,
         OCP=cbcl_q09_p+cbcl_q31_p+cbcl_q52_p+cbcl_q66_p+cbcl_q85_p+cbcl_q112_p,
         OBS=cbcl_q09_p,
         COM=cbcl_q66_p,
         .keep="unused") %>%
  group_by(src_subject_id) %>%
  summarise(OCS=sum(OCS),
            OCP=sum(OCP),
            OBS=sum(OBS),
            COM=sum(COM)) %>%
  left_join(select(DAT, src_subject_id, nOCD, bnOCD, nTD, bnTD), 
            by="src_subject_id") %>%
  mutate(OCD_Text = case_when(
    bnOCD==0 ~ "No OCD",
    bnOCD==1 & nOCD==0 ~ "Broad + Narrow OCD",
    bnOCD==1 & nOCD==1 ~ "Narrow OCD"),
    TD_Text = case_when(
      bnTD==0 ~ "TD Negative",
      bnTD==1 & nTD==0 ~ "Broad + Narrow TD",
      bnTD==1 & nTD==1 ~ "Narrow TD")) %>%
  left_join(select(filter(DB, eventname=="baseline_year_1_arm_1"), 
                   src_subject_id, demo_sex_v2), 
            by="src_subject_id") %>%
  rename(sex=demo_sex_v2) %>%
  filter(sex>0 & sex<3)

CBCL_OC$OCD_Text <- factor(CBCL_OC$OCD_Text, 
                           levels=c("No OCD", "Broad + Narrow OCD", "Narrow OCD"))
CBCL_OC$TD_Text <- factor(CBCL_OC$TD_Text, 
                           levels=c("TD Negative", "Broad + Narrow TD", "Narrow TD"))

# 5.2 RUN LOGISTIC REGRESSIONS ON CBCL DATA
LOGIT_CBCL <- list()

OCD_OR <- data.frame(
  CBCL=as.character(NULL),
  OCD=as.character(NULL),
  OR=as.numeric(NULL),
  L95=as.numeric(NULL),
  U95=as.numeric(NULL),
  P=as.numeric(NULL),
  K=as.numeric(NULL),
  ACC=as.numeric(NULL),
  SEN=as.numeric(NULL),
  SPE=as.numeric(NULL),
  PPV=as.numeric(NULL),
  NPV=as.numeric(NULL),
  F1=as.numeric(NULL),
  stringsAsFactors=FALSE)

for(i in c("OCS", "OCP", "OBS", "COM")) {
  for(j in c("nOCD", "bnOCD")) {
    FORMULA <- paste0("as.factor(", j, ") ~ ", i, "+ sex")
    LOGIT_CBCL[[i]][[j]] <- glm(as.formula(FORMULA), 
                                family=binomial(link="logit"), 
                                data=CBCL_OC,
                                na.action=na.exclude)
    
    CBCL_OC$PRED <- as.numeric(predict(LOGIT_CBCL[[i]][[j]], 
                                       type="response")>=0.5)
    
    CBCL_OC$REF <- CBCL_OC[[j]]
    
    CONFMAT <- confusionMatrix(data=factor(CBCL_OC$PRED, levels=c(0,1)), 
                               reference=factor(CBCL_OC$REF, levels=c(0,1)), 
                               positive="1")
    
    TAB <- data.frame(
      CBCL=i,
      OCD=j,
      OR=exp(coef(LOGIT_CBCL[[i]][[j]]))[i],
      L95=exp(confint(LOGIT_CBCL[[i]][[j]]))[i, "2.5 %"],
      U95=exp(confint(LOGIT_CBCL[[i]][[j]]))[i, "97.5 %"],
      P=summary(LOGIT_CBCL[[i]][[j]])$coefficients[i,4],
      K=CONFMAT$overall["Kappa"],
      ACC=CONFMAT$overall["Accuracy"],
      SEN=CONFMAT$byClass["Sensitivity"],
      SPE=CONFMAT$byClass["Specificity"],
      PPV=CONFMAT$byClass["Pos Pred Value"],
      NPV=CONFMAT$byClass["Neg Pred Value"],
      F1=CONFMAT$byClass["F1"],
      stringsAsFactors=FALSE)
    
    OCD_OR <- rbind(OCD_OR, TAB)
  }
}

OCD_OR <- OCD_OR %>%
  mutate(across(OR:F1, ~ as.numeric(.))) %>%
  mutate(Q=p.adjust(P, method="fdr"),
         .after=P)

###########################
## PART 6. PLOT FIGURE 2 ##
###########################

# 6.1. PREPARE DATA FOR PLOTTING
PlotPrev <- Prevs %>%
  mutate(Disorder=gsub("n|bn", "", Disorder)) %>%
  mutate(Type=ifelse(Type=="Broad","Broad + Narrow",Type)) %>%
  select(-Count) %>%
  bind_rows(REF_PREV) %>%
  mutate(Type2=ifelse(Type %in% c("Narrow", "Broad + Narrow"), Type, "Reference")) %>%
  filter(complete.cases(.))

PlotPrev$Type <- factor(PlotPrev$Type, 
                        levels=c("Narrow", "Broad + Narrow", "Cross-sectional reference", "Longitudinal reference"))

PlotPrev$Type2 <- factor(PlotPrev$Type2, 
                        levels=c("Narrow", "Broad + Narrow", "Reference"))

PlotPrev$Disorder <- factor(PlotPrev$Disorder, 
                            levels=c("BDI", "BDII", "BD", "PSY", "PD", 
                                     "SAD", "SOPH", "SP", "GAD", "OCD", 
                                     "BN", "BED", "ODD", "CD", "TD", "PTSD"))
OCD_OR$CBCL <- factor(OCD_OR$CBCL, 
                      levels=c("OCS", "OCP", "OBS", "COM"))

# 6.2. PLOT FIGURE 2A
F2A <- ggplot(mapping=aes(x=Disorder, y=P, ymax=U95, ymin=L95, 
                          fill=Type2, width=0.9, color=Type2)) +
  geom_bar(data=PlotPrev[PlotPrev$Type=="Cross-sectional reference",], 
           stat="identity", linewidth=0.7) +
  geom_errorbar(data=PlotPrev[PlotPrev$Type=="Cross-sectional reference",],
                linewidth=0.7) +
  geom_bar(data=PlotPrev[PlotPrev$Type %in% c("Narrow", "Broad + Narrow"),], 
           position=position_dodge2(preserve="single", padding=0.3), 
           stat="identity", alpha=0.8, linewidth=0.7) +
  geom_errorbar(data=PlotPrev[PlotPrev$Type %in% c("Narrow", "Broad + Narrow"),], 
                position=position_dodge2(preserve="single", padding=0.3),
                linewidth=0.7) +
  labs(x=NULL, y="PREVALENCE (%)", title=NULL, fill=NULL, color=NULL) +
  scale_fill_manual(values=c("#adb5bd", "#EB4E4E", "#77B6EA")) +
  scale_color_manual(values=c("#666666", "#931010", "#1C6DB0")) +
  scale_y_continuous(n.breaks=5,
                     expand=expansion(mult=0.01)) +
  theme_bw()  +
  theme(axis.text.x=element_text(angle=35, vjust=1, hjust=1, size=15),
        axis.title=element_text(size=15, face="bold", hjust=0),
        axis.text.y=element_text(angle=90, hjust=0.5, size=15),
        legend.position=c(0,0.95),
        legend.justification="left",
        legend.direction="horizontal",
        legend.title=element_text(vjust=1, size=15),
        legend.text=element_text(size=15),
        legend.background=element_blank())
F2A  

# 6.3. PLOT FIGURE 2B
CBCL_HIST <- list()

for(i in c("OCS", "OCP", "OBS", "COM")){
  DF <- CBCL_OC[, c(i, "OCD_Text")]
  DF$X <- DF[[i]]
  
  CBCL_HIST[[i]] <- 
    DF %>%
    filter(complete.cases(.)) %>%
    ggplot(aes(x=X)) +
    geom_histogram(binwidth=1, color="#ffffff", fill="#77B6EA") +
    theme_bw() +
    scale_y_continuous(n.breaks=3,
                       expand=expansion(mult=0.025)) +
    scale_x_continuous(n.breaks=4,
                       expand=expansion(mult=0.015)) +
    theme(axis.text.x=element_text(size=15),
          plot.title=element_text(hjust=0, size=15, face="bold"),
          axis.title=element_text(size=15),
          axis.text.y=element_text(angle=45, hjust=0.5, size=15),
          legend.position="bottom",
          legend.title=element_text(vjust=1, size=15),
          legend.text=element_text(size=15)) +
    facet_wrap(. ~ OCD_Text, ncol=3, scales="free_y", strip.position="bottom")
  
  if(i=="COM"){
    CBCL_HIST[[i]] <- CBCL_HIST[[i]] +
      labs(y="COUNTS", x=NULL, title=i) +
      theme(strip.background = element_rect(fill="transparent", 
                                            color="transparent"),
            strip.text=element_text(face="bold", size=15, hjust=0),
            strip.placement="outside",
            axis.title.y=element_text(hjust=0, face="bold"))} 
  else{
    CBCL_HIST[[i]] <- CBCL_HIST[[i]] +
      labs(y=" ", x=NULL, title=i) +
      theme(strip.background = element_blank(),
            strip.text=element_blank())}
}

F2B <- plot_grid(CBCL_HIST$OCS, 
                 CBCL_HIST$OCP, 
                 CBCL_HIST$OBS, 
                 CBCL_HIST$COM, 
                 rel_heights=c(1,1,1,1.2),
                 ncol=1, label_size = 18)

F2B

# 6.4. PLOT FIGURE 2C
F2C <- OCD_OR %>% 
  ggplot(aes(x=OR, xmin=L95, xmax=U95, y=OCD)) +
  geom_errorbar(width=.4, color="#77B6EA", size=1) +
  geom_point(shape=18, color="#1C6DB0", size=5) +
  geom_vline(xintercept=1, linetype="dashed", color="#931010", size=1) +
  labs(y=NULL, x="ODDS RATIO") +
  scale_x_continuous(breaks=seq(0.5, 3.5, 0.5), limits=c(0.9, 3.6),
                     expand=expansion(mult=0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=15),
        plot.title = element_text(hjust=0, size=18, face="bold"),
        axis.title = element_text(size=15, hjust=0, face="bold"),
        axis.text.y = element_text(angle=90, hjust=0.5, size=15),
        strip.background = element_rect(fill="transparent", color="transparent"),
        strip.text = element_text(face="bold", size=15, hjust=0),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_text(vjust=1, size=18),
        legend.text = element_text(size=15)) +
  facet_wrap(.~CBCL, ncol=1)

F2C

# 6.5. ORGANIZE AND SAVE FIGURE 2
plot_grid(F2A, plot_grid(F2B, F2C, 
                         rel_widths=c(2.3,1), ncol=2, labels=c("B", "C"), label_size=20),
          rel_heights=c(2,3), ncol=1, labels=c("A",""), label_size=20)

ggsave(paste0("./", VER, "/Figure2.png"), height=15, width=12, 
       device="png", units="in", dpi=300, plot=last_plot(), 
       bg="white")

############################
## PART 7. PLOT FIGURE S2 ##
############################

# 7.1. PLOT DATA
FS2 <- PlotPrev %>%
  ggplot(mapping=aes(x=Disorder, y=P, ymax=U95, ymin=L95, 
                     fill=Type, width=0.9, color=Type)) +
  geom_bar(position=position_dodge2(preserve="single", padding=0.3), 
           stat="identity", alpha=0.8) +
  geom_errorbar(position=position_dodge2(preserve="single", padding=0.3)) +
  labs(x=NULL, y="PREVALENCE (%)", title=NULL, fill=NULL, color=NULL) +
  scale_fill_manual(values=c("#EB4E4E", "#77B6EA", "#FFC482", "#35D4B1")) +
  scale_color_manual(values=c("#931010", "#1C6DB0", "#E07800", "#197662")) +
  scale_y_continuous(n.breaks=5,
                     limits=c(0,40)) +
  theme_bw()  +
  theme(axis.text.x=element_text(angle=35, vjust=1, hjust=1, size=15),
        axis.title=element_text(size=15, face="bold", hjust=0),
        axis.text.y=element_text(angle=90, hjust=0.5, size=15),
        legend.position=c(0,0.95),
        legend.justification="left",
        legend.direction="horizontal",
        legend.title=element_text(vjust=1, size=15),
        legend.text=element_text(size=15),
        legend.background=element_blank())
FS2  

# 7.2. SAVE PLOT
ggsave(paste0("./", VER, "/FigureS2.png"), height=5, width=10, 
       device="png", units="in", dpi=300, plot=last_plot(), 
       bg="white")

############################
## PART 8. PLOT FIGURE S3 ##
############################

# 8.1. PREPARE DATA FOR PLOTTING
REF_COM <- REF_COM %>%
  mutate(Type="Reference",
         .after=Secondary) %>%
  rename(C=P)

PlotComo <- Comos %>%
  filter(Type=="Narrow" & (Primary=="nOCD" | Secondary=="nOCD") & !(Primary=="nOCD" & Secondary=="nOCD")) %>%
  mutate(Primary=gsub("n|bn", "", Primary),
         Secondary=gsub("n|bn", "", Secondary)) %>%
  bind_rows(REF_COM) %>%
  filter(Primary!="BN"&Secondary!="BN")

# 8.2. PLOT DATA
PlotComoP <- PlotComo %>%
  filter(Primary=="OCD") %>%
  ggplot(aes(fill=Type, color=Type, y=C, ymin=L95, ymax=U95, x=Secondary)) + 
  geom_bar(position=position_dodge2(preserve="single", padding=0.3), 
           stat="identity", alpha=0.8, linewidth=0.7) +
  geom_errorbar(position=position_dodge2(preserve="single", padding=0.3),
                linewidth=0.7) +
  scale_fill_manual(values=c("#EB4E4E", "#adb5bd")) +
  scale_color_manual(values=c("#931010", "#666666")) +
  aes(stringr::str_wrap(Secondary, 15)) +
  labs(x = NULL, y = "Cormorbidity Rate (%)", title = "OCD Primary", fill="OCD diagnosis", color="OCD diagnosis") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1, size=15),
        plot.title = element_text(face="bold.italic", size=15, hjust=0, vjust=0),
        axis.title = element_text(size=15),
        axis.text.y = element_text(angle=90, hjust=0.5, size=15),
        legend.position = "none")

PlotComoS <- PlotComo %>%
  filter(Secondary=="OCD") %>%
  ggplot(aes(fill=Type, color=Type, y=C, ymin=L95, ymax=U95, x=Primary)) + 
  geom_bar(position=position_dodge2(preserve="single", padding=0.3), 
           stat="identity", alpha=0.8, linewidth=0.7) +
  geom_errorbar(position=position_dodge2(preserve="single", padding=0.3),
                linewidth=0.7) +
  scale_fill_manual(values=c("#EB4E4E", "#adb5bd")) +
  scale_color_manual(values=c("#931010", "#666666")) +
  coord_cartesian(ylim=c(0,100)) +
  aes(stringr::str_wrap(Primary, 15)) +
  labs(x = NULL, y = "Cormorbidity Rate (%)", title = "OCD Secondary", fill="OCD diagnosis", color="OCD diagnosis") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1, size=15),
        plot.title = element_text(face="bold.italic", size=15, hjust=0, vjust=0),
        axis.title = element_text(size=15),
        axis.text.y = element_text(angle=90, hjust=0.5, size=15),
        legend.title = element_text(vjust=1, size=18, margin=margin(r=15, unit="pt")),
        legend.text = element_text(size=15, margin=margin(r=15, unit="pt")),
        legend.position="bottom",
        legend.justification="left",
        legend.direction="horizontal",
        legend.background=element_blank())

plot_grid(PlotComoP, PlotComoS, labels=c("A", "B"), ncol=1, label_size = 18)

# 8.3. SAVE PLOT
ggsave(paste0("./", VER, "/FigureS3.png"), height=15, width=13, 
       device="png", units="in", dpi=300, plot=last_plot(), 
       bg="white")

#########################
## PART 9. EXPORT DATA ##
#########################

# 8.1. EXPORT PREVALENCE AND COMORBIDITY TABLES
write.xlsx(Results[["Full"]][["Prevalences"]], file = "SupplementalData.xlsx",
           sheetName = "ABCD_PREV", append = TRUE)

write.xlsx(Results[["Full"]][["Comorbidities"]], file = "SupplementalData.xlsx",
           sheetName = "ABCD_COMO", append = TRUE)
