# #############################################
# Analysis Script for Hyko Study 1
# 15.05.26
# #############################################

#necessary packages
library(dplyr)
library(psych)


#importing data from csv file via script
source("import_efre_hyko_2026-05-15_14-18.r")
# renaming dataset ds

data_basic <- ds # data_basic is default name of dataset

# ######################
# preparing and cleaning dataset
# ######################


sum(data_basic$FINISHED == TRUE, na.rm = TRUE) # number of people who finished study; last page of sosci reached; gives 382 participants
sum(!is.na(data_basic$TIME035)) # number of people who spent time on page 35; ; gives 380 participants

# difference explained by:
# CASE values where FINISHED is TRUE but TIME035 is NA
data_basic$CASE[data_basic$FINISHED == TRUE & is.na(data_basic$TIME035)] # participant 501 and 515 reach last page but do not spend time on it
# reason is that those were filtered ("Aufgrund der Auswahlkritiern abgewiesen")

# can also be seen here
split(data_basic$CASE, data_basic$IF01) # IF01 gives response to consent; 501 + 515 did not give consent

##
# taken study seriously
##

table(data_basic$CN05, useNA = "ifany") # taken seriously by 378; not taken seriously by 2; 10 that did not answer

##
# attention checks
##

table(data_basic$CN09, useNA = "ifany") # 14 that answered Manip_Check 1 wrong; 7 that did not answer at all
table(data_basic$CN15, useNA = "ifany") # 109 that answered Manip_Check 2 wrong; 7 that did not answer at all


###########################
# Computing variables
###########################

##
# ADS-L (Allgemeine Depressionsskala)
#

# scale on sosci seems to already have items as reverse-coded

# occuring values
data_basic %>%
  select(HU_ADS_01, HU_ADS_02, HU_ADS_03, HU_ADS_04, HU_ADS_05, HU_ADS_06, HU_ADS_07, HU_ADS_08, HU_ADS_09, HU_ADS_10,
         HU_ADS_11, HU_ADS_12, HU_ADS_13, HU_ADS_14, HU_ADS_15, HU_ADS_16, HU_ADS_17, HU_ADS_18, HU_ADS_19, HU_ADS_20) %>%
  unlist() %>%
  unique() 

# correlation matrix to see whether reverse items are scored in correct way

data_basic %>%
  select(
    HU_ADS_01, HU_ADS_02, HU_ADS_03, HU_ADS_04, HU_ADS_05,
    HU_ADS_06, HU_ADS_07, HU_ADS_08, HU_ADS_09, HU_ADS_10,
    HU_ADS_11, HU_ADS_12, HU_ADS_13, HU_ADS_14, HU_ADS_15,
    HU_ADS_16, HU_ADS_17, HU_ADS_18, HU_ADS_19, HU_ADS_20
  ) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(
    HU_ADS_01, HU_ADS_02, HU_ADS_03, HU_ADS_04, HU_ADS_05,
    HU_ADS_06, HU_ADS_07, HU_ADS_08, HU_ADS_09, HU_ADS_10,
    HU_ADS_11, HU_ADS_12, HU_ADS_13, HU_ADS_14, HU_ADS_15,
    HU_ADS_16, HU_ADS_17, HU_ADS_18, HU_ADS_19, HU_ADS_20
  ) %>%  psych::alpha()


##
# AT (Avatar Trust Scale: Automation)
#

# are there subscales to be differentiated between?
# to be reverse-coded: 04, 05, 06, 07, 09, 11, 17, 18

# occuring values
data_basic %>%
  select(AT01_01, AT01_02, AT01_03, AT01_04, AT01_05, AT01_06, AT01_07, AT01_08, AT01_09, AT01_10,
         AT01_11, AT01_12, AT01_13, AT01_14, AT01_15, AT01_16, AT01_17, AT01_18) %>%
  unlist() %>%
  unique() 

#reverse code

data_basic <- data_basic %>%
  mutate(across(
    c(AT01_04, AT01_05, AT01_06, AT01_07, AT01_09, AT01_11, AT01_17, AT01_18),
    ~ ifelse(is.na(.x), NA, 6 - as.numeric(.x))
  ))

# correlation matrix to see whether reverse items are scored in correct way

data_basic %>%
  select(
    AT01_01, AT01_02, AT01_03, AT01_04, AT01_05, AT01_06, AT01_07, AT01_08, AT01_09, AT01_10,
    AT01_11, AT01_12, AT01_13, AT01_14, AT01_15, AT01_16, AT01_17, AT01_18) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(
    AT01_01, AT01_02, AT01_03, AT01_04, AT01_05, AT01_06, AT01_07, AT01_08, AT01_09, AT01_10,
    AT01_11, AT01_12, AT01_13, AT01_14, AT01_15, AT01_16, AT01_17, AT01_18  ) %>%  psych::alpha()


##
# BFI-K (Big Five)
##


test





