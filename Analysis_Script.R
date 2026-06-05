# #############################################
# Analysis Script for Hyko Study 1
# 15.05.26
# #############################################

#necessary packages
library(dplyr)
library(psych)
library(stringr)


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
# technical problems
##

table(data_basic$CN03, useNA = "ifany") # 374 without technical difficulties; 6 with technical difficulties; ; 10 that did not answer



##
# attention checks beginning
##

table(data_basic$CN09, useNA = "ifany") # 14 that answered Manip_Check 1 wrong; 7 that did not answer at all
table(data_basic$CN15, useNA = "ifany") # 109 that answered Manip_Check 2 wrong; 7 that did not answer at all

##
# attention checks hidden
##

table(data_basic$BF01_22, useNA = "ifany") # 2 that answered Manip_Check BFI-K wrong; 10 that did not answer at all; correct is 5
table(data_basic$CS01_25, useNA = "ifany") # 7 that answered Manip_Check BFI-K wrong; 10 that did not answer at all; correct is 1

##
# Demographics
##

# Sex
table(data_basic$DM02)

# Gender
table(data_basic$DM03)

# Education
table(data_basic$DM06)

### note: there are more detailed demos such as current job etc.; can be added here

###########################
# Computing variables
###########################

##
# ADS-L (Allgemeine Depressionsskala)
#

# scale on sosci seems to already have items as reverse-coded --> already reverse coded on SoSci: Items 4,8,12,16

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


# to be reverse-coded: ???

# occuring values
data_basic %>%
  select(BF01_01, BF01_02, BF01_03, BF01_04, BF01_05, BF01_06, BF01_07, BF01_08, BF01_09, BF01_10,
         BF01_11, BF01_12, BF01_13, BF01_14, BF01_15, BF01_16, BF01_17, BF01_18, BF01_19, BF01_20,
         BF01_21) %>%
  unlist() %>%
  unique() 

#reverse code

# correlation matrix to see whether reverse items are scored in correct way
# Cronbach's alpha


##
# CN01 --> Wie viel wissen Sie bereits über Automatisierung?
##
# change factor to numeric

data_basic <- data_basic %>%
  mutate(
    CN01 = str_extract(as.character(CN01), "\\d+") %>%
      as.numeric()
  )

# occuring values
data_basic %>%
  select(CN01) %>%
  unlist() %>%
  unique() 


##
# CN02 --> Wie viel Erfahrung haben Sie bereits mit Automatisierung?
##


data_basic <- data_basic %>%
  mutate(
    CN02 = str_extract(as.character(CN02), "\\d+") %>%
      as.numeric()
  )

# occuring values
data_basic %>%
  select(CN02) %>%
  unlist() %>%
  unique() 


##
# Copinginventar
##


# to be reverse-coded: ???

# occuring values
data_basic %>%
  select(CS01_01, CS01_02, CS01_03, CS01_04, CS01_05, CS01_06, CS01_07, CS01_08, CS01_09, CS01_10,
         CS01_11, CS01_12, CS01_13, CS01_14, CS01_15, CS01_16, CS01_17, CS01_18, CS01_19, CS01_20,
         CS01_22, CS01_22, CS01_23, CS01_24) %>%
  unlist() %>%
  unique() 

#reverse code

# correlation matrix to see whether reverse items are scored in correct way

data_basic %>%
  select(
    CS01_01, CS01_02, CS01_03, CS01_04, CS01_05, CS01_06, CS01_07, CS01_08, CS01_09, CS01_10,
    CS01_11, CS01_12, CS01_13, CS01_14, CS01_15, CS01_16, CS01_17, CS01_18, CS01_19, CS01_20,
    CS01_22, CS01_22, CS01_23, CS01_24) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(
    CS01_01, CS01_02, CS01_03, CS01_04, CS01_05, CS01_06, CS01_07, CS01_08, CS01_09, CS01_10,
    CS01_11, CS01_12, CS01_13, CS01_14, CS01_15, CS01_16, CS01_17, CS01_18, CS01_19, CS01_20,
    CS01_22, CS01_22, CS01_23, CS01_24  ) %>%  psych::alpha()



##
# Naughty Nine !!!!nur 8 items!!!! ??????
##


# to be reverse-coded: ???

# occuring values
data_basic %>%
  select(DT01_01, DT01_02, DT01_03, DT01_04, DT01_05, DT01_06, DT01_07, DT01_08) %>%
  unlist() %>%
  unique() 

#reverse code

# correlation matrix to see whether reverse items are scored in correct way

data_basic %>%
  select(DT01_01, DT01_02, DT01_03, DT01_04, DT01_05, DT01_06, DT01_07, DT01_08) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(DT01_01, DT01_02, DT01_03, DT01_04, DT01_05, DT01_06, DT01_07, DT01_08) %>%  psych::alpha()



##
# Self-efficiacy
##

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(GS01_07, GS01_09, GS01_18, GS01_24, GS01_25, GS01_27, GS01_30, GS01_31, GS01_35, GS01_38) %>%
  unlist() %>%
  unique() 

#reverse code

# correlation matrix to see whether reverse items are scored in correct way

data_basic %>%
  select(GS01_07, GS01_09, GS01_18, GS01_24, GS01_25, GS01_27, GS01_30, GS01_31, GS01_35, GS01_38) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(GS01_07, GS01_09, GS01_18, GS01_24, GS01_25, GS01_27, GS01_30, GS01_31, GS01_35, GS01_38) %>%  psych::alpha()


##
# Erfindungen ordnen
##

# 01 = Buchdruck
# 02 = KI
# 03 = Smartphone
# 04 = Internet
# 05 = Automobil
# 06 = Computer
# 07 = Eisenbahn


# occuring values

data_basic %>%
  select(JF02_01, JF02_02, JF02_03, JF02_04, JF02_05, JF02_06, JF02_07) %>%
  unlist() %>%
  unique() 


##
# Nutzung KI außerhalb des Arbeitsplatzes
##

# occuring values

data_basic %>%
  select(JF03_01) %>%
  unlist() %>%
  unique() 


##
# Nutzung KI innerhalb des Arbeitsplatzes
##

# occuring values

data_basic %>%
  select(JF04_01) %>%
  unlist() %>%
  unique() 


##
# PTM-R Prosocial Tendencies
##

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(PT01_04, PT01_10, PT01_16, PT01_20, PT01_23, PT01_24) %>%
  unlist() %>%
  unique() 

#reverse code

# correlation matrix to see whether reverse items are scored in correct way

data_basic %>%
  select(PT01_04, PT01_10, PT01_16, PT01_20, PT01_23, PT01_24) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(PT01_04, PT01_10, PT01_16, PT01_20, PT01_23, PT01_24) %>%  psych::alpha()



##
# Trait Anxiety
##

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(SA01_01, SA01_02, SA01_03, SA01_04, SA01_05, SA01_06, SA01_07, SA01_08, SA01_09, SA01_10,
         SA01_11, SA01_12, SA01_13, SA01_14, SA01_15, SA01_16, SA01_17, SA01_18, SA01_19, SA01_20) %>%
  unlist() %>%
  unique() 

#reverse code

# correlation matrix to see whether reverse items are scored in correct way

data_basic %>%
  select(SA01_01, SA01_02, SA01_03, SA01_04, SA01_05, SA01_06, SA01_07, SA01_08, SA01_09, SA01_10,
         SA01_11, SA01_12, SA01_13, SA01_14, SA01_15, SA01_16, SA01_17, SA01_18, SA01_19, SA01_20) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(SA01_01, SA01_02, SA01_03, SA01_04, SA01_05, SA01_06, SA01_07, SA01_08, SA01_09, SA01_10,
         SA01_11, SA01_12, SA01_13, SA01_14, SA01_15, SA01_16, SA01_17, SA01_18, SA01_19, SA01_20) %>%  psych::alpha()



##
# Trait Anger
##

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(SX02_01, SX02_02, SX02_03, SX02_04, SX02_05,
         SX02_06, SX02_07, SX02_08, SX02_09, SX02_10) %>%
  unlist() %>%
  unique() 


#######
# Scenario Depression
#######

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(SC06_01, SC06_02, SC06_08, SC06_04,
         SC12_01, SC12_02, SC12_08, SC12_04,
         SC13_01, SC13_02, SC13_08, SC13_04,
         SC14_01, SC14_02, SC14_08, SC14_04,
         SC15_01, SC15_02, SC15_08, SC15_04) %>%
  unlist() %>%
  unique() 

#
# correlation + Cronbach for Scenario 1

data_basic %>%
  select(SC06_01, SC06_02, SC06_08, SC06_04) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(SC06_01, SC06_02, SC06_08, SC06_04) %>%  psych::alpha()




#######
# Scenario Psychopathy
#######

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(SC07_09, SC07_10, SC07_11, SC07_12,
         SC16_09, SC16_10, SC16_11, SC16_12,
         SC17_09, SC17_10, SC17_11, SC17_12,
         SC18_09, SC18_10, SC18_11, SC18_12,
         SC19_09, SC19_10, SC19_11, SC19_12) %>%
  unlist() %>%
  unique() 

#
# correlation + Cronbach for Scenario 1

data_basic %>%
  select(SC07_09, SC07_10, SC07_11, SC07_12) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(SC07_09, SC07_10, SC07_11, SC07_12) %>%  psych::alpha()



#######
# Scenario Trust
#######

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(SC08_13, SC08_14, SC08_15, SC08_16,
         SC20_13, SC20_14, SC20_15, SC20_16,
         SC21_13, SC21_14, SC21_15, SC21_16,
         SC22_13, SC22_14, SC22_15, SC22_16,
         SC23_13, SC23_14, SC23_15, SC23_16) %>%
  unlist() %>%
  unique() 

#
# correlation + Cronbach for Scenario 1

data_basic %>%
  select(SC08_13, SC08_14, SC08_15, SC08_16) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

# Cronbach's alpha

data_basic %>%
  select(SC08_13, SC08_14, SC08_15, SC08_16) %>%  psych::alpha()



#######
# Leadership Szenario Items
#######

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(SL04_01, SL04_02, SL04_09,      # cooperative items
         SL04_08, SL04_04, SL04_10) %>%  # reactive items
  unlist() %>%
  unique() 


#######
# Employee Szenario Items
#######

# to be reverse-coded: ???

# occuring values

data_basic %>%
  select(SL05_02, SL05_03, SL05_04,      # cooperative items
         SL05_01, SL05_05, SL05_06) %>%  # reactive items
  unlist() %>%
  unique() 





























