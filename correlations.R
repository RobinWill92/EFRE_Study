#####################
#prepare correlations#
#####################
library(dplyr)
library(psych)
attach(data_basic)
##1. exclude people with more than 2 failed attention checks----
#attention checks: BF01_22, CN09, CN15
data_basic <- data_basic %>%
  mutate(
    attention_fail_1 = if_else(BF01_22 == "5", 0, 1),
    attention_fail_2 = if_else(CN09 == "Automatisierung hat u.a. zum Ziel, Menschen sich wiederholende Aufgaben abzunehmen.", 0, 1),
    attention_fail_3 = if_else(CN15 == "Automatisierung beschreibt verschiedene Technologien, die auf Produktherstellung und Aufgabenausführung ausgerichtet sind.", 0, 1),
    attention_fail_overall = attention_fail_1 + attention_fail_2 + attention_fail_3
  )

table(attention_fail_overall)
## no one failed more than 2 attention checks


#exclude people with technical problems
table(CN03)
data_basic <- data_basic %>%
  filter(CN03 == "Nein")



#2. calculate mean scores for variables (main variables)----
#psychopathy----
data_basic <- data_basic %>%
  mutate(psychopathy = rowMeans(across(c(DT01_02, DT01_05, DT01_08)), na.rm = TRUE))
hist(psychopathy)
#depression----
data_basic <- data_basic %>%
  mutate(depression = rowMeans(across(c( HU_ADS_01, HU_ADS_02, HU_ADS_03, HU_ADS_04, HU_ADS_05,
                                          HU_ADS_06, HU_ADS_07, HU_ADS_08, HU_ADS_09, HU_ADS_10,
                                          HU_ADS_11, HU_ADS_12, HU_ADS_13, HU_ADS_14, HU_ADS_15,
                                          HU_ADS_16, HU_ADS_17, HU_ADS_18, HU_ADS_19, HU_ADS_20)), na.rm = TRUE))
hist(depression)
#trust-----
#willingness to interact
data_basic <- data_basic %>%
  mutate(trust_wia = rowMeans(across(c( AT01_01, AT01_02, AT01_03)), na.rm = TRUE))
hist(trust_wia)
#feeling secure
data_basic <- data_basic %>%
  mutate(trust_secure = rowMeans(across(c( AT01_04, AT01_05, AT01_06)), na.rm = TRUE))
hist(trust_secure)
#propensity to trust humans
data_basic <- data_basic %>%
  mutate(trust_humans = rowMeans(across(c( AT01_07, AT01_08, AT01_09, AT01_10, AT01_11)), na.rm = TRUE))
hist(trust_humans)
#propensity to trust automation
data_basic <- data_basic %>%
  mutate(trust_automation = rowMeans(across(c( AT01_12, AT01_13, AT01_14, AT01_15)), na.rm = TRUE))
hist(trust_automation)
#literacy
data_basic <- data_basic %>%
  mutate(trust_literacy = rowMeans(across(c( AT01_16, AT01_17, AT01_18)), na.rm = TRUE))
hist(trust_literacy)

#3. other variables
#Machiavellianism----
data_basic <- data_basic %>%
  mutate(machiavellianism = rowMeans(across(c(DT01_01, DT01_04, DT01_07)), na.rm = TRUE))
hist(machiavellianism)
#narcissism----
data_basic <- data_basic %>%
  mutate(narcissism = rowMeans(across(c(DT01_03, DT01_06)), na.rm = TRUE))
hist(narcissism)

#big5-----
#extraversion
data_basic <- data_basic %>%
  mutate(extraversion = rowMeans(across(c(BF01_01, BF01_06, BF01_11, BF01_16)), na.rm = TRUE))
hist(extraversion)
#agreeableness
data_basic <- data_basic %>%
  mutate(agreeablenesss = rowMeans(across(c(BF01_02, BF01_07, BF01_12, BF01_17)), na.rm = TRUE))
hist(agreeablenesss)
#conscientiousness
data_basic <- data_basic %>%
  mutate(conscientiousness = rowMeans(across(c(BF01_03, BF01_08, BF01_13, BF01_18)), na.rm = TRUE))
hist(conscientiousness)
#neuroticism
data_basic <- data_basic %>%
  mutate(neuroticism = rowMeans(across(c(BF01_04, BF01_09, BF01_14, BF01_19)), na.rm = TRUE))
hist(neuroticism)
#openess
data_basic <- data_basic %>%
  mutate(openess = rowMeans(across(c(BF01_05, BF01_10, BF01_15, BF01_20, BF01_21)), na.rm = TRUE))
hist(openess)

#coping----
#task-oriented
data_basic <- data_basic %>%
  mutate(ciss_task = rowMeans(across(c(CS01_05, CS01_09, CS01_12, CS01_14, CS01_15, CS01_19, CS01_23, CS01_24)), na.rm = TRUE))
hist(ciss_task)
#emotion-oriented
data_basic <- data_basic %>%
  mutate(ciss_emotion = rowMeans(across(c(CS01_02, CS01_03, CS01_07, CS01_08, CS01_11, CS01_13, CS01_16, CS01_21)), na.rm = TRUE))
hist(ciss_emotion)
# avoidance-oriented
data_basic <- data_basic %>%
  mutate(ciss_avoidance = rowMeans(across(c(CS01_01, CS01_17, CS01_18, CS01_20, CS01_04, CS01_06, CS01_10, CS01_22)), na.rm = TRUE))
hist(ciss_avoidance)


#4. correlation with study variables (scenarios)----
#depression
data_basic %>%
  select(depression, SC06_01, SC06_02, SC06_08, SC06_04,
         SC12_01, SC12_02, SC12_08, SC12_04,
         SC13_01, SC13_02, SC13_08, SC13_04,
         SC14_01, SC14_02, SC14_08, SC14_04,
         SC15_01, SC15_02, SC15_08, SC15_04) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)
#psychopathy
data_basic %>%
  select(psychopathy, SC07_09, SC07_10, SC07_11, SC07_12,
         SC16_09, SC16_10, SC16_11, SC16_12,
         SC17_09, SC17_10, SC17_11, SC17_12,
         SC18_09, SC18_10, SC18_11, SC18_12,
         SC19_09, SC19_10, SC19_11, SC19_12) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)
#trust
data_basic %>%
  select(trust_wia, trust_secure, trust_humans, trust_automation, trust_literacy,
         SC08_13, SC08_14, SC08_15, SC08_16,
         SC20_13, SC20_14, SC20_15, SC20_16,
         SC21_13, SC21_14, SC21_15, SC21_16,
         SC22_13, SC22_14, SC22_15, SC22_16,
         SC23_13, SC23_14, SC23_15, SC23_16) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)


#5. correlations with leadership scenarios
#depression
data_basic %>%
  select(depression, SL04_01, SL04_02, SL04_09,
         SL04_08, SL04_04, SL04_10) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)
#psychopathy
data_basic %>%
  select(psychopathy, SL04_01, SL04_02, SL04_09,
         SL04_08, SL04_04, SL04_10) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)
#trust
data_basic %>%
  select(trust_wia, trust_secure, trust_humans, trust_automation, trust_literacy,
         SL04_01, SL04_02, SL04_09,
         SL04_08, SL04_04, SL04_10) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)



