#####################
#prepare correlations#
#####################
library(dplyr)
library(psych)
attach(data_basic)
##1. exclude people with more than 2 failed attention checks
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

table(CN03)
#exclude people with technical problems
data_basic <- data_basic %>%
  filter(CN03 == "Nein")



#2. calculate mean scores for variables (main variables)----
#psychopathy----
psychopathy <- data_basic %>%
  mutate(psychopathy = rowMeans(across(c(DT01_02, DT01_05, DT01_08)), na.rm = TRUE)) %>%
  pull(psychopathy)
hist(psychopathy)
#depression----
depression <- data_basic %>%
  mutate(depression = rowMeans(across(c( HU_ADS_01, HU_ADS_02, HU_ADS_03, HU_ADS_04, HU_ADS_05,
                                          HU_ADS_06, HU_ADS_07, HU_ADS_08, HU_ADS_09, HU_ADS_10,
                                          HU_ADS_11, HU_ADS_12, HU_ADS_13, HU_ADS_14, HU_ADS_15,
                                          HU_ADS_16, HU_ADS_17, HU_ADS_18, HU_ADS_19, HU_ADS_20)), na.rm = TRUE)) %>%
  pull(depression)
hist(depression)
#trust-----
#willingness to interact
trust_wia <- data_basic %>%
  mutate(trust_wia = rowMeans(across(c( AT01_01, AT01_02, AT01_03)), na.rm = TRUE)) %>%
  pull(trust_wia)
hist(trust_wia)
#feeling secure
trust_secure <- data_basic %>%
  mutate(trust_secure = rowMeans(across(c( AT01_04, AT01_05, AT01_06)), na.rm = TRUE)) %>%
  pull(trust_secure)
hist(trust_secure)
#propensity to trust humans
trust_humans <- data_basic %>%
  mutate(trust_humans = rowMeans(across(c( AT01_07, AT01_08, AT01_09, AT01_10, AT01_11)), na.rm = TRUE)) %>%
  pull(trust_humans)
hist(trust_humans)
#propensity to trust automation
trust_automation <- data_basic %>%
  mutate(trust_automation = rowMeans(across(c( AT01_12, AT01_13, AT01_14, AT01_15)), na.rm = TRUE)) %>%
  pull(trust_automation)
hist(trust_automation)
#literacy
trust_literacy <- data_basic %>%
  mutate(trust_literacy = rowMeans(across(c( AT01_16, AT01_17, AT01_18)), na.rm = TRUE)) %>%
  pull(trust_literacy)
hist(trust_literacy)


#Machiavellianism----
machiavellianism <- data_basic %>%
  mutate(machiavellianism = rowMeans(across(c(DT01_01, DT01_04, DT01_07)), na.rm = TRUE)) %>%
  pull(machiavellianism)
hist(machiavellianism)
#narcissism----
narcissism <- data_basic %>%
  mutate(narcissism = rowMeans(across(c(DT01_03, DT01_06)), na.rm = TRUE)) %>%
  pull(narcissism)
hist(narcissism)






