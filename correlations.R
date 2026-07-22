#####################
#prepare correlations#
#####################
library(dplyr)
library(psych)
library(effectsize)
library(ggplot2)
library(tidyr)
library(car)

# run the script Analysis_Script.R
source("Analysis_Script.R")

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

table(data_basic$attention_fail_overall)
## no one failed more than 2 attention checks


#exclude people with technical problems
table(CN03)
data_basic <- data_basic %>%
  filter(CN03 == "Nein")

#exclude people who took less than 15 min
# 15 min = 900 sec
sum(data_basic$TIME_SUM < 900, na.rm = TRUE) # 21 people who took less than 15 min
data_basic <- data_basic %>%
  filter(TIME_SUM >= 900)



#2. calculate mean scores for variables (main variables)----
#psychopathy----
data_basic <- data_basic %>%
  mutate(psychopathy = rowMeans(across(c(DT01_02, DT01_05, DT01_08)), na.rm = TRUE))
hist(data_basic$psychopathy)
#depression----
data_basic <- data_basic %>%
  mutate(depression = rowMeans(across(c( HU_ADS_01, HU_ADS_02, HU_ADS_03, HU_ADS_04, HU_ADS_05,
                                         HU_ADS_06, HU_ADS_07, HU_ADS_08, HU_ADS_09, HU_ADS_10,
                                         HU_ADS_11, HU_ADS_12, HU_ADS_13, HU_ADS_14, HU_ADS_15,
                                         HU_ADS_16, HU_ADS_17, HU_ADS_18, HU_ADS_19, HU_ADS_20)), na.rm = TRUE))
hist(data_basic$depression)
#trust-----
#willingness to interact
data_basic <- data_basic %>%
  mutate(trust_wia = rowMeans(across(c( AT01_01, AT01_02, AT01_03)), na.rm = TRUE))
hist(data_basic$trust_wia)
#feeling secure
data_basic <- data_basic %>%
  mutate(trust_secure = rowMeans(across(c( AT01_04, AT01_05, AT01_06)), na.rm = TRUE))
hist(data_basic$trust_secure)
#propensity to trust humans
data_basic <- data_basic %>%
  mutate(trust_humans = rowMeans(across(c( AT01_07, AT01_08, AT01_09, AT01_10, AT01_11)), na.rm = TRUE))
hist(data_basic$trust_humans)
#propensity to trust automation
data_basic <- data_basic %>%
  mutate(trust_automation = rowMeans(across(c( AT01_12, AT01_13, AT01_14, AT01_15)), na.rm = TRUE))
hist(data_basic$trust_automation)
#literacy
data_basic <- data_basic %>%
  mutate(trust_literacy = rowMeans(across(c( AT01_16, AT01_17, AT01_18)), na.rm = TRUE))
hist(data_basic$trust_literacy)
#all together
data_basic <- data_basic %>%
  mutate(trust_all = rowMeans(across(c(AT01_01, AT01_02, AT01_03,
                                       AT01_04, AT01_05, AT01_06,
                                       AT01_07, AT01_08, AT01_09, AT01_10, AT01_11,
                                       AT01_12, AT01_13, AT01_14, AT01_15,
                                       AT01_16, AT01_17, AT01_18)), na.rm = TRUE))
#trust automation
data_basic <- data_basic %>%
  mutate(trust_automation_all = rowMeans(across(c(AT01_01, AT01_02, AT01_03,
                                       AT01_04, AT01_05, AT01_06,
                                       AT01_12, AT01_13, AT01_14, AT01_15,
                                       AT01_16, AT01_17, AT01_18)), na.rm = TRUE))
hist(data_basic$trust_automation_all)


#3. other variables
#Machiavellianism----
data_basic <- data_basic %>%
  mutate(machiavellianism = rowMeans(across(c(DT01_01, DT01_04, DT01_07)), na.rm = TRUE))
hist(data_basic$machiavellianism)
#narcissism (one item is missing) ---- 
data_basic <- data_basic %>%
  mutate(narcissism = rowMeans(across(c(DT01_03, DT01_06)), na.rm = TRUE))
hist(data_basic$narcissism)

#big5-----
#extraversion
data_basic <- data_basic %>%
  mutate(extraversion = rowMeans(across(c(BF01_01, BF01_06, BF01_11, BF01_16)), na.rm = TRUE))
hist(data_basic$extraversion)
#agreeableness
data_basic <- data_basic %>%
  mutate(agreeablenesss = rowMeans(across(c(BF01_02, BF01_07, BF01_12, BF01_17)), na.rm = TRUE))
hist(data_basic$agreeablenesss)
#conscientiousness
data_basic <- data_basic %>%
  mutate(conscientiousness = rowMeans(across(c(BF01_03, BF01_08, BF01_13, BF01_18)), na.rm = TRUE))
hist(data_basic$conscientiousness)
#neuroticism
data_basic <- data_basic %>%
  mutate(neuroticism = rowMeans(across(c(BF01_04, BF01_09, BF01_14, BF01_19)), na.rm = TRUE))
hist(data_basic$neuroticism)
#openess
data_basic <- data_basic %>%
  mutate(openess = rowMeans(across(c(BF01_05, BF01_10, BF01_15, BF01_20, BF01_21)), na.rm = TRUE))
hist(data_basic$openess)

#coping----
#task-oriented
data_basic <- data_basic %>%
  mutate(ciss_task = rowMeans(across(c(CS01_05, CS01_09, CS01_12, CS01_14, CS01_15, CS01_19, CS01_23, CS01_24)), na.rm = TRUE))
hist(data_basic$ciss_task)
#emotion-oriented
data_basic <- data_basic %>%
  mutate(ciss_emotion = rowMeans(across(c(CS01_02, CS01_03, CS01_07, CS01_08, CS01_11, CS01_13, CS01_16, CS01_21)), na.rm = TRUE))
hist(data_basic$ciss_emotion)
# avoidance-oriented
data_basic <- data_basic %>%
  mutate(ciss_avoidance = rowMeans(across(c(CS01_01, CS01_17, CS01_18, CS01_20, CS01_04, CS01_06, CS01_10, CS01_22)), na.rm = TRUE))
hist(data_basic$ciss_avoidance)

#self-efficacy----
data_basic <- data_basic %>%
  mutate(self_efficacy = rowMeans(across(c(GS01_07, GS01_09, GS01_18, GS01_24, GS01_25, GS01_27, GS01_30, GS01_31, GS01_35, GS01_38)), na.rm = TRUE))
hist(data_basic$self_efficacy)

#prosocial tendencies----
data_basic <- data_basic %>%
  mutate(proso_tend = rowMeans(across(c(PT01_04, PT01_10, PT01_16, PT01_20, PT01_23, PT01_24)), na.rm = TRUE))
hist(data_basic$proso_tend)

#anxiety----
data_basic <- data_basic %>%
  mutate(anxiety = rowMeans(across(c(SA01_01, SA01_02, SA01_03, SA01_04, SA01_05, SA01_06, SA01_07, SA01_08, SA01_09, SA01_10,
                                     SA01_11, SA01_12, SA01_13, SA01_14, SA01_15, SA01_16, SA01_17, SA01_18, SA01_19, SA01_20)), na.rm = TRUE))
hist(data_basic$anxiety)

#anger----
data_basic <- data_basic %>%
  mutate(anger = rowMeans(across(c(SX02_01, SX02_02, SX02_03, SX02_04, SX02_05,
                                   SX02_06, SX02_07, SX02_08, SX02_09, SX02_10)), na.rm = TRUE))
hist(data_basic$anger)

#4. correlation with study variables (scenarios)----
#depression with all depression items in the scenarios
data_basic %>%
  select(depression, SC06_01, SC06_02, SC06_08, SC06_04,
         SC12_01, SC12_02, SC12_08, SC12_04,
         SC13_01, SC13_02, SC13_08, SC13_04,
         SC14_01, SC14_02, SC14_08, SC14_04,
         SC15_01, SC15_02, SC15_08, SC15_04) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

#depression with the average depression items in the scenarios
data_basic <- data_basic %>% #creating average depression score across scenarios
  mutate(scen_depr_mean = rowMeans(across(c(
        SC06_01, SC06_02, SC06_08, SC06_04,
        SC12_01, SC12_02, SC12_08, SC12_04,
        SC13_01, SC13_02, SC13_08, SC13_04,
        SC14_01, SC14_02, SC14_08, SC14_04,
        SC15_01, SC15_02, SC15_08, SC15_04
      )),
      na.rm = TRUE
    )
  )
hist(data_basic$scen_depr_mean)

#correlation analysis
cor.test(
  data_basic$depression,
  data_basic$scen_depr_mean,
  method = "pearson",
  use = "pairwise.complete.obs"
)

#psychopathy with all psychopathy items in the scenarios
#"I pretend being too busy for the task" is stronger correlated with depression
#data_basic %>%
#select(depression, SC07_09, SC07_10, SC07_11, SC07_12,
#       SC16_09, SC16_10, SC16_11, SC16_12,
#       SC17_09, SC17_10, SC17_11, SC17_12,
#       SC18_09, SC18_10, SC18_11, SC18_12,
#       SC19_09, SC19_10, SC19_11, SC19_12) %>%
#  cor(use = "pairwise.complete.obs") %>% round(2)

data_basic %>%
  select(psychopathy, SC07_09, SC07_10, SC07_11, SC07_12,
         SC16_09, SC16_10, SC16_11, SC16_12,
         SC17_09, SC17_10, SC17_11, SC17_12,
         SC18_09, SC18_10, SC18_11, SC18_12,
         SC19_09, SC19_10, SC19_11, SC19_12) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

#psychopathy with the average psychopathic/egoistic items in the scenarios
data_basic <- data_basic %>% #creating average psychopathy score across scenarios
  mutate(scen_psycho_mean = rowMeans(across(c(
    SC07_09, SC07_10, SC07_11, SC07_12,
    SC16_09, SC16_10, SC16_11, SC16_12,
    SC17_09, SC17_10, SC17_11, SC17_12,
    SC18_09, SC18_10, SC18_11, SC18_12,
    SC19_09, SC19_10, SC19_11, SC19_12
  )),
  na.rm = TRUE
  )
  )
hist(data_basic$scen_psycho_mean)

#correlation analysis
cor.test(
  data_basic$psychopathy,
  data_basic$scen_psycho_mean,
  method = "pearson",
  use = "pairwise.complete.obs"
)


#trust with all trust items in the scenarios
data_basic %>%
  select(trust_wia, trust_secure, trust_humans, trust_automation, trust_literacy,
         SC08_13, SC08_14, SC08_15, SC08_16,
         SC20_13, SC20_14, SC20_15, SC20_16,
         SC21_13, SC21_14, SC21_15, SC21_16,
         SC22_13, SC22_14, SC22_15, SC22_16,
         SC23_13, SC23_14, SC23_15, SC23_16) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

#trust with the average trusting behavior items in the scenarios
data_basic <- data_basic %>% #creating average trusting behavior score across scenarios
  mutate(scen_trust_mean = rowMeans(across(c(
    SC08_13, SC08_14, SC08_15, SC08_16,
    SC20_13, SC20_14, SC20_15, SC20_16,
    SC21_13, SC21_14, SC21_15, SC21_16,
    SC22_13, SC22_14, SC22_15, SC22_16,
    SC23_13, SC23_14, SC23_15, SC23_16
  )),
  na.rm = TRUE
  )
  )
hist(data_basic$scen_trust_mean)

#correlation analysis
cor.test(
  data_basic$trust_all,
  data_basic$scen_trust_mean,
  method = "pearson",
  use = "pairwise.complete.obs"
)


#5. correlations with leadership scenarios----
#leadership perspective
#cooperative items
data_basic %>%
  select(depression, psychopathy, proso_tend, agreeablenesss, self_efficacy, conscientiousness, trust_all, trust_wia, trust_secure, trust_humans, trust_automation, trust_literacy,
         SL04_01, SL04_02, SL04_09) %>%
  cor(use = "pairwise.complete.obs") %>% round(2) #Item SL04_09 seems to not have worked as predicted

#coersive items
data_basic %>%
  select(depression, psychopathy, proso_tend, agreeablenesss, self_efficacy, conscientiousness, trust_all, trust_wia, trust_secure, trust_humans, trust_automation, trust_literacy,
         SL04_08, SL04_04, SL04_10) %>% 
  cor(use = "pairwise.complete.obs") %>% round(2)

#employee's perspective 
#cooperative items
data_basic %>%
  select(depression, psychopathy, proso_tend, agreeablenesss, self_efficacy, conscientiousness, trust_all, trust_wia, trust_secure, trust_humans, trust_automation, trust_literacy,
         SL05_02, SL05_03, SL05_04) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)
#reactant items
data_basic %>%
  select(depression, psychopathy, proso_tend, agreeablenesss, self_efficacy, conscientiousness, trust_all, trust_wia, trust_secure, trust_humans, trust_automation, trust_literacy,
         SL05_01, SL05_05, SL05_06) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)



# save(data_basic, file = "EFRE_data.Rdata")



#6. order effects----
#create groups for order effect
data_basic <- data_basic %>%
  mutate(
    order_leadership = factor(
      case_when(
      ZU02_01 == 1 ~ "leader first",
      ZU02_02 == 1 ~ "employee first",
      TRUE         ~ NA_character_  # Handles cases where neither is 1 (optional)
    )
  ))

# Compare means for cooperation: leadership scenario
means_order_effect_leadership_cooperation <- data_basic %>%
  group_by(order_leadership) %>%
  summarise(
    across(
      .cols = c(SL04_01, SL04_02, SL04_09), 
      .fns = ~ mean(.x, na.rm = TRUE),            
      .names = "mean_{.col}"                      
    ),
    .groups = 'drop'
  )
t.test(SL04_01 ~ order_leadership, data = data_basic)
cohens_d(SL04_01 ~ order_leadership, data = data_basic)

t.test(SL04_02 ~ order_leadership, data = data_basic)
cohens_d(SL04_02 ~ order_leadership, data = data_basic)

t.test(SL04_09 ~ order_leadership, data = data_basic)
cohens_d(SL04_09 ~ order_leadership, data = data_basic)

# Compare means for cooperation: employee scenario
means_order_effect_employee_cooperation <- data_basic %>%
  group_by(order_leadership) %>%
  summarise(
    across(
      .cols = c(SL05_02, SL05_03, SL05_04), 
      .fns = ~ mean(.x, na.rm = TRUE),            
      .names = "mean_{.col}"                      
    ),
    .groups = 'drop'
  )

t.test(SL05_02 ~ order_leadership, data = data_basic)
cohens_d(SL05_02 ~ order_leadership, data = data_basic)

t.test(SL05_03 ~ order_leadership, data = data_basic)
cohens_d(SL05_03 ~ order_leadership, data = data_basic)

t.test(SL05_04 ~ order_leadership, data = data_basic)
cohens_d(SL05_04 ~ order_leadership, data = data_basic)

# Compare means for coersive behavior: leadership scenario
means_order_effect_leadership_coersive <- data_basic %>%
  group_by(order_leadership) %>%
  summarise(
    across(
      .cols = c(SL04_08, SL04_04, SL04_10), 
      .fns = ~ mean(.x, na.rm = TRUE),            
      .names = "mean_{.col}"                      
    ),
    .groups = 'drop'
  )

t.test(SL04_08 ~ order_leadership, data = data_basic)
cohens_d(SL04_08 ~ order_leadership, data = data_basic)

t.test(SL04_04 ~ order_leadership, data = data_basic)
cohens_d(SL04_04 ~ order_leadership, data = data_basic)

t.test(SL04_10 ~ order_leadership, data = data_basic)
cohens_d(SL04_10 ~ order_leadership, data = data_basic)

# Compare means for reactant behavior: employee scenario
means_order_effect_employee_reactant <- data_basic %>%
  group_by(order_leadership) %>%
  summarise(
    across(
      .cols = c(SL05_01, SL05_05, SL05_06), 
      .fns = ~ mean(.x, na.rm = TRUE),            
      .names = "mean_{.col}"                      
    ),
    .groups = 'drop'
  )

t.test(SL05_01 ~ order_leadership, data = data_basic)
cohens_d(SL05_01 ~ order_leadership, data = data_basic)

t.test(SL05_05 ~ order_leadership, data = data_basic)
cohens_d(SL05_05 ~ order_leadership, data = data_basic)

t.test(SL05_06 ~ order_leadership, data = data_basic)
cohens_d(SL05_06 ~ order_leadership, data = data_basic)

#7. interaction order effects with trait variables ----

## leader perspective; coop behav (SL04_01, SL04_02, SL04_09)
# trust
data_basic <- data_basic %>% # centering trust_all
  mutate(
    trust_all_cent = scale(trust_all, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL04_01 ~ order_leadership * trust_all_cent, data = data_basic)
Anova(model, type = 3)

# prosocial tendencies
data_basic <- data_basic %>% # centering proso_tend
  mutate(
    proso_tend_cent = scale(proso_tend, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL04_01 ~ order_leadership * proso_tend_cent, data = data_basic)
Anova(model, type = 3)

# agreeableness
data_basic <- data_basic %>% # centering agreeablenesss
  mutate(
    agreeablenesss_cent = scale(agreeablenesss, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL04_01 ~ order_leadership * agreeablenesss_cent, data = data_basic)
Anova(model, type = 3)


## leader perspective; coersive behav (SL04_08, SL04_04, SL04_10)
# psychopathy
data_basic <- data_basic %>% # centering psychopathy
  mutate(
    psychopathy_cent = scale(psychopathy, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL04_08 ~ order_leadership * psychopathy_cent, data = data_basic)
Anova(model, type = 3)


## employees perspective; coop behav (SL05_02, SL05_03, SL05_04)
# trust
data_basic <- data_basic %>% # centering trust_all
  mutate(
    trust_all_cent = scale(trust_all, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL05_02 ~ order_leadership * trust_all_cent, data = data_basic)
Anova(model, type = 3)

# prosocial tendencies
data_basic <- data_basic %>% # centering proso_tend
  mutate(
    proso_tend_cent = scale(proso_tend, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL05_02 ~ order_leadership * proso_tend_cent, data = data_basic)
Anova(model, type = 3)

# agreeableness
data_basic <- data_basic %>% # centering agreeablenesss
  mutate(
    agreeablenesss_cent = scale(agreeablenesss, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL05_02 ~ order_leadership * agreeablenesss_cent, data = data_basic)
Anova(model, type = 3)



## employees perspective; reactant behav (SL05_01, SL05_05, SL05_06)
# psychopathy
data_basic <- data_basic %>% # centering psychopathy
  mutate(
    psychopathy_cent = scale(psychopathy, center = TRUE, scale = FALSE)
  )
# Anova
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(SL05_01 ~ order_leadership * psychopathy_cent, data = data_basic)
Anova(model, type = 3)



#8. correlations with study variables (scenarios) beyond primary interest ----

# machiavellianism, narcissism, BIG5, Coping (3 subscales), self-efficacy, prosocial tendencies, STAI, STAXI
# with average depression items in the scenarios (missing are trust subscales)
data_basic %>%
  select(machiavellianism, narcissism, extraversion, agreeablenesss, conscientiousness, neuroticism, openess,
         ciss_task, ciss_emotion, ciss_avoidance, self_efficacy, proso_tend, anxiety, anger, scen_depr_mean) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

#correlation analysis
cor.test(
  data_basic$anxiety,
  data_basic$scen_depr_mean,
  method = "pearson",
  use = "pairwise.complete.obs"
)

# machiavellianism, narcissism, BIG5, Coping (3 subscales), self-efficacy, prosocial tendencies, STAI, STAXI
# with average psychopathic/egoistic items in the scenarios (missing are trust subscales)
data_basic %>%
  select(machiavellianism, narcissism, extraversion, agreeablenesss, conscientiousness, neuroticism, openess,
         ciss_task, ciss_emotion, ciss_avoidance, self_efficacy, proso_tend, anxiety, anger, scen_psycho_mean) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

#correlation analysis
cor.test(
  data_basic$conscientiousness,
  data_basic$scen_psycho_mean,
  method = "pearson",
  use = "pairwise.complete.obs"
)


# machiavellianism, narcissism, BIG5, Coping (3 subscales), self-efficacy, prosocial tendencies, STAI, STAXI
# with average trusting behavior items in the scenarios (missing are trust subscales)
data_basic %>%
  select(machiavellianism, narcissism, extraversion, agreeablenesss, conscientiousness, neuroticism, openess,
         ciss_task, ciss_emotion, ciss_avoidance, self_efficacy, proso_tend, anxiety, anger, scen_trust_mean) %>%
  cor(use = "pairwise.complete.obs") %>% round(2)

#correlation analysis
cor.test(
  data_basic$anxiety,
  data_basic$scen_trust_mean,
  method = "pearson",
  use = "pairwise.complete.obs"
)


#9. Journalistenfragen----

# check ranking of AI

# JF02_01 = Buchdruck; JF02_02 = KI; JF02_03 = Smartphone; JF02_04 = Internet; JF02_05 = Automobil; JF02_06 = Computer; JF02_07 = Eisenbahn

data_basic %>%
  select(JF02_01, JF02_02, JF02_03, JF02_04, JF02_05, JF02_06, JF02_07) %>%
  unlist() %>%
  unique() 

# convert variables to numeric
data_basic <- data_basic %>%
  mutate(across(JF02_01:JF02_07, as.numeric))

# Convert to long format
long_data <- data_basic %>%
  select(DM01_01, JF02_01, JF02_02, JF02_03, JF02_04, JF02_05, JF02_06, JF02_07) %>%
    rename(
    Buchdruck = JF02_01,
    KI         = JF02_02,
    Smartphone = JF02_03,
    Internet   = JF02_04,
    Automobil  = JF02_05,
    Computer   = JF02_06,
    Eisenbahn  = JF02_07
  ) %>% 
  pivot_longer(
    cols = Buchdruck:Eisenbahn,
    names_to = "Variable",
    values_to = "Rank"
  ) 

# # if you only want certain age
# long_data_1st_quant <- long_data %>%
# filter(DM01_01 <= 26)
# 
# long_data_2nd_quant <- long_data %>%
# filter(DM01_01 > 26 & DM01_01 <= 31)
# 
# long_data_3rd_quant <- long_data %>%
#   filter(DM01_01 > 31 & DM01_01 <= 39)
# 
# long_data_4th_quant <- long_data %>%
#   filter(DM01_01 > 39)


ggplot(long_data,
       aes(x = factor(Rank),
           fill = Variable)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "#0072B2",  # blue
    "#D55E00",  # orange-red
    "#009E73",  # green
    "#CC79A7",  # purple
    "#E69F00",  # orange
    "#56B4E9",  # light blue
    "#F0E442"   # yellow
  )) +
  labs(x = "Rang",
       y = "Prozent der Studienteilnehmer:innen",
       fill = "Variable") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# alternative graph


long_data <- data_basic %>%
  pivot_longer(
    cols = JF02_01:JF02_07,
    names_to = "Variable",
    values_to = "Rank"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      JF02_01 = "Buchdruck",
      JF02_02 = "KI",
      JF02_03 = "Smartphone",
      JF02_04 = "Internet",
      JF02_05 = "Automobil",
      JF02_06 = "Computer",
      JF02_07 = "Eisenbahn"
    ),
    Rank = factor(Rank)
  ) %>%
  count(Variable, Rank)



long_data <- long_data %>%
  group_by(Variable) %>%
  mutate(percent = n / sum(n))

ggplot(long_data,
       aes(x = Variable,
           y = percent,
           fill = Rank)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c(
    "1" = "grey10",
    "2" = "grey25",
    "3" = "grey40",
    "4" = "grey55",
    "5" = "grey65",
    "6" = "grey75",
    "7" = "grey85"
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "Prozent der Studienteilnehmer:innen",
       fill = "Rang") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14))
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# check according to age

  # Compute quartiles
  quartiles <- quantile(data_basic$DM01_01,
                        probs = c(0.25, 0.50, 0.75),
                        na.rm = TRUE)
  
  # get max y-value from histogram
  max_y <- max(ggplot_build(ggplot(data_basic, aes(x = DM01_01)) +
                              geom_bar())$data[[1]]$count)
  
  ggplot(data_basic,
         aes(x = DM01_01)) +
    geom_bar() +
    geom_vline(xintercept = quartiles,
               linetype = "dashed",
               color = "red",
               linewidth = 1) +
    annotate("text",
             x = quartiles + 1,
             y = max_y * 0.90,   
             label = c("1 Quartil", "Median", "3. Quartil"),
             angle = 90,
             color = "red",
             size = 5) +
    labs(x = "Alter der Studienteilnehmenden",
         y = "Häufigkeit") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )

# Quantile

quantile(data_basic$DM01_01,
         probs = c(0.25, 0.5, 0.75),
         na.rm = TRUE)

mean(data_basic$DM01_01, na.rm = TRUE)
sd(data_basic$DM01_01, na.rm = TRUE)

# gender

unique(data_basic$DM03)
table(data_basic$DM03)



# How pos/neg is AI seen ----

# JF03_01 = außerhalb Arbeitsplatz
# JF04_01 = am Arbeitsplatz

# convert variables to numeric
data_basic <- data_basic %>%
  mutate(across(JF03_01: JF04_01, as.numeric))

data_basic %>%
  select(JF03_01, JF04_01) %>%
  unlist() %>%
  unique() 


data_basic %>%
  select(JF03_01, JF04_01) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("sehr negativ", "2", "3", "4", "sehr positiv")
  ) +
  labs(x = "",
       y = "Häufigkeit") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

# Compare means for cooperation: leadership scenario

data_basic %>%
  summarise(
    M_JF03 = mean(JF03_01, na.rm = TRUE),
    SD_JF03 = sd(JF03_01, na.rm = TRUE),
    M_JF04 = mean(JF04_01, na.rm = TRUE),
    SD_JF04 = sd(JF04_01, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(everything())

t.test(data_basic$JF03_01,
       data_basic$JF04_01,
       paired = TRUE)

cohens_d(data_basic$JF03_01,
         data_basic$JF04_01,
         paired = TRUE)



#9. TO DOs:----
#correlations with study variables beyond primary interest, order effect of leadership scenarios


