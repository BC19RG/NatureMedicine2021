## TABLE 3 - Fatigue
freq(fpos$fatigue) 
freq(fpos$severefatigue) 

# Gender
ctable(fpos$female, fpos$fatigue, OR = T, chisq = T)

femalelogit <- glm(fatigue ~ female, data = fpos, family = "binomial")
summary(femalelogit)
exp(cbind(OR = coef(femalelogit), confint(femalelogit)))
rm(femalelogit)

#Age
fpos$age15 <- droplevels(fpos$age15)

fpos%>%
  group_by(fatigue) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 
ageintlogit <- glm(fatigue ~ ageint, data = fpos, family = "binomial")
summary(ageintlogit)
exp(cbind(OR = coef(ageintlogit), confint(ageintlogit)))
rm(ageintlogit)


ctable(fpos$age15, fpos$fatigue)
age15logit <- glm(fatigue ~ age15, data = fpos, family = "binomial")
summary(age15logit)
exp(cbind(OR = coef(age15logit), confint(age15logit)))
rm(age15logit)

## T1 Body weight - BMI categories
fpos%>%
  filter(!is.na(bmi)) %>%
  group_by(fatigue) %>%
  summarise(n(), median(bmi), quantile(bmi, 1/4), quantile(bmi, 3/4), .groups = 'drop') 

bmilogit <- glm(fatigue ~ bmiint, data = fpos, family = "binomial")
summary(bmilogit)
exp(cbind(OR = coef(bmilogit), confint(bmilogit)))
rm(bmilogit)

ctable(fpos$bmicat, fpos$fatigue)
# fpos = fpos %>% mutate(bmicat = relevel(bmicat, "Normal"))
bmicatlogit <- glm(fatigue ~ bmicat, data = fpos, family = "binomial")
summary(bmicatlogit)
exp(cbind(OR = coef(bmicatlogit), confint(bmicatlogit)))
rm(bmicatlogit)


ctable(as.factor(fpos$fatigue), as.factor(fpos$obese25), OR = T, chisq = T)
ctable(as.factor(fpos$fatigue), as.factor(fpos$obese30), OR = T, chisq = T)
ctable(as.factor(fpos$fatigue), as.factor(fpos$obese35), OR = T, chisq = T)
ctable(as.factor(fpos$obese30), fpos$fatigue, OR = T, chisq = T)

bmi30logit <- glm(fatigue ~ obese30, data = fpos, family = "binomial")
summary(bmi30logit)
exp(cbind(OR = coef(bmi30logit), confint(bmi30logit)))
rm(bmi30logit)


lmbmichalder <- lm(fpos$chalderscore ~ fpos$bmi)
summary(lmbmichalder)
exp(cbind(OR = coef(lmbmichalder), confint(lmbmichalder)))
rm(lmbmichalder)

mean(fpos$vektoppgang, na.rm = T)
ggplot(fpos, na.rm = T, aes(x=vektoppgang), na.rm = T)+ geom_histogram(bins = 100)

# Comorbidities
# Any comorbidity
ctable(fpos$comorbid, fpos$fatigue, OR = T, chisq = T)
ctable(fpos$astma, fpos$fatigue, OR = T, chisq = T)
ctable(fpos$lungesyk, fpos$fatigue)
fisher.test(fpos$lungesyk, fpos$fatigue)
ctable(fpos$anylungdis, fpos$fatigue, OR = T, chisq = T)
ctable(fpos$hjerte, fpos$fatigue, OR = T, chisq = T)
ctable(fpos$hypertensjon, fpos$fatigue, OR = T, chisq = T)
ctable(fpos$diabetes, fpos$fatigue)
fisher.test(fpos$diabetes, fpos$fatigue)
ctable(fpos$nyre, fpos$fatigue)
fisher.test(fpos$nyre, fpos$fatigue)
ctable(fpos$immunsvak, fpos$fatigue)
fisher.test(fpos$immunsvak, fpos$fatigue)
ctable(fpos$smoke_current, fpos$fatigue)
fisher.test(fpos$smoke_current, fpos$fatigue)
ctable(fpos$smoke_ever, fpos$fatigue, useNA = "no", OR = T, chisq = T)

# SYMPTOMS during acute illness
ctable(as.factor(fpos$Fever), fpos$fatigue, OR = T, round.digits = 0, useNA = "no")
ctable(as.factor(fpos$Dyspnoea), fpos$fatigue, OR = T, round.digits = 0, useNA = "no")
ctable(as.factor(fpos$Cough), fpos$fatigue, OR = T, round.digits = 0, useNA = "no")
ctable(as.factor(fpos$Fatigue), fpos$fatigue, OR = T, round.digits = 0, useNA = "no")
ctable(as.factor(fpos$Myalgia), fpos$fatigue, OR = T, round.digits = 0, useNA = "no")
ctable(as.factor(fpos$Headache), fpos$fatigue, OR = T, round.digits = 0, useNA = "no")


ctable(fpos$asymptomatic0m, fpos$fatigue, useNA = "no", OR = T, chisq = T)
ctable(fpos$asymptomatic0m, fpos$fatigue, OR = T, chisq = T)
fisher.test(fpos$asymptomatic0m, fpos$fatigue)
ctable(as.factor(fpos$hosp), fpos$fatigue, useNA = "no", OR = T, chisq = T)
ctable(fpos$cohort, fpos$fatigue)
# fpos = fpos %>% mutate(cohort = relevel(cohort, "Outpatient"))
cohortlogit <- glm(fatigue ~ cohort, data = fpos, family = "binomial")
summary(cohortlogit)
exp(cbind(OR = coef(cohortlogit), confint(cohortlogit)))
rm(cohortlogit)

# Severity as 8-cat ordinal variable
ctable(as.factor(fpos$severity), as.factor(fpos$fatigue), OR = T, chisq = F, round.digits = 0)

fpos%>%
  group_by(fatigue) %>%
  summarise(n(), median(severity), quantile(severity, 1/4), quantile(severity, 3/4), .groups = 'drop') 


fpos%>%
  group_by(fatigue) %>%
  summarise(n(), mean_cl_normal(severity), .groups = 'drop') 

fpos$severityfactor <- as.factor(fpos$severity)
fpos <- fpos %>% 
  mutate(severityfactor = relevel(severityfactor, "2"))

severitylogit <- glm(fatigue ~ severityfactor, data = fpos, family = "binomial")
summary(severitylogit)
exp(cbind(OR = coef(severitylogit), confint(severitylogit)))
rm(severitylogit)

# Hospitalised or not
ctable(as.factor(fpos$hosp), as.factor(fpos$fatigue), OR = T, chisq = T, round.digits = 0)

# Days in hospital
fpos%>%
  filter(!is.na(liggetid)) %>%
  group_by(fatigue) %>%
  summarise(n(), mean_cl_normal(liggetid), .groups = 'drop') 
  #summarise(n(), median(liggetid), quantile(liggetid, 1/4), quantile(liggetid, 3/4), .groups = 'drop')

liggelogit <- glm(fatigue ~ liggetid, data = fpos, family = "binomial")
summary(liggelogit)
exp(cbind(OR = coef(liggelogit), confint(liggelogit)))
rm(liggelogit)
# Lab ----------------
# SPike G and MN antibodies titres using log10 transformed titres

fpos%>%
  group_by(fatigue) %>%
  summarise(n(), mean_cl_normal(spikeglog10), .groups = 'drop') 

fpos%>%
  group_by(fatigue) %>%
  summarise(n(), mean_cl_normal(mnlog10), .groups = 'drop')

wilcox.test(spikeglog10 ~ fatigue, data = fpos)
wilcox.test(mnlog10 ~ fatigue, data = fpos)


spikelogit <- glm(fatigue ~ spikeglog10, data = fpos, family = "binomial")
summary(spikelogit)
exp(cbind(OR = coef(spikeglogit), confint(spikelogit)))
rm(spikelogit)

mnlogit <- glm(fatigue ~ mnlog10, data = fpos, family = "binomial")
summary(mnlogit)
exp(cbind(OR = coef(mnlogit), confint(mnlogit)))
rm(mnlogit)


# 
antibody_lm <- lm(spikeglog10 ~ severity, data = fpos)
summary(antibody_lm)

fpos %>%
  ggplot(aes(x = severity, y = spikeglog)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Complications - during acute illness
ctable(as.factor(fpos$outcome_nyresvikt___1), fpos$fatigue, useNA = "no", OR = T, chisq = T)
fisher.test(as.factor(fpos$outcome_nyresvikt___1), fpos$fatigue)

ctable(as.factor(fpos$outcome_sirksvikt___1), fpos$fatigue, useNA = "no", OR = T, chisq = T)
fisher.test(as.factor(fpos$outcome_sirksvikt___1), fpos$fatigue)

ctable(as.factor(fpos$outcome_respsvikt___1), fpos$fatigue, useNA = "no", OR = T, chisq = T)

ctable(as.factor(fpos$outcome_nyresvikt___1), fpos$fatigue, useNA = "no", OR = T, chisq = T)

ctable(as.factor(fpos$outcome_antibiotika___1), fpos$fatigue, useNA = "no", OR = T, chisq = T)

# bloodtests 
# CRP
crplm <- lm(fpos$chalderscore ~ fpos$outcome_crp)
summary(crplm)
exp(cbind(OR = coef(crplm), confint(crplm)))
rm(crplm)

mod_crp <- glm(fatigue ~ outcome_crp, data = fpos, family = "binomial")
summary(mod_crp)
exp(cbind(OR = coef(mod_crp), confint(mod_crp)))
rm(mod_crp)

# Leucocytes
lm_lpk <- lm(fpos$chalderscore ~ fpos$outcome_lpk)
summary(lm_lpk)
exp(cbind(OR = coef(lm_lpk), confint(lm_lpk)))
rm(lm_lpk)

mod_lpk <- glm(fatigue ~ outcome_lpk, data = fpos, family = "binomial")
summary(mod_lpk)
exp(cbind(OR = coef(mod_lpk), confint(mod_lpk)))
rm(mod_lpk)

# PCT
lm_pct <- lm(fpos$chalderscore ~ fpos$outcome_pct)
summary(lm_pct)
exp(cbind(OR = coef(lm_pct), confint(lm_pct)))
rm(lm_pct)

mod_pct <- glm(fatigue ~ outcome_pct, data = fpos, family = "binomial")
summary(mod_pct)
exp(cbind(OR = coef(mod_pct), confint(mod_pct)))
rm(mod_pct)

# ------- Multivariable analysis ------------
model_all <- glm(fatigue ~ female + ageint + bmiint 
                 + anylungdis + hjerte 
                 #+ comorbid+ hypertensjon + diabetes + nyre + immunsvak + smoker
                + outcome_antibiotika___1
                + Fever + Dyspnoea
                + liggetid + severity  #+ hosp
                + spikeglog10 #+ mnlog10 
               , data = fpos, family = "binomial")
summary(model_all)
exp(cbind(OR = coef(model_all), confint(model_all)))
rm(model_all)

model_cat <- glm(fatigue ~ female + age15 + bmicat + 
                   comorbid + astma + lungesyk + hjerte + hypertensjon + diabetes + nyre +
                   immunsvak + smoke_current + smoke_ever +
                   asymptomatic0m + hosp + cohort, 
                 data = fpos, family = "binomial")
summary(model_cat)
exp(cbind(OR = coef(model_cat), confint(model_cat)))
rm(model_cat)

model_02 <- glm(fatigue ~ female + age15 
                + bmicat
                + anylungdis + hjerte + spikeg
                  # + hypertension + comorbid
                + cohort + outcome_antibiotika___1
                ,data = fpos, family = "binomial")
summary(model_02)
exp(cbind(OR = coef(model_02), confint(model_02)))
rm(model_02)

model_02 <- glm(fatigue ~ female + age + severity 
                #+ hosp + astma + lungesyk 
                + anylungdis + hjerte
                + bmi + liggetid 
                + spikeglog 
                #+ mnlog
                + outcome_antibiotika___1
                ,data = fpos, family = "binomial")
summary(model_02)
exp(cbind(OR = coef(model_02), confint(model_02)))
rm(model_02)

