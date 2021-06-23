library(tidyverse)
library(summarytools)

# Levels
d6 = d6 %>% mutate(negouthosp = relevel(negouthosp, "outpat"))
# FLOW chart
table(eligible$severity, eligible$negouthosp, useNA = "always")
freq(d$negouthosp, report.nas = F)
freq(eligible$negouthosp, report.nas = F)
table(eligible$villig, eligible$negouthosp)
freq(eligible$negouthosp)

freq(included$negouthosp)
freq(blood$negouthosp)
freq(d6$negouthosp)
freq(f$negouthosp)

table(d$villig,d$negouthosp)


## TABLE 1 - Study polulation
freq(d$covpos) # alle inlkluderte inkl 28 hm som ikkje har gitt prøve 
freq(d6$covpos) 
freq(d6$negouthosp) 

ctable(d6$age15, as.factor(d6$covpos))
ctable(d6$age15, as.factor(d6$negouthosp))

# Gender
ctable(d6$negouthosp, d6$female)
ctable(d6$covpos, d6$female)

femalelogit <- glm(hosp ~ female, data = d6pos, family = "binomial")
summary(femalelogit)
exp(cbind(OR = coef(femalelogit), confint(femalelogit)))
rm(femalelogit)

#Age
ctable(d6$negouthosp, d6$age15)
ctable(d6$covpos, d6$age15)

d6%>%
  group_by(covpos) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 

d6%>%
  group_by(negouthosp) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 

ageintlogit <- glm(hosp ~ ageint, data = d6pos, family = "binomial")
summary(ageintlogit)
exp(cbind(OR = coef(ageintlogit), confint(ageintlogit)))
rm(ageintlogit)

## T1 Body weight - BMI categories
ctable(d6$negouthosp, d6$bmicat)
ctable(d6$covpos, d6$bmicat)

ctable(d6$negouthosp, as.factor(d6$obese25))
ctable(d6$negouthosp, as.factor(d6$obese30))
ctable(d6$negouthosp, as.factor(d6$obese35))
bmi30logit <- glm(obese30 ~ negouthosp, data = d6, family = "binomial")
summary(bmi30logit)
exp(cbind(OR = coef(bmi30logit), confint(bmi30logit)))

d6%>%
  filter(!is.na(bmi)) %>%
  group_by(covpos) %>%
  summarise(n(), median(bmi), quantile(bmi, 1/4), quantile(bmi, 3/4), .groups = 'drop') 

d6%>%
  filter(!is.na(bmi)) %>%
  group_by(negouthosp) %>%
  summarise(n(), median(bmi), quantile(bmi, 1/4), quantile(bmi, 3/4), .groups = 'drop') 

bmilogit <- glm(hosp ~ bmiint, data = d6pos, family = "binomial")
summary(bmilogit)
exp(cbind(OR = coef(bmilogit), confint(bmilogit)))
rm(bmilogit)

mean(d6$vektoppgang, na.rm = T)
ggplot(d6, na.rm = T, aes(x=vektoppgang), na.rm = T)+ geom_histogram(bins = 100)

# Comorbidities
# Any comorbidity
ctable(d6$covpos, d6$comorbid, useNA = "no")
ctable(d6$negouthosp, d6$comorbid,useNA = "no")
comorbidlogit <- glm(hosp ~ comorbid, data = d6pos, family = "binomial")
summary(comorbidlogit)
exp(cbind(OR = coef(comorbidlogit), confint(comorbidlogit)))
rm(comorbidlogit)

# Asthma
ctable(d6$covpos, d6$astma, useNA = "no")
ctable(d6$negouthosp, d6$astma,useNA = "no")
astmalogit <- glm(hosp ~ astma, data = d6pos, family = "binomial")
summary(astmalogit)
exp(cbind(OR = coef(astmalogit), confint(astmalogit)))
rm(astmalogit)

# Other lung diseases
ctable(d6$covpos, d6$lungesyk, useNA = "no")
ctable(d6$negouthosp, d6$lungesyk, useNA = "no")
lungesyklogit <- glm(lungesyk ~ negouthosp, data = d6, family = "binomial")
summary(lungesyklogit)
exp(cbind(OR = coef(lungesyklogit), confint(lungesyklogit)))
rm(lungesyklogit)

# Any lung diseases
ctable(d6$covpos, d6$astma, useNA = "no")
ctable(d6$covpos, d6$anylungdis, useNA = "no")
ctable(d6$negouthosp, d6$anylungdis, useNA = "no")
anylungdislogit <- glm(hosp ~ anylungdis, data = d6pos, family = "binomial")
summary(anylungdislogit)
exp(cbind(OR = coef(anylungdislogit), confint(anylungdislogit)))
rm(anylungdislogit)

# Hypertension
ctable(d6$covpos, d6$hypertensjon, useNA = "no")
ctable(d6$negouthosp, d6$hypertensjon, useNA = "no")
hypertensjonlogit <- glm(hosp ~ hypertensjon, data = d6pos, family = "binomial")
summary(hypertensjonlogit)
exp(cbind(OR = coef(hypertensjonlogit), confint(hypertensjonlogit)))
rm(hypertensjonlogit)

# Chronic heart diseases
ctable(d6$covpos, d6$hjerte, useNA = "no")
ctable(d6$negouthosp, d6$hjerte, useNA = "no")
hjertelogit <- glm(hosp ~ hjerte, data = d6pos, family = "binomial")
summary(hjertelogit)
exp(cbind(OR = coef(hjertelogit), confint(hjertelogit)))
rm(hjertelogit)

# Chronic rheumatic disease
ctable(d6$covpos, d6$revma, useNA = "no")
ctable(d6$negouthosp, d6$revma, useNA = "no")
hjertelogit <- glm(hosp ~ revma, data = d6pos, family = "binomial")
summary(hjertelogit)
exp(cbind(OR = coef(hjertelogit), confint(hjertelogit)))
rm(hjertelogit)

# Diabetes
ctable(d6$covpos, d6$diabetes, useNA = "no")
ctable(d6$negouthosp, d6$diabetes, useNA = "no")
diabeteslogit <- glm(hosp ~ diabetes, data = d6pos, family = "binomial")
summary(diabeteslogit)
exp(cbind(OR = coef(diabeteslogit), confint(diabeteslogit)))
rm(diabeteslogit)

# Immunosuppression
ctable(d6$covpos, d6$immunsvak, useNA = "no")
ctable(d6$negouthosp, d6$immunsvak, useNA = "no", chisq = T)
immunsvaklogit <- glm(hosp ~ immunsvak, data = d6pos, family = "binomial")
summary(immunsvaklogit)
exp(cbind(OR = coef(immunsvaklogit), confint(immunsvaklogit)))
rm(immunsvaklogit)

# Current smoker
ctable(d6$covpos, d6$smoke_current, useNA = "no")
ctable(d6$negouthosp, d6$smoke_current, useNA = "no", chisq = T)
smoke_currentlogit <- glm(smoke_current ~ negouthosp, data = d6, family = "binomial")
summary(smoke_currentlogit)
exp(cbind(OR = coef(smoke_currentlogit), confint(smoke_currentlogit)))
rm(smoke_currentlogit)

# Ever smoker
ctable(d6$covpos, d6$smoke_ever, useNA = "no")
ctable(d6$negouthosp, d6$smoke_ever, useNA = "no", chisq = T)
smoke_everlogit <- glm(hosp ~ smoke_ever, data = d6pos, family = "binomial")
summary(smoke_everlogit)
exp(cbind(OR = coef(smoke_everlogit), confint(smoke_everlogit)))
rm(smoke_everlogit)

# Severity of initial illness
table(d6$severity, d6$negouthosp)
freq(d$severity)

d6%>%
  #group_by(negouthosp) %>%
  summarise(n(), median(severity), quantile(severity, 1/4), quantile(severity, 3/4), .groups = 'drop') 

d6%>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(severity), .groups = 'drop') 

severitylogit <- glm(hosp ~ severity, data = d6pos, family = "binomial")
summary(severitylogit)
exp(cbind(OR = coef(severitylogit), confint(severitylogit)))
rm(severitylogit)

d6 %>%
  #group_by(negouthosp) %>%
  summarise(n(), median(liggetid), quantile(liggetid, 1/4), quantile(liggetid, 3/4), .groups = 'drop') 

d6%>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(liggetid), .groups = 'drop') 

liggetidlogit <- glm(liggetid ~ negouthosp, data = d6, family = "poisson")
summary(liggetidlogit)
exp(cbind(OR = coef(liggetidlogit), confint(liggetidlogit)))
rm(liggetidlogit)

# lab
d6%>%
  #filter(covpos == 1) %>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(spikeglog10), .groups = 'drop') 

ablogit <- glm(hosp ~ spikeglog10, data = d6pos, family = "binomial")
summary(ablogit)
exp(cbind(OR = coef(ablogit), confint(ablogit)))
rm(ablogit)

d6%>%
  #filter(covpos == 1) %>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(mnlog10), .groups = 'drop') 

ablogit <- glm(hosp ~ mnlog10, data = d6pos, family = "binomial")
summary(ablogit)
exp(cbind(OR = coef(ablogit), confint(ablogit)))
rm(ablogit)
