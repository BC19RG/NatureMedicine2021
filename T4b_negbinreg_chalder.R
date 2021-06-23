# Chalder score - negative binomial
library(MASS)
library(epitools)
library(summarytools)

# multivariable only p<0.1
mod <- glm.nb(chalderscore ~ female + ageint + bmiint
              + anylungdis + hjerte + hypertensjon + diabetes + revma
              + severity + liggetid
              + spikeg01
              , data = fpos)
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ female + age10 + bmiint
              + anylungdis + hypertensjon + hjerte + revma + diabetes
              + liggetid
              + severity + spikeglog10
              , data = fpos)
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# univariable analysis - negativ binomial regression
mod <- glm.nb(chalderscore ~ female, data = fpos) #sex
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ age10, data = fpos) #age
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ bmiint, data = fpos) #bmi
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ anylungdis, data = fpos) #lung
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ hjerte, data = fpos) #heart
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ hypertensjon, data = fpos) #HT
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ revma, data = fpos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ diabetes, data = fpos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ nevro, data = fpos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ hypothyreosis, data = fpos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ immunsvak, data = fpos) # immuno
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ cancer, data = fpos) # immuno
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ smoker, data = fpos) # smoker
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Symptoms initial illness
mod <- glm.nb(chalderscore ~ Fever, data = fpos) # Fever
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ Dyspnoea, data = fpos) # Dyspnoea
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ Cough, data = fpos) # Cough
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ Fatigue, data = fpos) # Fatigue
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ Myalgia, data = fpos) # Myalgia
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ Headache, data = fpos) # Headache
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ Taste_Smell, data = fpos) # Taste
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Organsvikter
mod <- glm.nb(chalderscore ~ outcome_respsvikt___1, data = fpos) # Respiratory failure
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ outcome_nyresvikt___1, data = fpos) # Renal failure
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ outcome_antibiotika___1, data = fpos) # Renal failure
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)
# ----
mod <- glm.nb(chalderscore ~ severity, data = fpos) # severity
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ liggetid, data = fpos) # liggetid
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ spikeglog10, data = fpos) # spike
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ spikeg01, data = fpos) # spike
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ mnlog10, data = fpos) # MN
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(chalderscore ~ mn01, data = fpos) # MN
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)
