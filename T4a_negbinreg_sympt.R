# No of symptoms - negative binomial
library(MASS)
library(tidyverse)

# Multivariable model with all p<0.1
mod <- glm.nb(antallsympt ~ female + ageint + bmiint
              + anylungdis + hjerte
              + severity + liggetid + 
              + spikeg01
              , data = d6pos)
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ female + age10 + bmiint
              + anylungdis + hjerte
              + liggetid 
              + severity + spikeglog10
              , data = d6pos)
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ severity + spikeglog10, data = d6pos)
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)
# univariable analysis - negativ binomial regression
mod <- glm.nb(antallsympt ~ female, data = d6pos) #sex
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ ageint, data = d6pos) #age
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ age10, data = d6pos) #age
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ bmiint, data = d6pos) #bmi
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ anylungdis, data = d6pos) #lung
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ hjerte, data = d6pos) #heart
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ hypertensjon, data = d6pos) #HT
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ revma, data = d6pos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ diabetes, data = d6pos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ nevro, data = d6pos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ hypothyreosis, data = d6pos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ cancer, data = d6pos) # DM
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ immunsvak, data = d6pos) # immuno
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ smoker, data = d6pos) # smoker
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Symptoms initial illness
mod <- glm.nb(antallsympt ~ Fever, data = d6pos) # Fever
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ Dyspnoea, data = d6pos) # Dyspnoea
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ Cough, data = d6pos) # Cough
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ Fatigue, data = d6pos) # Fatigue
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ Myalgia, data = d6pos) # Myalgia
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ Headache, data = d6pos) # Headache
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ Taste_Smell, data = d6pos) # Headache
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Organsvikter
mod <- glm.nb(antallsympt ~ outcome_respsvikt___1, data = d6pos) # Respiratory failure
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ outcome_nyresvikt___1, data = d6pos) # Renal failure
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ outcome_antibiotika___1, data = d6pos) # Renal failure
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)


# --- 

mod <- glm.nb(antallsympt ~ severity, data = d6pos) # severity
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ liggetid, data = d6pos) # liggetid
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ spikeglog10, data = d6pos) # spike
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ spikeg01, data = d6pos) # spike
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ mnlog10, data = d6pos) # MN
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm.nb(antallsympt ~ mn01, data = d6pos) # MN
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)
