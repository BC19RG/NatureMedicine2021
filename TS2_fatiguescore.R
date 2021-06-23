# T2 chalder
ctable(f$negouthosp, f$fatigue)
ctable(f$age15, f$negouthosp)
ctable(f$covpos, f$fatigue, OR = T, chisq = T)
mod <- glm(fatigue ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, f$severefatigue)
ctable(f$covpos, f$severefatigue, OR = T, chisq = T)
mod <- glm(severefatigue ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

f%>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(chalderscore), .groups = 'drop') 
mod <- glm(chalderscore ~ covpos, data = f, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

f %>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(chdfysav), .groups = 'drop') 
mod <- glm(chdfysav ~ covpos, data = f, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

f %>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(chdmentav), .groups = 'drop') 
mod <- glm(chdmentav ~ covpos, data = f, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Gender
ctable(f$covpos, as.factor(f$female))
ctable(f$negouthosp, as.factor(f$female))

mod <- glm(female ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Alder
f%>%
  group_by(covpos) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 

f%>%
  group_by(negouthosp) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 

mod <- glm(ageint ~ negouthosp, data = f, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Chalder fysisk spørsmål
ctable(f$negouthosp, as.factor(f$chdf01sliten))
ctable(f$covpos, as.factor(f$chdf01sliten), OR = T, chisq = T)
mod <- glm(chdf01sliten ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdf01hvile))
ctable(f$covpos, as.factor(f$chdf01hvile), OR = T, chisq = T)
mod <- glm(chdf01hvile ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdf01sovnig))
ctable(f$covpos, as.factor(f$chdf01sovnig), OR = T, chisq = T)
mod <- glm(chdf01sovnig ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdf01kommeigang))
ctable(f$covpos, as.factor(f$chdf01kommeigang), OR = T, chisq = T)
mod <- glm(chdf01kommeigang ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdf01overskudd))
ctable(f$covpos, as.factor(f$chdf01overskudd), OR = T, chisq = T)
mod <- glm(chdf01overskudd ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdf01styrke))
ctable(f$covpos, as.factor(f$chdf01styrke), OR = T, chisq = T)
mod <- glm(chdf01styrke ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdf01svak))
ctable(f$covpos, as.factor(f$chdf01svak), OR = T, chisq = T)
mod <- glm(chdf01svak ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Chalder mental spørsmål
ctable(f$negouthosp, as.factor(f$chdm01konsentr))
ctable(f$negouthosp, as.factor(f$konsx))
ctable(f$covpos, as.factor(f$chdm01konsentr), OR = T, chisq = T)
mod <- glm(chdm01konsentr ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdm01forsnakke))
ctable(f$covpos, as.factor(f$chdm01forsnakke), OR = T, chisq = T)
mod <- glm(chdm01forsnakke ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdm01ordleting))
ctable(f$covpos, as.factor(f$chdm01ordleting), OR = T, chisq = T)
mod <- glm(chdm01ordleting ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

ctable(f$negouthosp, as.factor(f$chdm01hukommelse))
ctable(f$covpos, as.factor(f$chdm01hukommelse), OR = T, chisq = T)
mod <- glm(chdm01hukommelse ~ negouthosp, data = f, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# ----------- MULTIVARIABLE ANALYSIS ----------------
modfat <- glm(hosp ~ 
                female + 
                ageint + 
                #severity 
                #hosp 
                #+ astma + lungesyk 
                #+ anylungdis + hjerte
                #+ bmi 
                #+ liggetid 
                #+ spikeglog 
                #+ mnlog
                #+ outcome_antibiotika___1
                #+ m6sympt01x   
                + chdf01sliten + chdf01hvile + chdf01sovnig + chdf01kommeigang
              + chdf01overskudd + chdf01styrke + chdf01svak
              + chdm01konsentr + chdm01forsnakke + chdm01ordleting + chdm01hukommelse
              #+ m6feber + m6hoste + m6hjertebank + m6smaklukt + m6hodepine
              ,data = fpos, family = "binomial")
summary(modfat)
exp(cbind(OR = coef(modfat), confint(modfat)))
rm(modfat)
