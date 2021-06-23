# TABLE 2 - complications Cov +/- at 6 months
library(tidyverse)
library(summarytools)
library(gmodels)
library(pscl)

# levels
# d = d %>% mutate(negouthosp = relevel(negouthosp, "outpat"))
d6opd = d6opd %>% mutate(age15 = relevel(age15, "46-60"))
d6opd$age15 <- fct_relevel(d6opd$age15, "46-60", "0-15", "16-30", "31-45", "over 60") 
d6opd_16$age15 <- fct_relevel(d6opd_16$age15, "46-60", "0-15", "16-30", "31-45", "over 60") 

# totals
freq(d6$covpos)
freq(d6opd$age15)

# assumtpions
d6 %>%
  ggplot(aes(x = ageint))+
  geom_histogram(bins = 80)
           
# Gender
ctable(d6_16$covpos, as.factor(d6_16$female), useNA = "no", OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$female), useNA = "no", OR = T, chisq = T)
ctable(as.factor(d6opd$hosp), as.factor(d6opd$female), useNA = "no", OR = T, chisq = T)

mod <- glm(ageint ~ female, data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(female ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ female, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# age
d6%>%
  group_by(covpos) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 

d6opd %>%
  group_by(age15) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 

d6opd%>%
  group_by(hosp) %>%
  summarise(n(), median(ageint), quantile(ageint, 1/4), quantile(ageint, 3/4), .groups = 'drop') 

mod <- glm(as.factor(hosp) ~ ageint, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

#---any symptom
ctable(d6$covpos, as.factor(d6$m6sympt01x), OR = T, chisq = T, useNA = "no")
ctable(d6opd$age15, as.factor(d6opd$m6sympt01x), OR = T, chisq = T, useNA = "no")

mod <- glm(ageint ~ as.factor(m6sympt01x), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6sympt01x ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# feber
ctable(d6$covpos, as.factor(d6$m6feber), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6feber))

mod <- glm(ageint ~ as.factor(m6feber), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6feber ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)
# hoste
ctable(d6$covpos, as.factor(d6$m6hoste), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6hoste))

mod <- glm(ageint ~ as.factor(m6hoste), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6hoste ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# dyspnea
ctable(d6$covpos, as.factor(d6$m6tungpust), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6tungpust))

mod <- glm(ageint ~ as.factor(m6tungpust), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6tungpust ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# palpitations
ctable(d6$covpos, as.factor(d6$m6hjertebank), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6hjertebank))

mod <- glm(ageint ~ as.factor(m6hjertebank), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6hjertebank ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# mage
ctable(d6$covpos, as.factor(d6$m6mage), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6mage))

mod <- glm(ageint ~ as.factor(m6mage), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6mage ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# taste smell
ctable(d6$covpos, as.factor(d6$m6smaklukt), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6smaklukt))

mod <- glm(ageint ~ as.factor(m6smaklukt), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6smaklukt ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# fatigue
ctable(d6$covpos, as.factor(d6$fatiguex), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$fatiguex))
ctable(fposopd$age15, as.factor(fposopd$fatiguex))

mod <- glm(fatiguex ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(fatiguex ~ hosp, data = fpos, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(fatiguex ~ age15, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# kons
ctable(d6$covpos, as.factor(d6$konsx), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$konsx))
ctable(fposopd$age15, as.factor(fposopd$konsx))

mod <- glm(konsx ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(konsx ~ hosp, data = fpos, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(konsx ~ age15, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# memory
ctable(d6$covpos, as.factor(d6$memoryx), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$memoryx))
ctable(fposopd$age15, as.factor(fposopd$memoryx))

mod <- glm(memoryx ~ ageint, data = fposopd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(memoryx ~ hosp, data = fpos, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(memoryx ~ age15, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# sleep
ctable(d6$covpos, as.factor(d6$m6sovn), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6sovn))

mod <- glm(ageint ~ as.factor(m6sovn), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6sovn ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# headache
ctable(d6$covpos, as.factor(d6$m6hodepine), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6hodepine))

mod <- glm(ageint ~ as.factor(m6hodepine), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6hodepine ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# dizzyness
ctable(d6$covpos, as.factor(d6$m6svimmel), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6svimmel))

mod <- glm(ageint ~ as.factor(m6svimmel), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6svimmel ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))


rm(mod)

# tingling
ctable(d6$covpos, as.factor(d6$m6prikking), OR = T, chisq = T)
ctable(d6opd$age15, as.factor(d6opd$m6prikking))

mod <- glm(ageint ~ as.factor(m6prikking), data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6prikking ~ age15, data = d6opd, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# No of symptoms
d6%>%
  group_by(covpos) %>%
  summarise(n(), mean_cl_normal(antallsympt), .groups = 'drop') 

d6opd%>%
  group_by(age15) %>%
  summarise(n(), mean_cl_normal(antallsympt),  .groups = 'drop') 

d6opd %>%
  group_by(age15) %>%
  summarise(n(), median(antallsympt), quantile(antallsympt, 1/4), quantile(antallsympt, 3/4), .groups = 'drop') 


mod <- glm(ageint ~ antallsympt, data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(antallsympt ~ age15, data = d6opd, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# NUMBER OF SYMPTOMS - ZERO INFLATED POISSON

# Visual normality check

hist(d6opd$antallsympt, probability=T, main="Histogram - normality check",xlab="Value of antibodies")
lines(density(d6opd$antallsympt),col=2)

qqnorm(d6opd$antallsympt, main="QQ plot for normality check")
qqline(d6opd$antallsympt)

# Shapiro Wilk-test
shapiro.test(d6opd$antallsympt[d6opd$antallsympt>0])
shapiro.test(d6opd$antallsympt)
# Kolmogorov-Smirnov test (?more appropriate as n > 100, here 312)
ks.test(d6pos$spikeglog10[d6pos$spikeglog10>log10(150)], pnorm)

# transpose negatives for zero-inflated poisson regression
d6pos$spikeglog10transp <- d6pos$spikeglog10
d6pos$spikeglog10transp[d6pos$spikeglog10 < log10(150)] <- 0


# spike - zero-inflated
mod <-  zeroinfl(antallsympt ~ age15, data = d6opd)
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# Second last column - age integer variabel versus symptoms
mod <- glm(female ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6sympt01x ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6feber ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6hoste ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6tungpust ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6hjertebank ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6mage ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6smaklukt ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(fatiguex ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(konsx ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(memoryx ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6sovn ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6hodepine ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6svimmel ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(m6prikking ~ ageint, data = d6opd_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(as.integer(antallsympt) ~ ageint, data = d6opd_16, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)


# Last column - hosp/opd versus symptoms
mod <- glm(hosp ~ ageint, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ female, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6sympt01x, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6feber, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6hoste, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6tungpust, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6hjertebank, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6mage, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6smaklukt, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ fatiguex, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ konsx, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ memoryx, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6sovn, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6hodepine, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6svimmel, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ m6prikking, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

mod <- glm(hosp ~ antallsympt, data = d6pos_16, family = "binomial")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# ------ Multivariable analysis ----------

mod <- glm(ageint ~ female
           # m6feber 
           + m6hoste + m6tungpust + m6hjertebank + m6mage
           + m6smaklukt + memoryx + konsx + m6sovn + m6hodepine
           + m6svimmel + m6prikking + fatiguex
           , data = d6opd, family = "poisson")
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))
rm(mod)

# ---------
