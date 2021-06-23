# T3 multivariabel - linear and zero-inflated poisson
library(tidyverse)
library(summarytools)
library(rcompanion)
library(Rmisc)
  

tomas <- d6pos %>%
  select()
# difference IgG titres in hospitalized and home-isolated
wilcox.test(spikeglog10 ~ hosp, alternative = c("two.sided"), data = d6pos)

# Visual normality check

hist(d6pos$spikeglog10, probability=T, main="Histogram - normality check",xlab="Value of antibodies")
lines(density(d6pos$spikeglog10),col=2)

qqnorm(d6pos$spikeglog10, main="QQ plot for normality check")
qqline(d6pos$spikeglog10)

# Shapiro Wilk-test
shapiro.test(d6pos$spikeglog10[d6pos$spikeglog10>log10(150)])

# Kolmogorov-Smirnov test (?more appropriate as n > 100, here 312)
ks.test(d6pos$spikeglog10[d6pos$spikeglog10>log10(150)], pnorm)

tomas <- d6pos %>%
  select(spikeg, spikegloge, spikeglog10, female, age10, 
         bmiint, anylungdis, hypertensjon, hjerte, revma, diabetes, immunsvak, smoker, severity, liggetid, hosp)



# spike
mod <- lm(spikegloge ~ female + age10 + bmiint
           + hypertensjon + hjerte +  diabetes
          + severity + liggetid
          # + anylungdis + immunsvak + smoker
          , data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)


freq(d6pos$female, round.digits = 0)
wilcox.test(spikeglog10 ~ female, alternative = c("two.sided"), data = d6pos)
cor.test(d6pos$spikeglog10, as.numeric(d6pos$female), method = c("pearson"))
groupwiseGeometric(spikegloge ~ female, data = d6pos) # geometric mean

library(uwIntroStats)
regress("mean", spikeg ~ female, data = d6pos)


mod <- lm(spikegloge ~ female, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

mod <- lm(spikegloge ~ age10, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

mod <- lm(spikegloge ~ bmiint, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

freq(d6pos$anylungdis, round.digits = 0)
wilcox.test(spikeglog10 ~ anylungdis, alternative = c("two.sided"), data = d6pos)

mod <- lm(spikegloge ~ anylungdis, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

freq(d6pos$hypertensjon, round.digits = 0)
wilcox.test(spikeglog10 ~ hypertensjon, alternative = c("two.sided"), data = d6pos)

mod <- lm(spikegloge ~ hypertensjon, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

freq(d6pos$hjerte, round.digits = 0)
wilcox.test(spikeglog10 ~ hjerte, alternative = c("two.sided"), data = d6pos)
mod <- lm(spikegloge ~ hjerte, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

freq(d6pos$revma, round.digits = 0)
wilcox.test(spikeglog10 ~ revma, alternative = c("two.sided"), data = d6pos)
mod <- lm(spikegloge ~ revma, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

freq(d6pos$diabetes, round.digits = 0)
wilcox.test(spikeglog10 ~ diabetes, alternative = c("two.sided"), data = d6pos)
mod <- lm(spikegloge ~ diabetes, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

freq(d6pos$immunsvak, round.digits = 0)
wilcox.test(spikeglog10 ~ immunsvak, alternative = c("two.sided"), data = d6pos)
mod <- lm(spikegloge ~ immunsvak, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

freq(d6pos$smoker, round.digits = 0)
wilcox.test(spikegloge ~ smoker, alternative = c("two.sided"), data = d6pos)
mod <- lm(spikegloge ~ smoker, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

mod <- lm(spikegloge ~ severity, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)

mod <- lm(spikegloge ~ liggetid, data = d6pos)
summary(mod)
exp(cbind(GMR = coef(mod), confint(mod)))
rm(mod)
