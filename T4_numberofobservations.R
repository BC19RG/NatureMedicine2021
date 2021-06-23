# Frequencies of risk factors
library(summarytools)
freq(d6pos$female, round.digits = 0)
freq(d6pos$anylungdis, round.digits = 0) # 38
freq(d6pos$hjerte, round.digits = 0) # 22 
freq(d6pos$hypertensjon, round.digits = 0) # 35

freq(d6pos$diabetes, round.digits = 0) # 13
table(d6pos$diabetes, d6pos$oppf_6m_risikofakt___11) # 1-12-1
table(d6pos$diabetes, d6pos$crf_risk_diabetes_risk6m) # 2 ok

freq(d6pos$revma, round.digits = 0) # 20
freq(d6pos$nevro, round.digits = 0) # 8
freq(d6pos$hypothyreosis, round.digits = 0) # 8
freq(d6pos$cancer, round.digits = 0) # 8

freq(d6pos$immunsvak, round.digits = 0) # 11

freq(d6pos$crf_risk_leversykdom, round.digits = 0) # 2
freq(d6pos$oppf_6m_risikofakt___4, round.digits = 0) # hm lever 2
freq(d6pos$crf_risk_nyresvikt, round.digits = 0) # 2
freq(d6pos$crf_risk_nevrologisk, round.digits = 0) # 8
freq(d6pos$oppf_6m_risikofakt___3, round.digits = 0) # hm nevrologisk 11


freq(d6pos$crf_risk_demens, round.digits = 0) # 0
freq(d6pos$crf_risk_revma, round.digits = 0) # 20
fre
freq(d6pos$crf_risk_malignitet, round.digits = 0) # 5
freq(d6pos$crf_risk_annen, round.digits = 0) # 66
freq(d6pos$crf_risk_immunsvikt_tillegg1___1, round.digits = 0) # 0
freq(d6pos$crf_risk_immunsvikt_tillegg1___2, round.digits = 0) # 1
freq(d6pos$crf_risk_immunsvikt_tillegg1___3, round.digits = 0) # 0
freq(d6pos$crf_risk_immunsvikt_tillegg1___4, round.digits = 0) # 1
freq(d6pos$crf_risk_immunsvikt_tillegg1___5, round.digits = 0) # 0
freq(d6pos$crf_risk_immunsvikt_tillegg1___6, round.digits = 0) # 1
freq(d6pos$crf_risk_immunsvikt_tillegg1___7, round.digits = 0) # 3
freq(d6pos$crf_risk_immunsvikt_tillegg1___8, round.digits = 0) # 5

crf_risk_immunsvikt_tillegg1___2
freq(d6pos$smoker, round.digits = 0)
freq(d6pos$Fever, round.digits = 0)
freq(d6pos$Dyspnoea, round.digits = 0)
freq(d6pos$Cough, round.digits = 0)
freq(d6pos$Fatigue, round.digits = 0)
freq(d6pos$Myalgia, round.digits = 0)
freq(d6pos$Headache, round.digits = 0)
freq(d6pos$Taste_Smell, round.digits = 0)
freq(d6pos$outcome_respsvikt___1, round.digits = 0)
freq(d6pos$outcome_nyresvikt___1, round.digits = 0)
freq(d6pos$outcome_antibiotika___1, round.digits = 0)
freq(d6pos$spikeg01, round.digits = 0)
freq(d6pos$mn01, round.digits = 0)

freq(fpos$female, round.digits = 0)
freq(fpos$anylungdis, round.digits = 0)
freq(fpos$hjerte, round.digits = 0)
freq(fpos$hypertensjon, round.digits = 0)
freq(fpos$revma, round.digits = 0) # 20
freq(fpos$diabetes, round.digits = 0)
freq(fpos$immunsvak, round.digits = 0)
freq(fpos$smoker, round.digits = 0)
freq(fpos$Fever, round.digits = 0)
freq(fpos$Dyspnoea, round.digits = 0)
freq(fpos$Cough, round.digits = 0)
freq(fpos$Fatigue, round.digits = 0)
freq(fpos$Myalgia, round.digits = 0)
freq(fpos$Headache, round.digits = 0)
freq(fpos$Taste_Smell, round.digits = 0)
freq(fpos$outcome_respsvikt___1, round.digits = 0)
freq(fpos$outcome_nyresvikt___1, round.digits = 0)
freq(fpos$outcome_antibiotika___1, round.digits = 0)
freq(fpos$spikeg01, round.digits = 0)
freq(fpos$mn01, round.digits = 0)


freq(d6spikepos$female, round.digits = 0)
freq(d6spikepos$anylungdis, round.digits = 0)
freq(d6spikepos$hjerte, round.digits = 0)
freq(d6spikepos$hypertensjon, round.digits = 0)
freq(d6spikepos$diabetes, round.digits = 0)
freq(d6spikepos$immunsvak, round.digits = 0)
freq(d6spikepos$smoker, round.digits = 0)
freq(d6spikepos$spikeg01, round.digits = 0)
freq(d6spikepos$mn01, round.digits = 0)


freq(fspikepos$female, round.digits = 0)
freq(fspikepos$anylungdis, round.digits = 0)
freq(fspikepos$hjerte, round.digits = 0)
freq(fspikepos$hypertensjon, round.digits = 0)
freq(fspikepos$diabetes, round.digits = 0)
freq(fspikepos$immunsvak, round.digits = 0)
freq(fspikepos$smoker, round.digits = 0)
freq(fspikepos$spikeg01, round.digits = 0)
freq(fspikepos$mn01, round.digits = 0)
