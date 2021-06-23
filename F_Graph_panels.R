# PANELS FOR MAIN FIGURE
library(tidyverse)
library(gridExtra)

# New dataframe for symptoms

d6age <- d6opd %>%
  group_by(age15)%>%
  summarise(
    #n_sympt = count(m6sympt01x),
    av_sympt = mean(m6sympt01x),
    sd_sympt = sd(m6sympt01x),
    #iqr_sympt = iqr
    av_cough = mean(m6hoste),
    av_dyspnea = mean(m6tungpust),
    av_taste = mean(m6smaklukt),
    av_memory = mean(memoryx),
    av_conc = mean(konsx),
    av_fatigue = mean(fatiguex),
    .groups = 'drop')

# SYMPTOMS - 1st line - 4 panels - not used in final figure

ggplot(d6age)+ 
  geom_col(aes(x = age15, y = av_sympt), fill = "darkgrey")+
  geom_col(aes(x = age15, y = av_memory), fill = "darkblue")+
  stat_summary(x = age15, y = d6age$av_sympt, geom = "errorbar", fun.data = mean_se, position = "dodge")+
  ylim(0, 1)+
  ggtitle("a1. Memory loss")+ xlab("Age cohorts") + ylab("Proportion")


p1 <- ggplot(d6age)+ 
  geom_col(aes(x = age15, y = av_sympt), fill = "darkgrey")+
  geom_col(aes(x = age15, y = av_memory), fill = "darkblue")+
  ylim(0, 1)+
  ggtitle("a1. Memory loss")+ xlab("Age cohorts") + ylab("Proportion")

p2 <- ggplot(d6age)+ 
  geom_col(aes(x = age15, y = av_sympt), fill = "darkgrey")+
  geom_col(aes(x = age15, y = av_conc), fill = "blue")+
  ylim(0, 1)+
  ggtitle("a2. Concentration impairment")+ xlab("Age cohorts") + ylab("")

p3 <- ggplot(d6age)+ 
  geom_col(aes(x = age15, y = av_sympt), fill = "darkgrey")+
  geom_col(aes(x = age15, y = av_dyspnea), fill = "darkgreen")+
  ylim(0, 1)+
  ggtitle("a3. Dyspnoea")+ xlab("Age cohorts") + ylab("")

p4 <- ggplot(d6age)+ 
  geom_col(aes(x = age15, y = av_sympt), fill = "darkgrey")+
  geom_col(aes(x = age15, y = av_taste), fill = "darkred")+
  ylim(0, 1)+
  ggtitle("a4. Taste/smell disturbance")+ xlab("Age cohorts") + ylab("")

# Used in final figure from here
# Spike ANTIBODIES 2nd line - 4 panels
p5 <- ggplot(d6pos, aes(x = as.factor(severity), y = spikeg, fill = as.factor(severity))) +
  geom_boxplot(show.legend = FALSE)+ 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  ggtitle("a", subtitle = "Severity of initial illness") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5))+
  xlab("Severity")+
  ylab("Spike IgG titres")

p6 <- ggplot(d6pos, aes(x = as.factor(age15), y = spikeg, fill = as.factor(age15))) +
  geom_boxplot(show.legend = FALSE)+ 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  ggtitle("b", subtitle = "Age") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Age")+
  ylab("Spike IgG titres")

p7 <- ggplot(d6pos, aes(x = as.factor(spikegroups), y = antallsympt, fill = as.factor(spikegroups))) +
  geom_violin(show.legend = FALSE)+ 
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12))+
  ggtitle("c", subtitle = "Symptoms") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Spike IgG titres")+
  ylab("No. of symptoms")

p8 <- ggplot(d6pos, aes(x = as.factor(spikegroups), y = chalderscore, fill = as.factor(spikegroups))) +
  geom_violin(show.legend = FALSE)+ 
  ggtitle("d", subtitle = "Fatigue") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Spike IgG titres")+
  ylab("Fatigue score")

# MN antibodies 
p9 <- ggplot(d6pos, aes(x = as.factor(severity), y = mn, fill = as.factor(severity))) +
  geom_boxplot(show.legend = FALSE)+ 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  ggtitle("e", subtitle = "Severity of initial illness") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Severity")+
  ylab("Microneutralising antibody titres")

p10 <- ggplot(d6pos, aes(x = as.factor(age15), y = mn, fill = as.factor(age15))) +
  geom_boxplot(show.legend = FALSE)+ 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  ggtitle("f", subtitle = "Age") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Age")+
  ylab("Microneutralising antibody titres")

p11 <- ggplot(d6pos, aes(x = as.factor(mngroups), y = antallsympt, fill = as.factor(mngroups))) +
  geom_violin(show.legend = FALSE)+ 
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12))+
  ggtitle("g", subtitle = "Symptoms") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Microneutralising antibody titres")+
  ylab("No. of symptoms")

p12 <- ggplot(d6pos, aes(x = as.factor(mngroups), y = chalderscore, fill = as.factor(mngroups))) +
  geom_violin(show.legend = FALSE)+ 
  ggtitle("h", subtitle = "Fatigue") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Microneutralising antibody titres")+
  ylab("Fatigue score")



panels <- arrangeGrob(p5, p6, p7, p8, p9, p10, p11, p12, nrow=2)
ggsave(file="Fig2_panels.eps", panels, width=18, height=9, dpi=600, limitsize = F)
ggsave(file="Fig2_panels.pdf", panels, width=18, height=9, dpi=600, limitsize = F)
