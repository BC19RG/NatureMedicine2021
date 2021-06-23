# Table S3

library(tidyverse)
library(gridExtra)

d6pos$f_hosp <- NA
d6pos$f_hosp[d6pos$hosp == 0] <- "Home-isolated"
d6pos$f_hosp[d6pos$hosp == 1] <- "Hospitalised"
d6pos$f_hosp <- as.factor(d6pos$f_hosp)

cor1 <- ggplot(d6pos, aes(severity, spikeglog10))+ #, color = as.factor(f_hosp)
  geom_point(size = 2, position = position_jitter(w = 0.25, h = 0))+
  geom_smooth(method = "lm")+
  ggtitle("a", subtitle = "Severity vs. antibody titres") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab("Spike IgG titres (log 10)")+
  xlab("Severity*")


pss <- ggplot(d6pos, aes(spikeglog10, antallsympt, color = c(f_hosp)))+
  geom_point(size = 2, show.legend = FALSE, position = position_jitter(w = 0, h = 0.1))+
  geom_smooth(method = "lm", se = F, show.legend = FALSE)+
  facet_wrap(~as.factor(f_hosp), nrow = 1)+
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12))+
  ggtitle("b", subtitle = "Antibody titres vs number of symptoms") +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  xlab("Spike IgG titres (log 10)")+
  ylab("No. of symptoms (0-13)")

panels <- arrangeGrob(cor1, pss, nrow = 1)
ggsave(file="ExtDataFig1.eps", panels, width=8.8, height=4.4, dpi=600, limitsize = F)
ggsave(file="ExtDataFig1.pdf", panels, width=8.8, height=4.4, dpi=600, limitsize = F)

