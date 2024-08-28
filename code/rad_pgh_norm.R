### Pittsburgh Randon Data Project 

rm(list = ls())
library(tidyverse)
#library(ggplot2)
library(DataExplorer)
#library(dplyr)
library(ggpubr)
library(plyr)
library(tidyquant)
library(forecast)
library(zoo)
library(xts)
library(nortest)
library(TTR)
#library(tidyr)
library(readxl)
library(rlang)
library(reshape2)
library(magrittr)
library(glue)
library(ggtext)
library(gtsummary)
library(ISLR)
library(patchwork)
library(plotly)
library(scales)
library(janitor)


## Working directry 
getwd()
setwd("/Users/takayukitamura/Documents/R_Computing/rad_pa/")

## read.cvs 
rad_pa <- read.table("/Users/takayukitamura/Documents/R_Computing/rad_pa/rad_pa.csv", sep = ",",
                     header = TRUE, stringsAsFactors = FALSE) %>% 
  rename()


## remove na
rad_pa <- rad_pa %>% 
  na.omit() %>% 
  select(Address.Postal.Code, County.Name, Test.Floor.Level, Measure.Value, Test.Start.Date)
sum(is.na(rad_pa))

str(rad_pa)
dim(rad_pa)
summary(rad_pa)
head(rad_pa)

# write.csv(rad_pa,"rad_pa.csv")
# rad_pa <- read.csv("rad_pa.csv")

# rad_pa <-  rad_pa %>% 
#  select(!"X")

colnames(rad_pa)
summary(rad_pa)
summary(rad_pa$Measure.Value)
quantile(rad_pa$Measure.Value, 0.975)
quantile(rad_pa$Measure.Value, 0.025)

## how distribute with qqnorm, qqline, boxplot
rad_pa %>% 
  ggplot(aes(x = Measure.Value)) +
  geom_boxplot()+
  coord_flip()

qqnorm(rad_pa$Measure.Value)
qqline(rad_pa$Measure.Value)
boxplot(rad_pa$Measure.Value)

## stats_pa, summarise Measure.Value
stats_rad_pa <- rad_pa %>% 
  summarise(minimum = min(Measure.Value, round(2)),
            median = median(Measure.Value, round(2)),
            mean = mean(Measure.Value),
            maximum = max(Measure.Value, round(2)),
            sd = sd(Measure.Value, round(2)),
            n = length(Measure.Value))

quantile(rad_pa$Measure.Value)
var(rad_pa$Measure.Value)
sd(rad_pa$Measure.Value)
length(rad_pa$Measure.Value)
rad_pa %>% 
  arrange(-Measure.Value)

## histogram 
hist_pa <- ggplot(rad_pa, aes(x = Measure.Value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(Measure.Value, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in PA') +
  theme_bw()
hist_pa

## count of Radon Value at 20 or higher 
rad_count <- rad_pa %>% 
  filter(Measure.Value >= 20)
length(rad_count$Measure.Value)
percent <- (length(rad_count$Measure.Value))/(length(rad_pa$Measure.Value))*100
percent

## excluding out-layers with z-score 3.3 rule
colnames(rad_pa)
z_score_pa <- (rad_pa$Measure.Value - mean(rad_pa$Measure.Value))/sd(rad_pa$Measure.Value)
rad_pa$z_score <- z_score_pa  
colnames(rad_pa)
rad_pa <- rad_pa %>% 
  select(Test.Start.Date,Address.Postal.Code, County.Name,Test.Floor.Level,
         Measure.Value, z_score)

rad_count_z <- rad_pa %>% 
  filter(z_score >= 3.3)
length(rad_count_z$z_score)
percent <- (length(rad_count_z$z_score))/(length(rad_pa$z_score))*100
percent

rad_pa_norm <- rad_pa %>% 
  filter(z_score <= 3.3)

summary(rad_pa_norm$Measure.Value)

rad_count <- rad_pa_norm %>% 
  filter(Measure.Value >= 20)
length(rad_count$Measure.Value)
percent <- (length(rad_count$Measure.Value))/(length(rad_pa$Measure.Value))*100
percent

summary_pa_norm <- rad_pa_norm %>% 
  summarise("minimum" = min(Measure.Value, round(2)),
            "median" = median(Measure.Value, round(2)),
            "mean" = mean(Measure.Value),
            "maximum" = max(Measure.Value, round(2)),
            "sd" = sd(Measure.Value, round(2)),
            "count" = n()) %>% 
  round(1)

## data for Pittsburgh 
rad_pgh_ds <- rad_pa %>% 
  filter(County.Name %in% c("Allegheny","Washington", "Beaver", "Westmoreland", "Butler" ))

str(rad_pgh_ds)
length(rad_pgh_ds$Measure.Value)

rad_pgh_ds %>% 
  summarise("minimum" = min(Measure.Value),
            "median" = median(Measure.Value),
            "mean" = mean(Measure.Value),
            "maximum" = max(Measure.Value),
            "sd" = sd(Measure.Value),
            "count" = n()) %>% 
  round(1)

summary_pgh_ds <- rad_pgh_ds %>% 
  summarise(minimum = min(Measure.Value, round(2)),
            median = median(Measure.Value, round(2)),
            mean = mean(Measure.Value, round(2)),
            maximum = max(Measure.Value, round(2)),
            sd = sd(Measure.Value, round(2)),
            n = length(Measure.Value))

#rad_pgh_ds <- rad_pa_df %>% 
#  filter(County.Name ==  "Allegheny"|County.Name == "Washington"|County.Name == "Beaver "|County.Name == "Westmoreland "|County.Name == "Butler" ) %>% 
#  select("Test.Start.Date", "Address.Postal.Code","County.Name","Municipality.Name",
#         "Measure.Value") 

colnames(rad_pgh_ds)
head(rad_pgh_ds)
rad_pgh_ds %>% 
  arrange(Measure.Value)

rad_pgh_ds %>% 
  arrange(-Measure.Value)

## visualization
ggplot(rad_pgh_ds, aes(x = Measure.Value, y = z_score)) +
  geom_point()
         
rad_pgh_ds %>% 
  arrange(-z_score)

## create normalized data.set with 3.3 z-score
rad_pgh_norm <- rad_pgh_ds %>% 
  filter(z_score < 3.3)
length((rad_pgh_norm$Measure.Value))

rad_pgh_out <- rad_pgh_ds %>% 
  filter(z_score >= 3.3)
length(rad_pgh_out$Measure.Value)

rad_pgh_out %>% 
  arrange(Measure.Value)

(length(rad_pgh_ds$Measure.Value) - length(rad_pgh_out$Measure.Value))

boxplot(rad_pgh_norm$Measure.Value)
summary(rad_pgh_norm$Measure.Value)

summary_pgh_norm <- rad_pgh_norm %>% 
  summarise(minimum = min(Measure.Value, round(2)),
            median = median(Measure.Value, round(2)),
            mean = mean(Measure.Value),
            maximum = max(Measure.Value, round(2)),
            sd = sd(Measure.Value, round(2)),
            n = length(Measure.Value))

pgh_stats <- rad_pgh_norm %>% 
  summarise("minimum" = min(Measure.Value),
         "median" = median(Measure.Value),
         "mean" = mean(Measure.Value),
         "maximum" = max(Measure.Value),
         "sd" = sd(Measure.Value),
         "count" = length(Measure.Value)) %>% 
  round(2)

## understand the distribution better with percentage of each segment
rad_pgh_0.5 <- rad_pgh_ds %>% 
  filter(Measure.Value <= 0.5)
length(rad_pgh_0.5$Measure.Value)
(length(rad_pgh_0.5$Measure.Value)/35574)*100

rad_pgh_2.0 <- rad_pgh_ds %>% 
  filter(Measure.Value > 0.5 & Measure.Value <= 2.0)
length(rad_pgh_2.0$Measure.Value)
(length(rad_pgh_2.0$Measure.Value)/35574)*100

rad_pgh_4.0 <- rad_pgh_ds %>% 
  filter(Measure.Value > 2.0 & Measure.Value <= 4.0)
length(rad_pgh_4.0$Measure.Value)
(length(rad_pgh_4.0$Measure.Value)/35574)*100

rad_pgh_8.0 <- rad_pgh_ds %>% 
  filter(Measure.Value > 4.0 & Measure.Value <= 8.0)
length(rad_pgh_8.0$Measure.Value)
(length(rad_pgh_8.0$Measure.Value)/35574)*100

rad_pgh_20.0 <- rad_pgh_ds %>% 
  filter(Measure.Value > 8.0 & Measure.Value <= 20.0)
length(rad_pgh_20.0$Measure.Value)
(length(rad_pgh_20.0$Measure.Value)/35574)*100

rad_pgh_50.0 <- rad_pgh_ds %>% 
  filter(Measure.Value > 20 & Measure.Value <= 50.0)
length(rad_pgh_50.0$Measure.Value)
(length(rad_pgh_50.0$Measure.Value)/35574)*100

rad_pgh_100.0 <- rad_pgh_ds %>% 
  filter(Measure.Value > 50.0 & Measure.Value <= 100.0)
length(rad_pgh_100.0$Measure.Value)
(length(rad_pgh_100.0$Measure.Value)/35574)*100

rad_pgh_100.0_over <- rad_pgh_ds %>% 
  filter(Measure.Value > 100.0)
length(rad_pgh_100.0_over$Measure.Value)
(length(rad_pgh_100.0_over$Measure.Value)/35574)*100

length(rad_pgh_ds$Measure.Value)

rad_count <- rad_pgh_ds %>% 
  filter(Measure.Value >= 20)

length(rad_count$Measure.Value)
percent <- (length(rad_count$Measure.Value))/(length(rad_pgh_ds$Measure.Value))*100
percent

length(rad_pgh_ds$Measure.Value)

## histogram 
ggplot(rad_pgh_ds, aes(x = Measure.Value)) +
  geom_histogram(fill = 'red') +
  scale_x_log10()+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pittsburgh Metro') +
  theme_bw()

## adding limits = c(0.5, 150)
ggplot(rad_pgh_norm, aes(x = Measure.Value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(Measure.Value, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pittsburgh Metro') +
  theme_bw()

## adding annotate 
hist_pgh <- ggplot(rad_pgh_ds, aes(x = Measure.Value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(Measure.Value, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Great Pittsburgh') +
  theme_bw() +
  annotate("text", x = c(20), y = c(6000), size = 5, label=c("n = 35,557")) +
  annotate("text", x = c(30), y = c(5700), size = 5, label=c("median = 2.9pCi/L")) +
  annotate("text", x = c(30), y = c(5400), size = 5, label=c("mean = 5.4pCi/L ")) +
  annotate("text", x = c(17), y = c(5100), size = 5, label=c("sd = 8.7")) +
  annotate("text", x = c(30), y = c(4900), size = 4, label=c("*removed 17 data w/3.3 z-score"))
  
## adding stats and annotate
hist_pgh <- ggplot(rad_pgh_ds, aes(x = Measure.Value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(data=pgh_stats, aes(xintercept=mean)) +
  geom_vline(data =pgh_stats, aes(xintercept=median), linetype="dashed") +
  geom_vline(data = pgh_stats, aes(xintercept=mean+sd), linetype = "dashed", color = "blue")+
  geom_vline(data = pgh_stats, aes(xintercept=mean+sd*2), linetype = "dotted", color = "blue") +
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', y = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh') +
  annotate("text", x=c(2.9), y=c(6400), label=c("median=2.9"))+
  annotate("text", x=c(5.0), y=c(6000), label=c("mean=5.3"))+
  annotate("text", x=c(12.0), y=c(5500), label=c("mean+sd=13.4"))+
  annotate("text", x=c(25.0), y=c(5000), label=c("mean+2sd=21.4"))+
  annotate("text", x=c(75), y=c(4000), label=c("n=35527"))+
  annotate("text", x=c(75), y=c(3700),label=c("sd=8.05") )+
  annotate("text", x=c(75), y=c(3400), label=c("(removed 47 outliers)"), size = 3)+
  theme_bw() +
  theme(legend.position = "none")

hist_pgh
getwd()

ggsave("radon_pitt_with_stat.png")
summary(rad_pgh_norm$Measure.Value)
sd(rad_pgh_norm$Measure.Value)
rad_count <- rad_pgh_norm %>% 
  filter(Measure.Value >= 20)
length(rad_count$Measure.Value)
percent <- (length(rad_count$Measure.Value))/(length(rad_pgh_norm$Measure.Value))*100
percent

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/radon_pgh.png", 
       height = 4, width = 6)

#skewness(rad_pgh_ds$Measure.Value)
ggplot(rad_pgh_norm, aes(x = Measure.Value)) +
  geom_histogram(bins = 15, fill = 'red') +
  scale_x_log10()+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pittsburgh Metro')

mean(rad_pgh_norm$Measure.Value)
ggplot(rad_pgh_norm, aes(x = Measure.Value, fill = "red"))+
  geom_density()+
  scale_x_continuous(limits = c(1.0, 50))

rad_county <- rad_pgh_ds %>% 
  filter(County.Name == "Washington") %>% 
  select("Test.Start.Date", "Address.Postal.Code","County.Name",
         "Measure.Value") 
length(rad_county$Measure.Value)
colnames(rad_pgh_ds)

rad_zip <- rad_pgh_ds %>% 
  filter(Address.Postal.Code == 15205) %>% 
  select("Test.Start.Date", "Measure.Value", "z_score" )
length(rad_zip$Measure.Value)
summary(rad_zip$Measure.Value)
rad_zip %>% 
  arrange(-Measure.Value)

summary_rad_zip <- rad_zip %>% 
  summarise(minimum = min(Measure.Value, round(2)),
            median = median(Measure.Value, round(2)),
            mean = mean(Measure.Value),
            maximum = max(Measure.Value, round(2)),
            sd = sd(Measure.Value, round(2)),
            n = length(Measure.Value))

rad_zip %>% 
  ggplot(aes(x = Measure.Value)) +
  geom_histogram(bins = 16, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(Measure.Value, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon') +
  theme_bw()

rad_zip %>% 
  ggplot(aes(x = Measure.Value)) +
  geom_boxplot(alpha=0.5, outlier.color = "red", 
               outlier.shape = 1) +
  coord_flip()

rad_zip %>% 
  ggplot(aes(Measure.Value))+
  scale_x_log10()+
  geom_histogram(bins = 10)+
  xlim(0,20)
  
rad_zip %>% 
  ggplot(aes(Measure.Value))+
  scale_x_log10()+
  geom_histogram(bins = 10)+
  xlim(0,20)

rad_zip %>% 
  ggplot(aes(x=Measure.Value, color = "red", fill = "red"))+
  geom_density(alpha=0.5)+
  scale_x_continuous(limits = c(1.0, 20))

qqnorm(rad_zip$Measure.Value)
qqline(rad_zip$Measure.Value)

##### exluding data > 1000 #####
rad_pgh_1000 <- rad_pgh_ds %>% 
  filter(Measure.Value < 1000)
rad_pgh_1000
summary(rad_pgh_1000$Measure.Value)
sd(rad_pgh_1000$Measure.Value)
length(rad_pgh_1000$Measure.Value)

rad_pgh_0.5 <- rad_pgh_1000 %>% 
  filter(Measure.Value <= 0.5)
length(rad_pgh_0.5$Measure.Value)
(length(rad_pgh_0.5$Measure.Value)/35572)*100

rad_pgh_2.0 <- rad_pgh_1000 %>% 
  filter(Measure.Value > 0.5 & Measure.Value <= 2.0)
length(rad_pgh_2.0$Measure.Value)
(length(rad_pgh_2.0$Measure.Value)/35572)*100

rad_pgh_4.0 <- rad_pgh_1000 %>% 
  filter(Measure.Value > 2.0 & Measure.Value <= 4.0)
length(rad_pgh_4.0$Measure.Value)
(length(rad_pgh_4.0$Measure.Value)/35572)*100

rad_pgh_8.0 <- rad_pgh_1000 %>% 
  filter(Measure.Value > 4.0 & Measure.Value <= 8.0)
length(rad_pgh_8.0$Measure.Value)
(length(rad_pgh_8.0$Measure.Value)/35572)*100

rad_pgh_20.0 <- rad_pgh_1000 %>% 
  filter(Measure.Value > 8.0 & Measure.Value <= 20.0)
length(rad_pgh_20.0$Measure.Value)
(length(rad_pgh_20.0$Measure.Value)/35572)*100

rad_pgh_50.0 <- rad_pgh_1000 %>% 
  filter(Measure.Value > 20 & Measure.Value <= 50.0)
length(rad_pgh_50.0$Measure.Value)
(length(rad_pgh_50.0$Measure.Value)/35572)*100

rad_pgh_100.0 <- rad_pgh_1000 %>% 
  filter(Measure.Value > 50.0 & Measure.Value <= 100.0)
length(rad_pgh_100.0$Measure.Value)
(length(rad_pgh_100.0$Measure.Value)/35572)*100

rad_pgh_100.0_over <- rad_pgh_1000 %>% 
  filter(Measure.Value > 100.0)
length(rad_pgh_100.0_over$Measure.Value)
(length(rad_pgh_100.0_over$Measure.Value)/35572)*100

length(rad_pgh_ds$Measure.Value)

rad_count <- rad_pgh_norm %>% 
  filter(Measure.Value >= 20)

length(rad_count$Measure.Value)
percent <- (length(rad_count$Measure.Value))/(length(rad_pgh_ds$Measure.Value))*100
percent

length(rad_pgh_1000$Measure.Value)

ggplot(rad_pgh_1000, aes(x = Measure.Value)) +
  geom_histogram(bins = 10, fill = 'red') +
  scale_x_log10()+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pittsburgh Metro') +
  theme_bw()

ggplot(rad_pgh_norm, aes(x = Measure.Value)) +
  geom_histogram(bins = 12, fill = 'red', alpha = 0.8) +
  scale_x_log10(limits = c(0.1, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pittsburgh Metro') +
  theme_bw()

#skewness(rad_pgh_ds$Measure.Value)
ggplot(rad_pgh_norm, aes(x = Measure.Value)) +
  geom_histogram(bins = 12, fill = 'red') +
  scale_x_log10()+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pittsburgh Metro')

mean(rad_pgh_norm$Measure.Value)
ggplot(rad_pgh_1000, aes(x = Measure.Value, fill = "red"))+
  geom_density()+
  scale_x_continuous(limits = c(1.0, 50))

rad_county <- rad_pgh_ds %>% 
  filter(County.Name == "Washington") %>% 
  select("Test.Start.Date", "Address.Postal.Code","County.Name",
         "Measure.Value") 
length(rad_county$Measure.Value)
colnames(rad_pgh_ds)

rad_zip <- rad_pgh_1000 %>% 
  filter(Address.Postal.Code == 15205) %>% 
  select("Test.Start.Date", "Measure.Value", "z_score" )
length(rad_zip$Measure.Value)
summary(rad_zip$Measure.Value)
rad_zip %>% 
  arrange(-Measure.Value)

rad_zip %>% 
  ggplot(aes(x = Measure.Value)) +
  geom_boxplot(alpha=0.5, outlier.color = "red", 
               outlier.shape = 1) +
  coord_flip()

rad_zip %>% 
  ggplot(aes(Measure.Value))+
  scale_x_log10()+
  geom_histogram(bins = 10)+
  xlim(0,20)

rad_zip %>% 
  ggplot(aes(x=Measure.Value, color = "red", fill = "red"))+
  geom_density(alpha=0.5)+
  scale_x_continuous(limits = c(1.0, 20)) +
  theme(legend.position = "none") 
  

qqnorm(rad_zip$Measure.Value)
qqline(rad_zip$Measure.Value)

ggarrange(hist_pa, hist_pgh)



