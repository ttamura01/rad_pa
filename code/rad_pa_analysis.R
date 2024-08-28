library(tidyverse)
library(dplyr)
library(ggtext)
setwd("/Users/takayukitamura/Documents/R_Computing/rad_pa")
# to create rad_pa and rad_pgh from radon open-data: https://data.pa.gov/Energy-and-the-Environment/Radon-Test-Results-September-1986-Current-Annual-C/vkjb-sx3k/about_data


rad_pa <- read_csv("/Users/takayukitamura/Desktop/Radon_Test_Results.csv") %>% 
  select(`Test End Date`, `County Name`, `Measure Value` ) %>%
  na.omit(`Measure Value`) %>% 
  filter(`Measure Value` >=0)

# to create stats 

pa_stats <- rad_pa %>% 
  summarise(minimum = min(`Measure Value`, round(2)),
            median = median(`Measure Value`, round(2)),
            mean = mean(`Measure Value`),
            maximum = max(`Measure Value`, round(2)),
            sd = sd(`Measure Value`, round(2)),
            n = length(`Measure Value`))

pa_stats

## understand the distribution better with percentage of each segment

# PA

rad_pa_2.0 <- rad_pa %>% 
  filter(`Measure Value` < 2.0)
length(rad_pa_2.0$`Measure Value`)
(length(rad_pa_2.0$`Measure Value`)/3077181)*100

rad_pa_4.0 <- rad_pa %>% 
  filter(`Measure Value` >= 2.0 & `Measure Value` < 4.0)
length(rad_pa_4.0$`Measure Value`)
(length(rad_pa_4.0$`Measure Value`)/3077181)*100

rad_pa_10.0 <- rad_pa %>% 
  filter(`Measure Value` >= 4.0 & `Measure Value` < 10.0)
length(rad_pa_10.0$`Measure Value`)
(length(rad_pa_10.0$`Measure Value`)/3077181)*100

rad_pa_20.0 <- rad_pa %>% 
  filter(`Measure Value` >= 10.0 & `Measure Value` < 20.0)
length(rad_pa_20.0$`Measure Value`)
(length(rad_pa_20.0$`Measure Value`)/3077181)*100

rad_pa_50.0 <- rad_pa %>% 
  filter(`Measure Value` >= 20 & `Measure Value` < 50.0)
length(rad_pa_50.0$`Measure Value`)
(length(rad_pa_50.0$`Measure Value`)/3077181)*100

rad_pa_50.0 <- rad_pa %>% 
  filter(`Measure Value` >= 50.0)
length(rad_pa_50.0$`Measure Value`)
(length(rad_pa_50.0$`Measure Value`)/3077181)*100

## histogram 

# PA

ggplot(rad_pa, aes(x = `Measure Value`)) +
  geom_histogram(fill = 'red') +
  scale_x_log10()+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Greater Pennsylvania') +
  theme_bw()

## adding limits = c(0.5, 150)

# PA

ggplot(rad_pa, aes(x = `Measure Value`)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(`Measure Value`, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pennsylvania') +
  theme_bw()

## adding annotate 

# PA

ggplot(rad_pa, aes(x = `Measure Value`)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(`Measure Value`, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Pennsylvania State',
       caption = "source: open data PA") +
  theme_bw() +
  annotate("text", x = c(20), y = c(300000), size = 5, label=c("n = 3077181")) +
  annotate("text", x = c(30), y = c(275000), size = 5, label=c("median = 2.5pCi/L")) +
  annotate("text", x = c(30), y = c(250000), size = 5, label=c("mean = 6.08pCi/L ")) +
  annotate("text", x = c(17), y = c(225000), size = 5, label=c("sd = 22.6")) 

## adding stats and annotate

# PA

ggplot(rad_pa, aes(x = `Measure Value`)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.5) +
  geom_vline(data=pa_stats, aes(xintercept=mean)) +
  geom_vline(data =pa_stats, aes(xintercept=median), linetype="dashed") +
  geom_vline(data = pa_stats, aes(xintercept=mean+sd), linetype = "dashed", color = "blue")+
  geom_vline(data = pa_stats, aes(xintercept=mean+sd*2), linetype = "dotted", color = "blue") +
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', y = 'Counts', 
       title = 'Measured Radon in Pennsylvania',
       caption = "source: Open Data PA") +
  annotate("text", x=c(5), y=c(475000), label=c("median=2.5pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(9.0), y=c(450000), label=c("mean=6.08pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(20.0), y=c(400000), label=c("mean+sd=30.08pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(25.0), y=c(375000), label=c("mean+2sd=51.2pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(20), y=c(350000), label=c("(n=3077181)"), size = 5, face = "bold")+
  annotate("text", x=c(11), y=c(425000),label=c("sd=22.6pCi/l"), size = 5, face = "bold")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown()) +
  theme_bw() +
  theme(legend.position = "none")
