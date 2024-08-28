setwd("/Users/takayukitamura/Documents/R_Computing/rad_pa")

library(tidyverse)
library(dplyr)
library(ggtext)
library(glue)

# to create rad_pa and rad_pgh from radon open-data: https://data.pa.gov/Energy-and-the-Environment/Radon-Test-Results-September-1986-Current-Annual-C/vkjb-sx3k/about_data

# rad_pgh <- read_csv("/Users/takayukitamura/Desktop/Radon_Test_Results.csv") %>% 
#   select(`Test End Date`, `County Name`, `measure.value` ) %>% 
#   filter(`County Name` == "Allegheny"| `County Name` == "Lawrence" |`County Name` ==  "Butler"|
#            `County Name` == "Beaver"| `County Name` ==  "Westmoreland" |`County Name` ==  "Washington"|
#            `County Name` == "Armstrong"
#   ) %>% 
#   na.omit(`measure.value`)

rad_pgh <- read.csv("data/rad_pgh.csv") %>% 
  rename_all(tolower)

# write_csv(rad_pgh, "rad_pgh.csv")

# to create stats 

pgh_stats <- rad_pgh %>% 
  summarise(minimum = min(measure.value, round(2)),
            median = median(measure.value, round(2)),
            mean = mean(measure.value),
            maximum = max(measure.value, round(2)),
            sd = sd(measure.value, round(2)),
            n = length(measure.value))

pgh_stats

## understand the distribution better with percentage of each segment
# PGH

rad_pgh_2.0 <- rad_pgh %>% 
  filter(measure.value < 2.0)
length(rad_pgh_2.0$measure.value)
(length(rad_pgh_2.0$measure.value)/520244)*100

rad_pgh_4.0 <- rad_pgh %>% 
  filter(measure.value >= 2.0 & measure.value < 4.0)
length(rad_pgh_4.0$`measure.value`)
(length(rad_pgh_4.0$measure.value)/520244)*100

rad_pgh_10.0 <- rad_pgh %>% 
  filter(measure.value >= 4.0 & measure.value < 10.0)
length(rad_pgh_10.0$measure.value)
(length(rad_pgh_10.0$measure.value)/520244)*100

rad_pgh_20.0 <- rad_pgh %>% 
  filter(measure.value >= 10.0 & measure.value < 20.0)
length(rad_pgh_20.0$measure.value)
(length(rad_pgh_20.0$measure.value)/520244)*100

rad_pgh_50.0 <- rad_pgh %>% 
  filter(measure.value >= 20 & `measure.value` < 50.0)
length(rad_pgh_50.0$measure.value)
(length(rad_pgh_50.0$measure.value)/520244)*100

rad_pgh_100.0 <- rad_pgh %>% 
  filter(measure.value >= 100.0)
length(rad_pgh_100.0$measure.value)
(length(rad_pgh_100.0$measure.value)/520244)*100

rad_pgh %>% arrange(-measure.value)


## histogram 

# PGH

ggplot(rad_pgh, aes(x = measure.value)) +
  geom_histogram(fill = 'red') +
  scale_x_log10()+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh') +
  theme_bw()

## adding limits = c(0.5, 150)

# PGH

ggplot(rad_pgh, aes(x = measure.value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(`measure.value`, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh') +
  theme_bw()

## adding annotate 
# PGH

ggplot(rad_pgh, aes(x = measure.value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(measure.value, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Great Pittsburgh',
       caption = "source: open data PA") +
  theme_bw() +
  annotate("text", x = c(20), y = c(90000), size = 5, label=c("n = 520244")) +
  annotate("text", x = c(30), y = c(80000), size = 5, label=c("median = 2.8pCi/L")) +
  annotate("text", x = c(30), y = c(70000), size = 5, label=c("mean = 5.5pCi/L ")) +
  annotate("text", x = c(17), y = c(60000), size = 5, label=c("sd = 18.5"))

## adding stats and annotate
# PGH

ggplot(rad_pgh, aes(x = measure.value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.5) +
  geom_vline(data=pgh_stats, aes(xintercept=mean)) +
  geom_vline(data =pgh_stats, aes(xintercept=median), linetype="dashed") +
  geom_vline(data = pgh_stats, aes(xintercept=mean+sd), linetype = "dashed", color = "blue")+
  geom_vline(data = pgh_stats, aes(xintercept=mean+sd*2), linetype = "dotted", color = "blue") +
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', y = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh',
       caption = "source: Open Data PA") +
  annotate("text", x=c(5), y=c(92000), label=c("median=2.8pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(9.0), y=c(86000), label=c("mean=5.5pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(17.0), y=c(75000), label=c("mean+sd=24.0pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(25.0), y=c(70000), label=c("mean+2sd=42.5pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(20), y=c(65000), label=c("(n=520,244)"), size = 5, face = "bold")+
  annotate("text", x=c(11), y=c(80000),label=c("sd=18.5pCi/l"), size = 5, face = "bold")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown()) 

ggsave("radon_pgh.png", height = 5, width = 5)

##Excluding outliers from the universe

a <- rad_pgh %>% 
  filter(!measure.value > 100)

pgh_stats_2 <- a %>% 
  summarise(minimum = min(measure.value, round(2)),
            median = median(measure.value, round(2)),
            mean = mean(measure.value),
            maximum = max(measure.value, round(2)),
            sd = sd(measure.value, round(2)),
            n = length(measure.value))

round(pgh_stats_2[5],1)
(round(pgh_stats_2[5],1)*2 + round(pgh_stats_2[3],1))
N <- format(pgh_stats_2[6], big.mark=",")

ggplot(a, aes(x = measure.value)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.5) +
  geom_vline(data=pgh_stats_2, aes(xintercept=mean)) +
  geom_vline(data =pgh_stats_2, aes(xintercept=median), linetype="dashed") +
  geom_vline(data = pgh_stats_2, aes(xintercept=mean+sd), linetype = "dashed", color = "blue")+
  geom_vline(data = pgh_stats_2, aes(xintercept=mean+sd*2), linetype = "dotted", color = "blue") +
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', y = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh',
       subtitle = "Extluded outliers (100pCi/L or higher = 5SD from the universe)",
       caption = "source: Open Data PA") +
  annotate("text", x=c(5), y=c(92000), label=c(glue("median={round(pgh_stats_2[2],1)}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(9.0), y=c(86000), label=c(glue("mean={round(pgh_stats_2[3],1)}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(10.0), y=c(80000), label=c(glue("sd={round(pgh_stats_2[5],1)}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(25.0), y=c(70000), label=c(glue("mean+2sd={(round(pgh_stats_2[5],1)*2 + round(pgh_stats_2[3],1))}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(20), y=c(65000), label=c(glue("(n={N})")), size = 5, face = "bold")+
  annotate("text", x=c(21), y=c(75000),label=c(glue("mean+sd={(round(pgh_stats_2[5],1) + round(pgh_stats_2[3],1))}pCi/l")), size = 5, face = "bold")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown()) 

ggsave("radon_pgh_norm.png", height = 5, width = 5)
