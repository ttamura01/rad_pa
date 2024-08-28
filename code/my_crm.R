library(ggplot2)
library(tidyr)
library(tidyverse)
library(plyr)
library(lubridate)

setwd("/Users/takayukitamura/Documents/R_Computing/rad_pa")
getwd()
read.table("my_crm.csv")
my_data <- read.table("my_crm.csv", sep = ",", header = TRUE)
head(my_data)
colnames(my_data)[colnames(my_data) == "MeasuredRadon"] <- "radon"
my_data2 <- data.frame(Date = 8/7/2023,
                       radon = 88.8)
sapply(my_data2,class)

class(my_sata2$radon)
my_data2
cbind.data.frame()
my_data2 <- colnames(c("Date", "radon"))
colnames(my_data2)
my_data %>% 
  summary(radon)
my_data %>% 
  ggplot(aes(x = radon)) +
  geom_boxplot()

my_data %>% 
  ggplot(aes(radon)) +
  geom_histogram(bins = 10)

my_data <- my_data %>% 
  rename("MeasuredRadon" = "radon")
sw <- as_tibble(sw)
rename(sw, weight = mass)
sw <- starwars %>% 
  select(name, height, mass, birth_year) %>% 
  rename(weight = mass)
rename(sw, weight = "mass")
my_data %>% 
  ggplot(aes(x = ))
starwars
dim(starwars)  
sw <- starwars %>% 
  select(name, mass, height) 
colnames(sw)
sw %>% 
  rename(tall=height)
# rename in R
data("airquality")
head(airquality)
colnames(airquality)
## rename one column name ##
colnames(airquality)[colnames(airquality) == "Day"] <- "Day_of_the_week"
## rename all column names ##
colnames(airquality) <- c("var1", "var2", "var3", "var4", "var5", "var6")
colnames(airquality) 
## rename some column names ##
colnames(airquality)[colnames(airquality) %in% 
                       c("var1", "var3")] <- c("sun", "storm")
colnames(airquality)[colnames(airquality) %in% 
                       c("sun", "storm")] <- c("sun", "storm")
colnames(airquality)

x <- 20221005
class(x)
x_date1 <- as.Date(as.character(x),
                   format = "%Y%m%d")
x_date1
x_data2 <- strptime(x, format = "%Y%m%d")
x_data2
install.packages("lubridate")
library(lubridate)
x_data3 <- ymd(x)
x_data3
