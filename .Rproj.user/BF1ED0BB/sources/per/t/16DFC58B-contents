#load the tidyverse and the dataset
library(tidyverse)
library(ggfortify)
fricdat <- read_csv("./data_raw/fricatives.csv")

#get an idea of what the data look like
str(fricdat)
glimpse(fricdat)

#tidy the dataset; we want to collapse the _cog and _skewness columns into single columns,
#and create a segment column with [s] and [sh]
tidy_fric <- fricdat %>%
  gather(., meas, val, -obs) %>%
  separate(., meas, into = c("phoneme", "meas"), sep = "_", remove = T) %>%
  spread(., meas, val)

View(tidy_fric)  

#table of descriptive statistics
tidy_fric %>%
  group_by(., phoneme) %>%
  summarize(., mean_cog = mean(cog), sd_cog = sd(cog),
            mean_skew = mean(skewness), sd_skew = sd(skewness))

#boxplot of center of gravity as function of phoneme
tidy_fric %>%
  ggplot(aes(phoneme, cog))+
           geom_boxplot()

#plot skewness ~ phoneme using a statistical transformation
tidy_fric %>%
  ggplot(aes(phoneme, skewness))+
  stat_summary(fun.data = mean_cl_boot)

#fit a model that examines center of gravity as a function of skewness for [s]
#transform the data

tidy_s <- tidy_fric %>%
  filter(., phoneme == "s")

scsmod <- lm(cog ~ skewness, data = tidy_s)
summary(scsmod)

broom::tidy(scsmod)


#make a plot to show the relationship
ggplot(tidy_s, aes(skewness, cog))+
  geom_point()+
  geom_smooth(method=lm)

#check model diagnostics and have plots

#1. mean of residuals is zero
summary(scsmod$residuals)   #mean is zero, but 1st and 3rd quartile are very different

#2. homoskedasticity of residuals
autoplot(scsmod, which = c(1,3))

#3. No autocorrelation of residuals
acf(scsmod$residuals)

#4. Normality of residuals
autoplot(scsmod, which = 2)

#write up the results
