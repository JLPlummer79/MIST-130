#After some discussion with our TA, I have decided on a
# a Pearson R correlation, to show that the temp_avg and
# snowfall are correlated.
# I feel that this test will show that these two thing are
#correlated inversely, that is as average temperature decreases
# snow_fall increases

#Code for Pearsons correlation
# Put temp_ave data in a variable var1 <- temp_avg
# Put snow_fall data in a variable  var2 <- snow_fall
# data_cor_1 <- cor(df0$temp_avg, df0$snow_fall, method = c("pearson"))
#The result was -0.2005137, a number between 0 & -1
#and was expected to be negatively correlated.

#The number being close to zero demonstrates that the two
#variables are not strongly correlated, and that I should
#look to another two (or four) variables for correlation

#This data does not meet the assumptions of the tester;
#and thus should be re-evaluated with other variables.

#This means, in the real world, that it may not always mean
#with a lower temperature that there is greater snowfall.
# The tester will examine other variables for correlation.

#Plot of the data, shows a relatively normal distribution
#plot(df0$temp_avg, df0$snow_fall)

#Analysis of linear model with different variables;
#library(lme4)
#library(tidyverse)
#library(lmerTest)
#include a map graphic
df0 %>%
  na.omit() %>%
  group_by(station) %>%
  filter(snow_fall > 5) %>%
  filter(latitude > 40) %>%
  ggplot(aes(x = snow_fall, y = latitude, group = snow_fall)) + geom_point()

df0 %>%
  na.omit() %>%
  mutate(lat_Abv_40 = ifelse(latitude > 40, "abv", "bel")) -> data_for_model

#data_cor_2 <- cor(data_for_model$latitude, data_for_model$snow_fall,
#                  method = c("pearson"))

high_lat_model <- lmer(snow_fall ~ lat_Abv_40 + (1|station),
                       data= data_for_model)

summary(high_lat_model)

#based on the p value we can reject the null hypothesis and conclude that stations
#above the 40th parallel (latitudes above 40 degrees) have more snowfall that
#stations below the 40th parallel

data_for_model %>%
  filter(snow_fall != 0) -> data_only_snow_fall

#with zero snow
data_for_model %>%
  ggplot(aes(x = snow_fall)) + geom_histogram() + facet_wrap(~lat_Abv_40)

#without zero snow
data_only_snow_fall %>%
  ggplot(aes(x = snow_fall)) + geom_histogram() + facet_wrap(~lat_Abv_40)


#I predicted that there would be a greater amount of snowfall at stations above
# the 40th parallel than stations below the 40th parallel, because generally
#speaking temperatures tend to be lower at higher latitudes

data_cor_0 <- cor(data_for_model$latitude, data_for_model$temp_avg,
                  method = c("pearson"))
#the Pearson correlation has an r value of -0.5 showing that there is a strong
#negative correlation
#between average temperature and latitude; the higher the latitude the lower
#the average temperture is

data_for_model %>%
  ggplot(aes(x = temp_avg, y = latitude)) + geom_point() +
  geom_smooth(method="lm")

#The distribution for snowfall is skewed


