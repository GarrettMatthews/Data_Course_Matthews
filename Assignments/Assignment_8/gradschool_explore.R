# Importing libraries

library(tidyverse)
library(modelr)
library(GGally)
library(MASS)

## Goal: Explore the grad school admissions data, build models to figure out requirements
## for acceptance. Admitt code: 1=success 0=failure. Rank: 1 = top tier

df = read.csv("GradSchool_Admissions.csv")

# Exploring the data

glimpse(df)

df$admit = factor(df$admit)
df$rank = factor(df$rank)

ggpairs(df, aes(color = admit))


df %>%
  ggplot(aes(x=rank, y=gpa, fill = admit)) +
  geom_boxplot() 
df %>%
  ggplot(aes(x=rank, y=gre, fill = admit)) +
  geom_boxplot() 


df %>%
  ggplot(aes(x=gre, y= gpa, color = admit)) + geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  facet_wrap(~rank)
## Note: I find it interesting that in a tier 4 school, there was a student with a 4.0 and 
## a 800 gre that was not accepted into grad school, while in the other 3 tiers there was 
## a student with similar scores that was accepted.

# Building models

df$admit = plyr::mapvalues(df$admit, from = c(0,1), to = c(FALSE, TRUE))

mod1 = glm(admit ~ gre + gpa + rank, data = df)
summary(mod1)
aic.output = stepAIC(mod1)
summary(aic.output)

gre_n = rnorm(50, mean = 587.7, sd = 115.5165)
gpa_n = rnorm(50, mean = 3.3899, sd = 0.3805668)
rank_n = sample(rep(c(1,2,3,4),13),50)

test = data.frame(gre = gre_n, gpa = gpa_n, rank = rank_n)
test = add_predictions(test,mod1, var = "admit_odds")
exp(coefficients(mod1))


test %>%
  ggplot(aes(x = admit_odds, y= gre)) + geom_point() + geom_smooth(se=FALSE)
test %>%
  ggplot(aes(x = admit_odds, y= gpa)) + geom_point() + geom_smooth(se=FALSE)
test %>%
  ggplot(aes(x = admit_odds, y= rank)) + geom_point() + geom_smooth(se=FALSE)



#log odds



mod_rank = glm(admit ~ rank, data = df)
mod_gpa = glm(admit ~ gpa, data = df)
mod_gre = glm(admit ~ gre, data =df)

summary(mod_rank)
summary(mod_gpa)
summary(mod_gre)



df2 = add_predictions(df,mod_rank, var = "Rank_Pred")
df2 = add_predictions(df2,mod_gpa, var = "GPA_Pred")
df2 = add_predictions(df2,mod_gre, var = "GRE_Pred")

df2 %>%
  ggplot(aes(x= admit)) + geom_point(aes(y = gpa), color = 'Black') +
  geom_point(aes(y = GPA_Pred), color = "Red")

mod2 = glm(admit ~ rank * gre * gpa + rank + gre + gpa, data = df)
stepAIC(mod2)


mod3 = glm(gre ~ rank * gpa, data = df)
df2 = add_predictions(df, mod3)
df2 %>%
  ggplot(aes(x= admit)) + geom_point(aes(y = gre), color = "Black") + 
  geom_jitter(aes(y= pred), color = "Red", alpha = 0.25)

?truncnorm::rtruncnorm()
