# Attaching packages
library(tidyverse)
library(modelr)

# Opening Data

df = read.csv("./mushroom_growth.csv")

# Exploring Data

glimpse(df)
# It seems like there is a bit of an influence on growth rate, but only having two temperatures
# make it hard to tell
df %>%
  ggplot(aes(x=Temperature,y=GrowthRate, col = Species)) +
  geom_point() + geom_smooth()
# Nitrogen seems have an affect on growth rate
df %>%
  ggplot(aes(x=(Nitrogen),y=GrowthRate)) +
  geom_point() + geom_smooth()
# Light might have an influence on growth rate
df %>%
  ggplot(aes(x=Light,y=GrowthRate)) +
  geom_point() + geom_smooth(method = "lm")
# There  might be an influence here, but it's hard to tell because geom_smooth doesn't work
# with factors; High and Low mean nothing in this circumstance
df %>%
  ggplot(aes(x=Humidity,y=GrowthRate)) +
  geom_point() + geom_smooth()

# Making the models
mod1 = lm(GrowthRate ~ Light*Nitrogen+Humidity, data = df)
summary(mod1)

mod2 = aov(GrowthRate ~ Light * Temperature, data = df)
summary(mod2)


####################################################################
## Practicing mods in class
#nls(formula= ((a*x^2)+(b*x)+c), data = df, start=c(a=-4,x=df$Nitrogen,b=20,c=))
#glm(GrowthRate ~ Nitrogen, data = df, family = "binomial")

#df$GrowthRate = as.integer(df$GrowthRate)
#mod3 =predict(loess(GrowthRate~Nitrogen, data = df))

#glimpse(df)

#ggplot(df, aes(x=Nitrogen,y=GrowthRate)) +
#  geom_point() + geom_smooth(se =FALSE) +
#  geom_point(aes(y=mod3),color= "Red",size=4)

#mod4 = loess(GrowthRate~Nitrogen, data = df)

#nd = data.frame(Nitrogen=15)
#predict(mod4,nd)
####################################################################

# Finding the  mean square error of each model
df = add_residuals(df, mod1,  var = "resd1")
df = add_residuals(df, mod2, var = "resd2")      
mean(df$resd1)
mean(df$resd2)
# Mod2 appears to be better, as the mean square error is an order of magnitude smaller

#Adding predictions

df = add_predictions(df,mod1,var = "pred1")
df = add_predictions(df,mod2,var = "pred2")

# Plotting the predictions

df %>%
  ggplot(aes(x= Light)) + geom_point(aes(y=GrowthRate), size = 1.5) +
  geom_point(aes(y=pred1),color= "Blue", alpha = .75, size = .75) +
  geom_point(aes(y=pred2),color = "Red", alpha = .75, size = .75) 




