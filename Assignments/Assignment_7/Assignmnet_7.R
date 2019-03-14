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

mod1 = lm(GrowthRate ~ Light, data = df)
summary(mod1)
# The p-value for GrowthRate affected by Light is quite small, indicating there
# is a strong relationship

mod2 = aov(GrowthRate ~ Nitrogen, data = df)
summary(mod2)


nls(formula= ((a*x^2)+(b*x)+c), data = df, start=c(a=-4,x=df$Nitrogen,b=20,c=))
glm(GrowthRate ~ Nitrogen, data = df, family = "binomial")

df$GrowthRate = as.integer(df$GrowthRate)
mod3 =predict(loess(GrowthRate~Nitrogen, data = df))

glimpse(df)

ggplot(df, aes(x=Nitrogen,y=GrowthRate)) +
  geom_point() + geom_smooth(se =FALSE) +
  geom_point(aes(y=mod3),color= "Red",size=4)

mod4 = loess(GrowthRate~Nitrogen, data = df)

nd = data.frame(Nitrogen=15)
predict(mod4,nd)


        