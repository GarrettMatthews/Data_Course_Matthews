library(tidyverse)
library(modelr)
library(GGally)
library(glmulti)
library(MASS)

data(mtcars)
df = mtcars

df$am = factor(df$am)
df$vs = factor(df$vs)

glimpse(df)



        
ggplot(df,aes(x=cyl,y=mpg)) +
  geom_point() +
  geom_smooth(method = "lm")

mod1 = lm(mpg ~ cyl, data=df)
summary(mod1)

df2 = add_predictions(df,mod1)

ggplot(df2, aes(x=cyl)) +
  geom_jitter(aes(y=mpg)) +
  geom_jitter(aes(y=pred),color = "Red")

df2 = mutate(df2, DIFF = abs(pred-mpg))
mean(df2$DIFF)

ggplot(df,aes(x=cyl,y=mpg,color=factor(am))) +
  geom_point() + geom_smooth(method="lm")

mod2 = lm(mpg ~ cyl+factor(am), data =df)
summary(mod2)


df2 = add_predictions(df2, mod2, var = "pred2")
df2 = mutate(df2, DIFF2 = abs(pred2 - mpg))

ggplot(df2, aes(x = cyl)) +
  geom_point(aes(y=mpg, col = factor(am), alpha = .5) +
  geom_jitter(aes(y = pred), color = "Red") +
  geom_jitter(aes(y=pred2), color = "Blue")

mean(df2$DIFF)  
mean(df2$DIFF2)  


?mtcars

mod3 = lm(mpg ~ cyl+am+wt+hp+disp+carb, data = df)
df3 = add_predictions(df, mod3)
df3 = mutate(df3, DIFF = abs(pred - mpg))
mean(df3$DIFF)


df.mod = sample_n(df,nrow(df)/2)
df.sample = anti_join(df,df.mod)

mod.full.cross = glm( mpg ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data = df)
df3 = add_predictions(df.sample,mod.full.cross)

mean(abs(df3$pred-df3$mpg))


aic.output = stepAIC(mod.full.cross)


df %>%
  ggplot(aes(x=cyl, y= mpg, color = hp)) +
  geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~am)

mod.test = glm(mpg ~ cyl + hp + wt + am, data = df)
summary(mod.test)
