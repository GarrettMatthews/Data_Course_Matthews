#libraries
library(tidyverse)
library(modelr)

#Starting task 1

df = read.csv("./salaries.csv")

glimpse(df)
# Condensing salary and job title into two single columns
df = df %>%
  gather(5:7,key = "Title", value = "Salary")

#Making the boxplot:  Create boxplot of salary by University Tier, colored by Faculty Rank

p1 = df %>%
  ggplot(aes(x=Tier, y=Salary, fill = Title)) +
  geom_boxplot() + labs(title = "Faculty Salaries - 1995", x = "Rank")

#Exporting the box plot

jpeg(filename = "Matthews_Exam2_Plot1.jpeg")
p1
dev.off()


#Starting task 2

atm = read.csv("atmosphere.csv")

glimpse(atm)

#Goal: Create two lm's with Diversity as dependent variable

#Precipitation seems to be a good indicator
atm %>%
  ggplot(aes(x=Precip, y= Diversity)) + geom_point() + geom_smooth(method =  'lm')
#CO2 seems to be good also
atm %>%
  ggplot(aes(x=CO2_Concentration, y= Diversity)) + geom_point() + geom_smooth(method =  'lm')
#Month not so much
atm %>%
  ggplot(aes(x=Month, y= Diversity)) + geom_point() + geom_smooth(method =  'lm')
# Aerosol Density seems to be good
atm %>%
  ggplot(aes(x=Aerosol_Density, y= Diversity)) + geom_point() + geom_smooth(method =  'lm')
# Not so much for quarter either
atm %>%
  ggplot(aes(x=Quarter, y= Diversity)) + geom_point() + geom_smooth(method =  'lm')
# There seems to be an increase in diversity over time as well
atm %>%
  ggplot(aes(x=Year, y= Diversity)) + geom_point() + geom_smooth(method =  'lm')

#Staring to make the models
mod1 = lm(Diversity ~ Precip * CO2_Concentration, data = atm)
summary(mod1)
mod2 = lm (Diversity ~ Precip * CO2_Concentration * Aerosol_Density + Year, data = atm)
summary(mod2)

#Finding residuals
atm = add_residuals(atm,mod1, var = "Mod1_R")
atm = add_residuals(atm, mod2, var = "Mod2_R")

mean(atm$Mod1_R)
mean(atm$Mod2_R)
# According to the mean of the residuals mod1 has better explanatory power, because the 
# residuals are smaller by a factor of 3

# Finding predictions
atm = add_predictions(atm, mod1, var = "Mod1")
atm = add_predictions(atm, mod2, var = "Mod2")

#Making the plot with the models

atm %>%
  ggplot(aes(x= Year)) + geom_point(aes(y=Diversity),size = 2) +
  geom_smooth(aes(y=Mod1), color = "Blue", method =  'lm') +
  geom_smooth(aes(y=Mod2), color = "Red", method = "lm") +
  geom_smooth(aes(y=Diversity), color = "Black", method = 'lm')

# I used year for x, because I felt like that might have the least influence despite being in
# mod2, because just using row.names() wouldn't generate a trend line, all 3 trend lines
# are stacked

atm %>%
  ggplot(aes(x= Year)) + geom_point(aes(y=Diversity),size = 2) +
  geom_point(aes(y=Mod1), color = "Blue", alpha = .75) +
  geom_point(aes(y=Mod2), color = "Red", alpha = .75 ) +
  geom_smooth(aes(y=Diversity), color = "Black", method = 'lm', se = FALSE)

#Using hypothetical data
hyp = read.csv("hyp_data.csv")

m1p =predict(mod1, newdata = hyp)
m2p =predict(mod2, newdata = hyp)

# Summaries
sum = summary(mod1)
sum2 = summary(mod2)
#Used sinnk because summary doesn't output a dataframe, and write.table wanted a data frame
sink(file = "model_summaries.txt")
print(sum)
print(sum2)
sink()

# Bonus

hyp1 = hyp
hyp1$Diversity = m1p
hyp2 = hyp
hyp2$Diversity = m2p
# I was unable to join the hypothetical data frames with the regular because
# quarter was a factor in one and an integer in the other, same with month
levels(atm$Quarter)
m = c("January","February","March","April","May","June","July","August","September",
      "October","November","December")
atm$Quarter = plyr::mapvalues(atm$Quarter, from= c("Q1","Q2","Q3","Q4"), to = c(1,2,3,4))
atm$Quarter = as.integer(atm$Quarter)
atm$Month = plyr::mapvalues(atm$Month, from = m, to = c(1,2,3,4,5,6,7,8,9,10,11,12))
atm$Month = as.integer(atm$Month)

atm1 = full_join(atm,hyp1)
