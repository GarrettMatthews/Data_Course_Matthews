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
# I graphed comparative values to gain an initial idea of what variables might be 
#worth exploring

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

#Making models based off of trends I saw in the graphs above

mod1 = lm(Diversity ~ Precip * CO2_Concentration, data = atm)
summary(mod1)
# I used + Year rather than * Year because I felt like the interaction with year
# would be less important despite there being a graphical trend of year affecting
# diversity

mod2 = lm (Diversity ~ Precip * CO2_Concentration * Aerosol_Density + Year, data = atm)
summary(mod2)

#Finding residuals for the models
atm = add_residuals(atm,mod1, var = "Mod1_R")
atm = add_residuals(atm, mod2, var = "Mod2_R")
#I took the mean to compare the two model's residuals
mean(abs(atm$Mod1_R))
mean(abs(atm$Mod2_R))
# According to the mean of the residuals mod2 has a better explaining power than mod1
# because it has a lower mean than mod1

# Finding predictions
atm = add_predictions(atm, mod1, var = "Mod1")
atm = add_predictions(atm, mod2, var = "Mod2")

#Making the plot with the models
# Here I only plotted trend lines with the points, which I realized after was less useful
# and not the goal for this portion of the test
atm %>%
  ggplot(aes(x= Year)) + geom_point(aes(y=Diversity),size = 2) +
  geom_smooth(aes(y= atm$Mod1), color = "Blue", method =  'lm', se = FALSE, alpha = .5) +
  geom_smooth(aes(y= atm$Mod2), color = "Red", method = "lm", se = FALSE, alpha = .5) +
  geom_smooth(aes(y=Diversity), color = "Black", method = 'lm', se = FALSE, alpha = .5)

# I used year for x, because I felt like that might have the least influence despite being in
# mod2, because just using row.names() wouldn't generate a trend line, all 3 trend lines
# are stacked

# Plotting with points is much more useful because I can see how close the predictions
# where to actual values recorded
atm %>%
  ggplot(aes(x= Year)) + geom_point(aes(y=Diversity),size = 2) +
  geom_point(aes(y=Mod1), color = "Blue", alpha = .75) +
  geom_point(aes(y=Mod2), color = "Red", alpha = .75 ) +
  geom_smooth(aes(y=Diversity), color = "Black", method = 'lm', se = FALSE) + 
  annotate("text", x= 2002, y= 6000, label = "Black dots are based on data, 
           blue on model 1, red on model 2")
  

#Using hypothetical data
hyp = read.csv("hyp_data.csv")

m1p =predict(mod1, newdata = hyp)
m2p =predict(mod2, newdata = hyp)

# Summaries
sum = summary(mod1)
sum2 = summary(mod2)

#Used sink because summary doesn't output a dataframe, and write.table wanted a data frame
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
# so I used plyr::mapvalues to change the atmospheric data in order to join these
# data frames
levels(atm$Quarter)
m = c("January","February","March","April","May","June","July","August","September",
      "October","November","December")
atm$Quarter = plyr::mapvalues(atm$Quarter, from= c("Q1","Q2","Q3","Q4"), to = c(1,2,3,4))
atm$Quarter = as.integer(atm$Quarter)
atm$Month = plyr::mapvalues(atm$Month, from = m, to = c(1,2,3,4,5,6,7,8,9,10,11,12))
atm$Month = as.integer(atm$Month)

atm1 = full_join(atm,hyp1)
atm1$Year = factor(atm1$Year)
# Hypothetical data is 2019, this graph is based off of mod1

atm1 %>%
  ggplot(aes(x = Precip, y= Diversity, col = Year)) + geom_point() + facet_wrap(~Year) +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_color_discrete(labels = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010,
                                  2011, 2012, "Predicted"))

# Making the graph with hypothetical data from 2019 based off of mod2

atm2 = full_join(atm,hyp2)
atm2$Year = factor(atm2$Year)

atm2 %>%
  ggplot(aes(x= Precip, y = Diversity, col = Year)) + geom_point() + facet_wrap(~Year) +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_color_discrete(labels = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010,
                                  2011, 2012, "Predicted"))


# Examining the graphs from mod1 and mod2 indicate that mod1 was a better predictor, as
# discussed earlier, as the values seem to fall along the group values much better