df = read.csv("./Data/1620_scores.csv")


library(tidyverse)
library(modelr)
library(GGally)

exam.sum = df$Exam.1..4245260. + df$Exam.2..4245261.
exam.mean = exam.sum / 2
df$Exam.Mean = exam.mean
names(df)

df %>%
  ggplot(aes(x=Actual.PreTest.Score...not.what.your.grade.is...4122452., y= Exam.Mean)) +
  geom_point() + geom_smooth(method = "lm")


df %>%
  ggplot(aes(x=YOUR.CHOICE....Midterm..4122464.,y=Exam.Mean)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

quiz.sum = (df$X1..Quiz.1..4122444. + df$X2..Quiz.2..4122445. + df$X3..Quiz.3..4122443.+df$X4..Quiz.4..4122448.+
               df$X5..Quiz.5..4122449.+df$X6..Quiz.6..4122450.+df$X7..Quiz.7..4122447.)
quiz.mean = quiz.sum / 7
df$Quiz.Mean = quiz.mean

df %>%
  ggplot(aes(x=Quiz.Mean, y= Exam.Mean, col = YOUR.CHOICE....Midterm..4122464.)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

mod1 = lm(Exam.Mean ~ Quiz.Mean * YOUR.CHOICE....Midterm..4122464., data = df)
summary(mod1)

mod2 = lm(Exam.Mean ~ Quiz.Mean * YOUR.CHOICE....Midterm..4122464. + Actual.PreTest.Score...not.what.your.grade.is...4122452., data = df)
summary(mod2)

anova(mod1,mod2)


mod3 = lm(Exam.Mean ~ Quiz.Mean + YOUR.CHOICE....Midterm..4122464. + Actual.PreTest.Score...not.what.your.grade.is...4122452., data = df)
summary(mod3)

df = add_predictions(df,mod3, var = "mod3")
df = add_predictions(df,mod2, var = "mod2")
df = add_predictions(df,mod1, var = "mod1")


df %>%
  ggplot(aes(x=row.names(df))) + geom_point(aes(y=Exam.Mean), color = "Black", size = 2) +
  geom_point(aes(y=mod1), color = "Red" ) +
  geom_point(aes(y=mod2), color = "Blue") + 
  geom_point(aes(y=mod3), color = "Green") 

df %>%
  ggplot(aes(x=Quiz.Mean,y=Exam.Mean)) +
  geom_point() +
  geom_smooth(aes(y=mod1), color = "Red", method = 'lm', alpha = .25) +
  geom_smooth(aes(y=mod2), color = "Blue", method = 'lm', alpha = .25) +
  geom_smooth(aes(y=mod3), color = "Green", method = 'lm', alpha = .25)


sum(residuals(mod1)^2)
sum(residuals(mod2)^2)
sum(residuals(mod3)^2)


df2 =data.frame(Actual.PreTest.Score...not.what.your.grade.is...4122452. = c(50,30,40), 
                YOUR.CHOICE....Midterm..4122464. = c(80,50,50),
                Quiz.Mean = c(10,3,8))

predict(mod3, newdata = df2)


### Cleaning the data


df = read.csv("./Data/Bird_Measurements.csv")

names(df)


masscols = c(5,7,9)
impt.cols = c(1:4)



bird.mass = df[,c(impt.cols,masscols)]

mass.long = bird.mass %>%
  gather(key = Sex, value = Mass,5:7)

unique(mass.long$Sex)
mass.long$Sex = str_remove(mass.long$Sex, "_mass")
