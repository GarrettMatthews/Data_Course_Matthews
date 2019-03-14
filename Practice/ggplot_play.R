#Loading libraries
library(tidyverse)
library(modelr)
library(wesanderson)
library(colorspace)

#Loading data set, and renaming for conveniance
data("mtcars")
df = mtcars
# Editing data for clarity (usually changing to a factor)
df$am = factor(df$am)
#Initial plotting
p1 = df %>%
  ggplot(aes(x=hp, y= mpg, col = am)) +
  geom_point(size = 12, alpha = .5) + labs(title = "MPG vs HP", x = "Horsepower",
                                           y = expression(paste(italic("Miles")~  "per"~  italic("Gallon"))), color = "Auto/Man")
p1
#Custom pallete
pal = c("#0e4700","#af20bc")
# Google "color picker" to easily find color character codes
p1 = p1 + #scale_color_discrete(labels = c("Auto","Man")) + 
  scale_color_manual(labels = c("Auto","Man"), values = pal)
p2 = p1 + theme(title = element_text(face = "italic", hjust = 0.5), 
           panel.background = element_rect(fill= "Orange"), 
           panel.grid = element_line("Red"),
           legend.background = element_rect(fill = "Orange"),
           legend.key = element_rect(fill = "Blue", color = "Purple"))
p1

# theme_set() -- allows you to make your own theme to use in that script

p2 + theme(axis.text = element_text(face = "italic")) 

#####
# More graphing

p3 = df %>%
  ggplot(aes(x=am, y = mpg)) 
p3 + geom_boxplot()
p3 + geom_violin() + geom_point(alpha = .5)


sal = read.csv("Desktop/Git_Repositories/Data_Course/Exams/Exam_2/salaries.csv")

sal = sal %>%
  gather(key = "Rank", value = "Salary", 5:7)

sal %>%
  ggplot(aes(x=Rank, y= Salary, fill = Tier, color = Tier)) + geom_violin() + geom_point(alpha = .35, position = "jitter")

#github.com/clauswilke/colorblindr

# get devtools, ggtern