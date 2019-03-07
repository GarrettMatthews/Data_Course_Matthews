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
