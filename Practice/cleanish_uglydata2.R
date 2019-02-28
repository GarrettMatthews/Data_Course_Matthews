library(tidyverse)

df = read.csv("Exercises/ugly_data2.csv")

df1 = df[1:9,]
df2 = df[20:28,]
f014 = df2[2:10, 2]
f1524 = df2[2:10, 3]
f2534 = df2[2:10, 4]
f3544 = df2[2:10, 5]
df2 = data.frame(f014 = f014,f1524 = f1524,f2534=f2534,f3544=f3544)

df1$Gender = "Male"
df2$Gender = "Female"
df = full_join(df1,df2,by= "Gender")
df = gather(df,AgeGroup,Count,2:5)
df$AgeGroup = str_remove(df$AgeGroup, "^m")
df$AgeGroup = str_remove(df$AgeGroup, "^f")


ages =  str_replace(df$AgeGroup,"014","0014")
ages1 =substr(ages,start = 1,stop= 2 )
ages2 = substr(ages,start = 3,stop = 4)
paste0(ages1,"-",ages2) -> df$AgeGroup

df
