library(tidyverse)

df = read.csv("Exercises/ugly_data1.csv")

long = gather(df,Group,Value,2:9)

grep("^m",long$Group) -> malerows
grep("^f",long$Group) -> femalerows

long$Gender[malerows] <- "Male"
long$Gender[femalerows] <- "Female"


long$Group = str_remove(long$Group, "^m")
long$Group = str_remove(long$Group, "^f")


ages =  str_replace(long$Group,"014","0014")
ages1 =substr(ages,start = 1,stop= 2 )
ages2 = substr(ages,start = 3,stop = 4)
paste0(ages1,"-",ages2) -> long$Group
names(long)[names(long) == "Group"] <- "AgeRange"
names(long)


write.csv(long,"../Data_Course_Matthews/Practice/ugly_data_1_clean.csv",row.names= FALSE, quote = FALSE)
