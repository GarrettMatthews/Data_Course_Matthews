df = read.csv("ITS_mapping.csv", sep = "\t")
library(tidyverse)


glimpse(df)
levels(df$Collection_Date)
# I saw that there were levels in the date column that were not entered in like the rest of the dates
# in order to rectify this I used droplevels() and copied and pasted the offending levels
droplevels(df, exclude = c("40602" , "40604","40605","41250","41289","41305","41390","41841",
            "41842","41843","41844","41845","41846","41947","41949",
            "42005","42006","42007","42008","42009","42010","42011",
            "42012" ,"42013","42014","42015","42016","42017","42018",     
            "42019","42020","42021","42022","42023","42024","42025"  ,   
            "42026","42027","42028","42029","42030","42031","42032",
            "42033","42034","42035","42036","42037","42038","42039" ,    
            "42040","42041","42042","42043","42044","42045","42046"  ,   
            "42047","42048","42049","42050","42051","42052","42053",     
            "42054","42055","42056","42057","42058","42059","42060",     
            "42061","42062","42063","42064","42065","42066","42067",     
            "42068","42069","42070","42071","42072","42073","42074",     
            "42075","42076","42077","42078","42079","42080","42081",     
            "42082","42083","42084","42085","42087","42088","42165",     
            "42166","42170","42171","42172","42173","42174","42175",     
            "42206","42244","42255","42269","42275","42340")) -> df3
factor(df3$Collection_Date) -> df3$Collection_Date
levels(df3$Collection_Date)
as.POSIXct(df3$Collection_Date, format = "%d.%m.%Y") -> df3$Collection_Date

summary(df3)



levels(df$SampleID)
levels(df$Ecosystem)
as.data.frame(table(df$SampleID,df$Ecosystem)) -> df2
ggplot(df2, aes(x=Var2,y=Freq)) +
  geom_violin()

as.data.frame(table(df$Ecosystem,df$Host)) -> df4


ggplot(df4, aes(x=Var1,y=Freq)) +
  geom_point(aes(col=Var2)) +
  theme(legend.position = "none")

ggplot(df, aes(x=Ecosystem,y=Lat)) +
  geom_boxplot()

png(filename = "silly_boxplot.png")
ggplot(df, aes(x=Ecosystem,y=Lat)) +
  geom_boxplot()
dev.off()