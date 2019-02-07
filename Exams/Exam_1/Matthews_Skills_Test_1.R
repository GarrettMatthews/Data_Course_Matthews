df = read.delim("DNA_Conc_by_Extraction_Date.csv")

library(tidyverse)

names(df)
head(df)
hist(df$DNA_Concentration_Ben, main = "DNA concentration, Ben", xlab = "DNA Concentration")
hist(df$DNA_Concentration_Katy, main = "DNA concentration, Katy", xlab = "DNA Concentration")

as.POSIXct(df$Date_Collected, format = "%Y-%m-%d") -> df$Date_Collected
factor(df$Year_Collected) -> df$Year_Collected


jpeg("Matthews_Plot1.jpeg")
plot(x=df$Year_Collected,y=df$DNA_Concentration_Katy, main = "Katy's Extractions", xlab = "Year", ylab = "DNA Concentration")
dev.off()
jpeg("Matthews_Plot2.jpeg")
plot(x=df$Year_Collected,y=df$DNA_Concentration_Ben, main = "Ben's Extractions", xlab = "Year", ylab= "DNA Concentration")
dev.off()

summary(df$DNA_Concentration_Ben)
summary(df$DNA_Concentration_Katy)

mb = c()
mk = c()
x = 1
y = 1

for(i in(levels(df$Year_Collected))){
  mb[x] = print(mean(df[df$Year_Collected == i, "DNA_Concentration_Ben"]))
  x = x+1
}
for(i in(levels(df$Year_Collected))){
  mk[y] = print(mean(df[df$Year_Collected == i, "DNA_Concentration_Katy"]))
  y = y+1
}

m = data.frame(Year = levels(df$Year_Collected), Ben.Mean = mb, Katy.Mean = mk )

print( m$Ben.Mean / m$Katy.Mean) -> dat
df2 = data.frame(Year = levels(df$Year_Collected), Compare = dat)

# To use the min function, I needed to convert the year column to a interger, however, this changed the
# years values from being a year, to being 1-12.
as.integer(df2$Year) -> df2$Year
min(df2[df2$Compare, "Year"])
# The min function told me what row had the smallest value of the compared means of Ben and Katy
# I reset the data frame to redisplay the correct years, and then look at which year had the 
# corresponding smallest value
df2 = data.frame(Year = levels(df$Year_Collected), Compare = dat)
df2[2,"Year"]

