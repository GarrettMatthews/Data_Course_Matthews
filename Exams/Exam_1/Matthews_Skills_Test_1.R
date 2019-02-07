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

summary(m)
