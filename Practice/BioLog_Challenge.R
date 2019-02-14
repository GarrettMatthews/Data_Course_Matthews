library(tidyverse)
df = read.csv("./Data/BioLog_Plate_Data.csv")

glimpse(df)
?plyr::mapvalues()
df = df %>%
  gather(key = "Hour",value ="Concentration", c("Hr_24","Hr_48","Hr_144"))
df$Hour = as.numeric(plyr::mapvalues(df$Hour, from = c("Hr_24","Hr_48","Hr_144"), to = c(24,48,144)))
# write a command the subsets the BioLog data to Clear_Creek samples, with dilution of 0.01, and only "Glycogen"
df2 = df %>%
  filter(Sample.ID == "Clear_Creek") %>%
  filter(Dilution == 0.01) %>%
  filter(Substrate == "Glycogen")

# Now plot those three replicates over time
df3 = df2 %>%
  gather(key = "Hour",value ="Concentration", 6:8)
factor(df3$Rep) -> df3$Rep
df3$Hour = as.numeric(plyr::mapvalues(df3$Hour, from = c("Hr_24","Hr_48","Hr_144"), to = c(24,48,144)))

df3 %>%
  ggplot(aes(x=Hour,y=Concentration,col=Rep))+
  geom_point() + geom_smooth()

# Make a plot of Tween 80 utilization over time for ALL the samples, colored by Sample.ID
levels(df$Substrate)
df4 = df %>%
  filter(Substrate == "Tween 80 ")
df4 %>%
  ggplot(aes(x= Hour, y = Concentration, col=Sample.ID)) +
  geom_point() + facet_wrap(~Sample.ID) + geom_smooth()
# Now, same plot, but combine both soils and both waters into soil and water groups and color by soil vs water
df5 = df4
df5$Sample.ID = plyr::mapvalues(df4$Sample.ID, from = c("Clear_Creek","Waste_Water","Soil_1","Soil_2"), to = c("Water","Water","Soil","Soil"))
df5 %>%
  ggplot(aes(x= Hour, y = Concentration, col=Sample.ID)) +
  geom_point() + facet_wrap(~Sample.ID) + geom_smooth()
# Make a table of summary statistics: for each combination of Sample.ID and Substrate, give:
# -- Number of observations
# -- Mean absorbance value

sum_table = df %>%
  group_by(Sample.ID, Substrate) %>%
  summarise( N = n(), Mean = mean(Concentration))

# Example output ....

# Sample.ID     Substrate                       N Mean
# <fct>         <fct>                       <int> <dbl>
# 1 Clear_Creek 2-Hydroxy Benzoic Acid         27 0.0562
# 2 Clear_Creek 4-Hydroxy Benzoic Acid         27 0.247 
# 3 Clear_Creek D-Cellobiose                   27 0.403 
# 4 Clear_Creek D-Galactonic Acid γ-Lactone    27 0.314 
# 5 Clear_Creek D-Galacturonic Acid            27 0.385 
# 6 Clear_Creek D-Glucosaminic Acid            27 0.154 
# 7 Clear_Creek D.L -α-Glycerol Phosphate      27 0.0335
# 8 Clear_Creek D-Mallic Acid                  27 0.170 
# 9 Clear_Creek D-Mannitol                     27 0.346 
# 10 Clear_Creek D-Xylose                       27 0.0323
# 

