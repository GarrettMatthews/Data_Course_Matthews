library(tidyverse)

data("mtcars")
str(mtcars)
?mtcars
df = mtcars

auto = df %>%
  filter(am < 1)

write.csv(auto,"./automatic_mtcars.csv")
png("mpg_vs_hp_auto.png")
auto %>%
  ggplot(aes(x=hp,y=mpg)) +
  geom_point() + xlab("Horsepower") + ylab("Miles Per Gallon") + 
  labs(title = "MPG affected by Horse Power in automatic cars") 
dev.off()

tiff("mpg_vs_wt_auto.tiff")
auto %>%
  ggplot(aes(x=wt, y=mpg)) +
  geom_point() + labs(title = "MPG affected by weight in automatic cars") + xlab("Weight") +
  ylab("Miles Per Gallon")
dev.off()

df2 = df %>%
  filter(disp <= 200)
write.csv(df2,"./mtcars_max200_displ.csv")

max_mtcars = max(df$hp)
max_auto = max(auto$hp)
max_disp = max(df2$hp)
max_mtcars = df %>%
  filter(hp == max_mtcars)
max_auto = df %>%
  filter(hp == max_auto)
max_disp = df %>%
  filter(hp == max_disp)
df3 = rbind(max_mtcars,max_auto,max_disp)
write.table(df3,"./hp_maximums.txt")

