# Skills Test 3

# Below are two data sets. Using these data, re-create the two plots in the exam_3 directory.
# Use ggplot
# export (using code) these two plots into your exam_3 directory. 
# They should be saved as .png files at 300 dpi with a width of 5 inches and height of 5 inches
# Naming convention should be LASTNAME_plot1.png and LASTNAME_plot2.png
# Push your R script and the two image files to GitHub.

# Load packages
library(tidyverse)
library(gapminder)

# creating a color palette for plot 1
pal = c("#6b5456","#ec8d1b","#6abf2a","#8b53b7","#70acbe","#01c95b","#c00014","#31332f","#f7d000","#abba00")

# generate data for plot 1
plot1.data = gapminder


# Make and export plot 1
p1 = plot1.data %>%
  ggplot(aes(x=year, y= lifeExp, color = continent)) + geom_point(alpha = .25) +
  geom_smooth(se = FALSE) + scale_color_manual(values = pal) + 
  labs(title = "Life Expectancy Over Time", subtitle = "Colored by Continent",
       y = "Life Expectancy", x = "Year", color = "Continent")
p1

p2 = p1 + theme(panel.background = element_rect(fill = "White"),
                panel.grid = element_line("#eaecef"),
                axis.ticks = element_line(colour = "White"),
                legend.background = element_rect(fill = "White"),
                legend.key = element_rect(fill = "White"))

ggsave("./Matthews_plot1.png", plot = p2, width = 5, height = 5, units = "in", dpi = 300)

# Generate data for plot 2 (run the following 5 lines only once to ensure we are all using identical data)
set.seed(123)
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
plot2.data <- rbind(a,b,c)

# make and export plot2
p3 = plot2.data %>%
  ggplot(aes(x = x, y = y)) + geom_bin2d()

p4 = p3 + theme(panel.background = element_rect(fill = "White"),
                axis.title = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                plot.margin = unit(c(0,0,0,0), "in"), 
                legend.box.margin = unit(c(0,-.07,0,-.15), "in"))
p4
ggsave("./Matthews_plot2.png", plot = p4, width = 5, height = 5, units = "in", dpi = 300)



