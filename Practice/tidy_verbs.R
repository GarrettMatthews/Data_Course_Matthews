library(tidyverse)
# The tidy verbs
data(iris)
df = iris


#Average sepal length for each species of iris

df2 = df %>%
  group_by(Species) %>%
  summarise(N = n(), mSL= mean(Sepal.Length), sdSL = sd(Sepal.Length), min_SL = min(Sepal.Length), max_SL = max(Sepal.Length)) %>%
  as.data.frame()

class(df2)

df %>%
  select(starts_with("S"))
df %>%
  select(one_of(c("Petal.Length","Sepal.Roundness")))






?mutate() # adds new variables that are functions of existing variables
?select() # picks variables based on their names.
?filter() # picks cases based on their values.
?summarise() # reduces multiple values down to a single summary.
?arrange() # changes the ordering of the rows.
?group_by() # allows you to perform any operation “by group”.

# %>%  pipe!


