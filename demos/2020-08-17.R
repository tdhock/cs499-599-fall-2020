## If you get "ggplot2 package not found"
install.packages("ggplot2")

## Use double colon prefix for packages to make it clear where each
## object comes from.
ggplot2::ggplot()

## Attach all objects from the ggplot2 package. Useful when there is a
## package with a lot of functions that you want to use (in that case
## the double colon prefix clutters the code and prevents easy
## understanding).
library(ggplot2)

## Now we don't need the double colon prefix to access objects in the
## ggplot2 package.
ggplot()

## scatterplot of mpg data. data set and aes specified globally at the
## ggplot level, so are the default for each geom.
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap("class")

## alternative syntax with aes/data specified for the geom. useful is
## you want different data sets / aes for each geom.
ggplot() +
  geom_point(aes(
    x = displ, y = hwy),
    data=mpg)

## set a constant visual property as an argument of the geom, OUTSIDE
## of aes.
ggplot() +
  geom_point(aes(
    x = displ, y = class),
    color="blue",
    data=mpg)

## example of stroke aes, must be used with shape=21
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 1)
