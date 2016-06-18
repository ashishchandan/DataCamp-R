

#-----------------------------------------------------------------------------
# libraries
#-----------------------------------------------------------------------------

library("ggplot2")

rm(list = ls())


#-----------------------------------------------------------------------------
# chapter 1 - Introduction
#-----------------------------------------------------------------------------

#install.packages("MASS")

library("MASS")

data <- MASS::mammals


ggplot(data = data, aes(x = body, y = brain)) + geom_point(alpha = 0.6) +
    coord_fixed() +
    scale_x_log10() +
    scale_y_log10() +
    stat_smooth(method = "lm", col = "#C42126", se = F)


ggplot(mtcars, aes(x = wt, y = mpg, size = disp, col = disp)) + geom_point()


levels(iris$Species) <- c("Setosa", "Versicolor", "Virginica") #correct labels

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
    geom_jitter(alpha = 0.6) +
    facet_grid(. ~ Species) +
    stat_smooth(method = "lm", se = F, col = "red") +
    scale_y_continuous("Sepal Width (cm)", limits = c(2,5), expand = c(0,0)) +
    scale_x_continuous("Sepal Length (cm)", limits = c(4,8), expand = c(0,0)) +
    coord_equal() #equal x and y



dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

dia_plot + geom_point(aes(col = clarity)) #aes() can be called within geom_points()


ggplot(diamonds, aes(x = carat, y = price)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", se = F, aes(col = clarity)) #aes() called within geom_smooth, model for each type clarity




#-----------------------------------------------------------------------------
# chapter 2 - Data
#-----------------------------------------------------------------------------

library(tidyr) #gather and spread function
library(dplyr)

iris.wide <- iris %>% ?

iris.tidy <- iris %>%
    gather(key, Value, -Species) %>% #- in front of categorical var
    separate(key, c("Part", "Measure"), "\\.") #seperate eg Sepal.Width into two vars

str(iris)
str(iris.tidy)

ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
    geom_jitter() +
    facet_grid(. ~ Measure)

ggplot(iris.wide, aes(x = Length, y = Width, col = Part)) +
    geom_jitter() +
    facet_grid(. ~ Species)



iris$Flower <- 1:nrow(iris)

iris.wide <- iris %>%
    gather(key, value, -Species, -Flower) %>%
    separate(key, c("Part", "Measure"), "\\.") %>%
    spread(Measure, value)


#-----------------------------------------------------------------------------
# chapter 3 - Aesthetics
#-----------------------------------------------------------------------------

mtcars$cyl <- as.factor(mtcars$cyl)

ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point(shape = 13, size = 5)
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + geom_point(shape = 16, size = 6, alpha = 0.6)
ggplot(mtcars, aes(x = wt, y = mpg, size = cyl)) + geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, alpha = cyl)) + geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, shape = cyl)) + geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, label = cyl)) + geom_text()


ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point(col = "#123456")
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + geom_point(size = 10, shape = 23, col = "#123456")
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + geom_point(alpha = 0.5)
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + geom_point(shape = 24, col = 'yellow')
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + geom_text(label = "X", color = 'red', size = 10)


ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl))) + geom_point()
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am))) + geom_point()
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am), size = hp/wt)) + geom_point()
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am), size = hp/wt)) + geom_text(aes(label = rownames(mtcars)))





#-----------------------------------------------------------------------------
# chapter 4 - Geometries
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
# chapter 5 - qplot and wrap-up
#-----------------------------------------------------------------------------



