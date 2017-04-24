
#-----------------------------------------------------------------------------
#- DataCamp: ggplot2 (p1)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#- load libraries
#-----------------------------------------------------------------------------

library(ggplot2)

rm(list = ls())
cat("\014")


#-----------------------------------------------------------------------------
#- geom_point()
#-----------------------------------------------------------------------------

ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_point()

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, col = disp)) + geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) + geom_point()


ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_point(aes(col = clarity))

ggplot(diamonds, aes(x = carat, y = price, col = clarity)) + geom_smooth()

ggplot(diamonds, aes(x = carat, y = price, col = clarity)) + geom_point(alpha = 0.4)


ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_point(aes(col = clarity))


ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(se = FALSE) + 
  geom_smooth(aes(col = clarity), se = FALSE)


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_y_continuous("Sepal Width (cm)", limits = c(2,5), expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)", limits = c(4,8), expand = c(0,0)) +
  coord_equal()


ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linetype = 2)


ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)

ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 16, size = 6, alpha = 0.6)

ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(size = 10, shape = 23)

ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(size = 4, shape = 1)


#-----------------------------------------------------------------------------
#- geom_bar()
#-----------------------------------------------------------------------------

# fill absolute
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) + geom_bar()

# fill proportional
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) + geom_bar(position = "fill")  

# dodge
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Number") +
  scale_fill_manual("Transmission", 
                    values = c("#E41A1C", "#377EB8"),
                    labels = c("Manual", "Automatic")) 


# jittering - mitigate overplotting
ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +
  geom_point(size = 1, position = "jitter", alpha = 0.5, shape = 1)


# univariate histogram
ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth = 1)

# univariate histogram, density
ggplot(mtcars, aes(mpg)) + geom_histogram(aes(y=..density..), binwidth = 1)


ggplot(mtcars, aes(as.factor(cyl), fill = as.factor(am))) +
  geom_bar(position = "stack")

ggplot(mtcars, aes(as.factor(cyl), fill = as.factor(am))) +
  geom_bar(position = "fill") 

ggplot(mtcars, aes(as.factor(cyl), fill = as.factor(am))) +
  geom_bar(position = "dodge")

ggplot(mtcars, aes(as.factor(cyl), fill = as.factor(am))) + 
  geom_bar(position = position_dodge(0.4), alpha = 0.6)

ggplot(mtcars, aes(mpg, col = cyl)) + geom_freqpoly(binwidth = 1)


ggplot(mtcars, aes(mpg, fill = as.factor(cyl))) +
  geom_histogram(binwidth = 1, position = "dodge")

ggplot(mtcars, aes(mpg, fill = as.factor(cyl))) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4)


#-----------------------------------------------------------------------------
#- geom_line()
#-----------------------------------------------------------------------------

ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_line()

# plot rectangles
ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_line() +
  geom_rect(aes(xmin = 15, xmax = 20, ymin = -Inf, ymax = +Inf), 
            inherit.aes = FALSE, fill = "red", alpha = 0.01)

# plot smooth trends on top off
ggplot(mtcars, aes(x = mpg, y = disp, col = cyl)) +
  geom_line(alpha = 0.3) +
  geom_smooth(lwd = 2, se = FALSE)


#-----------------------------------------------------------------------------
#- geom_dotplot()
#-----------------------------------------------------------------------------

ggplot(mtcars, aes(cyl, wt, fill = factor(am))) +
  geom_dotplot(stackdir = "center", binaxis = "y")

