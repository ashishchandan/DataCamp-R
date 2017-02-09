

#-----------------------------------------------------------------------------
# libraries
#-----------------------------------------------------------------------------

library("dplyr")
library("ggvis")


#-----------------------------------------------------------------------------
# ..
#-----------------------------------------------------------------------------

mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()
mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()

mtcars %>% ggvis(~wt, ~mpg, fill := "red") %>% layer_points()
mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths()
mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>% layer_smooths()


pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars()
pressure %>% ggvis(~temperature, ~pressure) %>% layer_lines()
pressure %>% ggvis(~temperature, ~pressure, fill = ~temperature) %>% layer_points()
pressure %>% ggvis(~temperature, ~pressure, size = ~pressure) %>% layer_points()


faithful %>%
    ggvis(~waiting, ~eruptions, fill := "red") %>%
    layer_points() %>%
    add_axis("y", title = "Duration of eruption (m)",
             values = c(2, 3, 4, 5), subdivide = 4) %>%
    add_axis("x", title = "Time since previous eruption (m)")


# ~ = identify column or var names
# := = set properties, setting operator



iris %>% ggvis(~Sepal.Width, ~Sepal.Length, fill := "red", size = ~Petal.Width) %>% layer_points()

faithful %>% ggvis(~waiting, ~eruptions) %>% layer_points(size := 3) %>% layer_smooths()

pressure %>% ggvis(~temperature, ~pressure, size = ~pressure) %>% layer_points()
pressure %>% ggvis(~temperature, ~pressure, size := 100) %>% layer_points()
pressure %>% ggvis(~temperature, ~pressure, fill := "red") %>% layer_points()

#http://ggvis.rstudio.com/properties-scales.html


faithful %>%
    ggvis(~waiting, ~eruptions,
          size = ~eruptions, opacity := 0.5,
          fill := "blue", stroke := "black") %>%
    layer_points()


faithful %>%
    ggvis(~waiting, ~eruptions,
          fillOpacity = ~eruptions, size := 100,
          fill := "red", stroke := "red", shape := "cross") %>%
    layer_points()


pressure %>%
    ggvis(~temperature, ~pressure,
          stroke := "red", strokeWidth := 2, strokeDash := 6) %>%
    layer_lines()




#compute_smooths() to simplify model fits

mtcars %>% compute_smooth(mpg ~ wt)
mtcars %>% compute_model_prediction(mpg ~ wt) %>% ggvis(~pred_, ~resp_) %>% layer_lines()

mtcars %>% compute_smooth(mpg ~ wt) %>% ggvis(~pred_, ~resp_) %>% layer_lines()

mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths() %>% layer_points()



#histograms
faithful %>%
    compute_bin(~waiting, width = 5) %>%
    ggvis(x = ~xmin_, x2 = ~xmax_, y = 0, y2 = ~count_) %>%
    layer_rects()

faithful %>% ggvis(~waiting) %>% layer_histograms(width = 5)


#densities
faithful %>% ggvis(~waiting, fill := "green") %>% layer_densities()

#bars
mtcars %>% ggvis(~factor(cyl)) %>% layer_bars()



#use dplyr's group_by
mtcars %>% group_by(cyl) %>% ggvis(~mpg, ~wt, stroke = ~factor(cyl)) %>% layer_smooths()

mtcars %>% group_by(cyl) %>% ggvis(~mpg, fill = ~factor(cyl)) %>% layer_densities()

#group by 2 vars, include ~interaction
mtcars %>% group_by(cyl, am) %>% ggvis(~mpg, fill = ~interaction(cyl, am)) %>% layer_densities()




#-----------------------------------------------------------------------------
# interactive plots
#-----------------------------------------------------------------------------

faithful %>%
    ggvis(~waiting, ~eruptions, fillOpacity := 0.5,
          shape := input_select(label = "Choose shape:",
                                choices = c("circle", "square", "cross",
                                            "diamond", "triangle-up", "triangle-down")),
          fill := input_select(label = "Choose color:",
                               choices = c("black", "red", "blue", "green"))) %>% layer_points()

mtcars %>%
    ggvis(~mpg, ~wt,
          fill := input_radiobuttons(label = "Choose color:",
                                     choices = c("black", "red", "blue", "green"))) %>% layer_points()

mtcars %>%
    ggvis(~mpg, ~wt,
          fill := input_text(label = "Choose color:",
                             value = "black")) %>% layer_points()


#use map = as.name to create input vars
mtcars %>%
    ggvis(~mpg, ~wt,
          fill = input_select(label = "Choose fill variable:",
                              choices = names(mtcars), map = as.name)) %>% layer_points()


mtcars %>% ggvis(~mpg) %>% layer_histograms(width = input_numeric(label = "Choose a binwidth:", value = 1))

mtcars %>% ggvis(~mpg) %>% layer_histograms(width = input_slider(label = "Choose a binwidth:", min = 1, max = 20))





#-----------------------------------------------------------------------------
#- multi-layered plots
#-----------------------------------------------------------------------------

pressure %>%
    ggvis(~temperature, ~pressure, stroke := "skyblue") %>%
    layer_lines() %>%
    layer_points()


pressure %>%
    ggvis(~temperature, ~pressure) %>%
    layer_lines(stroke := "skyblue") %>%
    layer_points()


pressure %>%
    ggvis(~temperature, ~pressure) %>%
    layer_lines(stroke := "skyblue") %>%
    layer_points(shape := "triangle-up")


pressure %>%
    ggvis(~temperature, ~pressure, stroke := "skyblue",
          strokeOpacity := 0.5, strokeWidth := 5) %>%
    layer_lines() %>%
    layer_points(fill = ~temperature, shape := "triangle-up", size := 300)


pressure %>%
    ggvis(~temperature, ~pressure) %>%
    layer_lines(opacity := 0.5) %>%
    layer_points() %>%
    layer_model_predictions(model = "lm", stroke := "navy") %>%
    layer_smooths(stroke := "skyblue")


pressure %>%
    ggvis(~temperature, ~pressure, stroke := "circle") %>%
    layer_lines(stroke := "orange", strokeDash := 5, strokeWidth := 5) %>%
    layer_points(size := 100, fill := "lightgreen") %>%
    layer_smooths(stroke := "darkred")



#-----------------------------------------------------------------------------
#- Axes and Legends
#-----------------------------------------------------------------------------

faithful %>% 
    ggvis(~waiting, ~eruptions) %>% 
    layer_points() %>% 
    add_axis("x", 
             title = "Time since previous eruption (m)", 
             values = c(50, 60, 70, 80, 90), 
             subdivide = 9,
             orient = "top") %>%
    add_axis("y", 
             title = "Duration of eruption (m)", 
             values = c(2, 3, 4, 5), 
             subdivide = 9,
             orient = "right")


faithful %>% 
    ggvis(~waiting, ~eruptions, opacity := 0.6, 
          fill = ~factor(round(eruptions))) %>% 
    layer_points() %>% 
    add_legend("fill", title = "~ duration (m)", orient = "left")

