
#-----------------------------------------------------------------------------
#- Datacamp Cleanig data in R
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#- inspect data
#-----------------------------------------------------------------------------

data <- mtcars

head(data, n = 8)

tail(data, n =  2)

class(data$mpg)

library(dplyr)

data <- mutate_each(data, funs(as.numeric), mpg:disp)

print(data)

str(data)

dim(data)

colnames(data)

dplyr::glimpse(data)

summary(data)

hist(data$mpg)

plot(data$mpg, data$drat)


#-----------------------------------------------------------------------------
#- tidyr
#-----------------------------------------------------------------------------

library(tidyr)

data <- mtcars %>% select(cyl, drat)

data_wide <- cbind( data, tidyr::spread(data, cyl, drat) )

data_wide <- tidyr::spread(data, cyl, drat) # spread needs dataframe, key, value

data <- tidyr::gather(data_wide, cyl, drat, c(1:3), na.rm = TRUE) # gather needs dataframe, key, value, columns to be gathered

data <- mtcars

data <- tidyr::unite(data, col = mpg_qsec, mpg, qsec, sep = "-")

data <- tidyr::separate(data, col = mpg_qsec, into = c("mpg", "qsec"), sep = "-")


#-----------------------------------------------------------------------------
#- lubridate
#-----------------------------------------------------------------------------

library(lubridate)

ymd("2017-04-01")

ymd("2017 April 1st")

mdy("April 1, 2017")

hms("11:05:45")

ymd_hms("2017/04/01 11.05.45")


#-----------------------------------------------------------------------------
# stringr
#-----------------------------------------------------------------------------

library(stringr)

str_trim(c("   Filip ", "Nick  ", " Jonathan"))

str_pad(c("23485W", "8823453Q", "994Z"), width = 9, side = "left", pad = "0")

toupper(c("aa", "bb"))

tolower(c("AA", "BB"))

str_detect(as.character(mtcars$cyl), "6")

str_replace(as.character(mtcars$cyl), "6", "Six")


#-----------------------------------------------------------------------------
# missing data
#-----------------------------------------------------------------------------

data <- mtcars

data[6, 4] <- NA

is.na(data)
any(is.na(data))
sum(is.na(data))

summary(data)

data[ complete.cases(data) , ]
na.omit(data)


#-----------------------------------------------------------------------------
# outliers
#-----------------------------------------------------------------------------

boxplot(data$hp, horizontal = TRUE)

