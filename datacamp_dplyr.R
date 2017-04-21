
#-------------------------------------------------------
#- Datacamp: Data manipulation in R with dplyr
#-------------------------------------------------------

#-------------------------------------------------------
#- load libraries
#-------------------------------------------------------

install.packages("dplyr")
install.packages("hflights")

library('dplyr')
library('hflights') #flights departed from Houston in 2011


#-------------------------------------------------------
#- load libraries
#-------------------------------------------------------

head(hflights)
summary(hflights)


#-------------------------------------------------------
#- introduction to dplyr and tbls
#-------------------------------------------------------

hflights <- tbl_df(hflights) #special df to view data in console

glimpse(hflights)


carriers <- hflights$UniqueCarrier

#create lut (look up table) with named characters
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

hflights$Carrier <- lut[hflights$UniqueCarrier]

glimpse(hflights)



lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")

hflights$Code <- lut[hflights$CancellationCode]

glimpse(hflights)



#-------------------------------------------------------
#- Select and mutate
#-------------------------------------------------------

select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)
select(hflights, Origin:Cancelled)
select(hflights, - (DepTime:AirTime))
select(hflights, ends_with("Delay"))
select(hflights, UniqueCarrier, ends_with("Num"), starts_with("Cancel"))
select(hflights, contains("Tim"), contains("Del"))


g1 <- mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime)
g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut)
g3 <- mutate(g2, AverageSpeed = Distance / AirTime * 60)

m1 <- mutate(hflights, loss = ArrDelay - DepDelay, loss_ratio = loss / DepDelay)
m2 <- mutate(hflights, TotalTaxi = TaxiIn + TaxiOut, 
                       ActualGroundTime = ActualElapsedTime - AirTime,
                       Diff = TotalTaxi - ActualGroundTime)


#-------------------------------------------------------
#- Filter and arrange
#-------------------------------------------------------

filter(hflights, Distance >= 3000)
filter(hflights, UniqueCarrier %in% c("JetBlue", "Southwest", "Delta"))
filter(hflights, TaxiIn + TaxiOut > AirTime)

filter(hflights, DepTime < 500 | ArrTime > 2200)
filter(hflights, DepDelay > 0, ArrDelay < 0)
filter(hflights, Cancelled == 1, DepDelay > 0)

c1 <- filter(hflights, Dest == "JFK")
c2 <- mutate(c1, Date = paste(Year, Month, DayofMonth, sep = "-"))
select(c2, Date, DepTime, ArrTime, TailNum)



dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))

arrange(dtc, DepDelay)
arrange(dtc, CancellationCode)
arrange(dtc, UniqueCarrier, DepDelay)

arrange(hflights, UniqueCarrier, desc(DepDelay))
arrange(hflights, DepDelay + ArrDelay)


#-------------------------------------------------------
#- Summarise and %>%
#-------------------------------------------------------

mtcars$mpg %>% hist()

mtcars %>% select(mpg) %>% summarise(min = min(mpg),
                                     max = max(mpg),
                                     sum = sum(mpg),
                                     mean = mean(mpg),
                                     median = median(mpg),
                                     quantile = quantile(mpg, 0),
                                     sd = sd(mpg),
                                     var = var(mpg),
                                     interQuartileRange = IQR(mpg),
                                     length = length(mpg),
                                     first = first(mpg),
                                     last = last(mpg),
                                     n = n(),
                                     nth = nth(mpg, 2),
                                     n_distinct = n_distinct(mpg))


summarise(hflights, min_dist = min(Distance), max_dist = max(Distance))
summarise(filter(hflights, Diverted == 1), max_div = max(Distance))


temp1 <- filter(hflights, !is.na(ArrDelay))
summarise(temp1, earliest = min(ArrDelay), 
                 average = mean(ArrDelay), 
                 latest = max(ArrDelay), 
                 sd = sd(ArrDelay))

temp2 <- filter(hflights, !is.na(TaxiIn), !is.na(TaxiOut))
summarise(temp2, max_taxi_diff = max(abs(TaxiIn - TaxiOut)))


summarise(hflights, 
          n_obs = n(), 
          n_carrier = n_distinct(UniqueCarrier), 
          n_dest = n_distinct(Dest))


aa <- filter(hflights, UniqueCarrier == "AA")
summarise(aa, 
          n_flights = n(), 
          n_canc = sum(Cancelled == 1),
          avg_delay = mean(ArrDelay, na.rm = TRUE))



c(1, 2, 3) %>% sum()
c(1, 2, 3, NA) %>% sum(na.rm = TRUE)

hflights$ArrDelay %>% hist(col = "steelblue", border = "white", xlim = c(-50, 400))

hflights %>%
    mutate(diff = TaxiOut - TaxiIn) %>%
    filter(!is.na(diff)) %>%
    summarise(avg = mean(diff))



hflights %>%
    mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
    filter(!is.na(mph), mph < 70) %>%
    summarise(n_less = n(), 
              n_dest = n_distinct(Dest), 
              min_dist = min(Distance), 
              max_dist = max(Distance))

hflights %>%
    mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
    filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
    summarise(n_non = n(), 
              n_dest = n_distinct(Dest), 
              min_dist = min(Distance), 
              max_dist = max(Distance))

hflights %>%
    filter(!is.na(DepTime), !is.na(ArrTime), DepTime > ArrTime) %>%
    summarise(num = n())


#-------------------------------------------------------
#- Group_by and 
#-------------------------------------------------------

hflights %>%
    group_by(UniqueCarrier) %>%
    summarise(p_canc = mean(Cancelled == 1) * 100, 
              avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%
    arrange(avg_delay, p_canc)

hflights %>%
    filter(!is.na(ArrDelay), ArrDelay > 0) %>%
    group_by(UniqueCarrier) %>%
    summarise(avg = mean(ArrDelay)) %>%
    mutate(rank = rank(avg)) %>%
    arrange(rank)

hflights %>%
    group_by(TailNum) %>%
    summarise(ndest = n_distinct(Dest)) %>%
    filter(ndest == 1) %>%
    summarise(nplanes = n())

hflights %>% 
    group_by(UniqueCarrier, Dest) %>%
    summarise(n = n()) %>%
    mutate(rank = rank(desc(n))) %>%
    filter(rank == 1)


#-------------------------------------------------------
#- Dplyr and databases (e.g. SQL)
#-------------------------------------------------------

#https://cran.rstudio.com/web/packages/dplyr/vignettes/databases.html

my_db <- src_mysql(dbname = "dplyr", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "student",
                   password = "datacamp")

nycflights <- tbl(my_db, "dplyr")

glimpse(nycflights)

nycflights %>%
    group_by(carrier) %>%
    summarise(n_flights = n(), avg_delay = mean(arr_delay)) %>%
    arrange(avg_delay)

