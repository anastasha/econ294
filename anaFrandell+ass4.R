###############homework 4 ###########

# 0 - identify information
AnaFrandellAssignment2 <- list(
  fristName = "Ana",
  lastName = "Frandell",
  email = "anafrandell@hotmail.com",
  studentID = 212859817
)


#1 -Load data 
flights<- read.csv(file="E:/R-Lab/Assignments/flights.csv",stringsAsFactors=FALSE)
planes<- read.csv(file="E:/R-Lab/Assignments/planes.csv",stringsAsFactors=FALSE)
weather<- read.csv(file="E:/R-Lab/Assignments/weather.csv",stringsAsFactors=FALSE)
airports<- read.csv(file="E:/R-Lab/Assignments/airports.csv",stringsAsFactors=FALSE)
library(foreign)

#2 convert date column from type char to type date
as.Date(flights$date)
as.Date(planes$date)
as.Date(weather$date)
as.Date(airports$date)

#3 extract all flights that match :
install.packages("dplyr")
library(dplyr)
unique(flights$dest)
    #went to San Francisco or oakland, CA
flights.2a <- dplyr::filter(flights, dest=="SFO" | dest=="OAK")
print(nrow(flights.2a))
    #fligths delayed by an hour or more
flights.2b <- dplyr::filter(flights, arr_delay>60)
print(nrow(flights.2b))
    #arrival delay was more than twice as much as the departure delay
flights.2c<-dplyr::filter(flights, arr_delay>=2*dep_delay)
print(nrow(flights.2c))

#4 using select ()'s helper function, 3 different ways to select the delay variables from flights 

install.packages("plyr")
library(plyr)
  #select columns by name 
select(flights, dep_delay, arr_delay)
    #select columns between two variable (inclusive)
select(flights,dep_delay:arr_delay)
select(flights, contains("delay"))    
select(flights, matches("delay"))


#Question 5
#A
arrange(flights, -dep_delay)
head(flights,n=5)
#B
flights.5b<-flights
flights$delaydiff<-(flights.5b$dep_delay-flights.5b$arr_delay)
flights.5b<-arrange(flights.5b,-delaydiff)
head(flights.5b, n=5)



#5- arrange()
    #5a - top 5 most Departure delayed flights
arrange(flights, desc(dep_delay))
head(flights$dep_delay, 5)
    #5b- top 5 flights that caught up the most during the flight
caught.up<-(flights$dep_delay - flights$arr_delay)
arrange(flights, desc(caught.up))
print(caught.up[1:5])


#6 - mutate()

flights <- flights%>% 
  mutate(poop= time/60,
         speed=dist/poop,
         delta= dep_delay - arr_delay)
   
    #6a- top 5 flighs by speed
flight.6a<-arrange(flights, speed)
head(flights$speed, 5)
    #6b - top  flights that made up the most time in flight(should match 5b)
flight.6b<-arrange(flights, poop)
head(flights$poop, 5)
    #bc- top flights that lost the most time in flight
flight.6c<-arrange(flights, desc(poop))
head(flights$poop, 5)


#7 - group_by and summarize
flights.7a <- flights %>%
  dplyr::group_by(carrier) %>%
  dplyr::summarise(
    flight.cancled = sum(cancelled, na.rm= TRUE),
    flights.total= n(),
    flights.percent= 100*(sum(cancelled, na.rm= TRUE)/n()),
    delta.min = min(delta, na.rm = TRUE),
    delta.1stq = quantile(delta, 0.25, na.rm = TRUE),
    delta.mean = mean(delta, na.rm = TRUE),
    delta.median = median(delta, na.rm = TRUE),
    delta.3rdq = quantile(delta, 0.75, na.rm = TRUE),
    delta.90 = quantile(delta, 0.90, na.rm=TRUE),
    delta.max = max(delta, na.rm = TRUE)
  )

arrange(flights.7a, desc(flights.percent))
summary(flights.7a)


    #my code 
day_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date)%>%
    summarize(
      delay = mean(dep_delay),
      n = n()
      )
    #his code 
day_delay <- dplyr::filter(
  summarize(
    group_by(
      dplyr::filter(
        flights,
        !is.na(dep_delay)
      ),
      date
    ),
    delay = mean(dep_delay),
    n = n()
  ),
  n > 10
) 

cat( "dplyr provides the ability to use different types of data: data fram, table etc.
grou_by command groups/filters the data by the given varaible. In this case it groups the dataset flights by
values that don't have NA in dep_delay. Then it summarizes the date, mean of dep_delay for the number of observations above 10.")

#8 add new column to day-delay with differences between today and yesterdays average delay
    #print top 5 days that had the biggest increase in average dep-delay from one day to the next#
arrange(day_delay, date)
delay.8 <- day_delay %>% 
  mutate(today_delay = delay,
         yest_delay =  lag(delay,1),
         increase_delay = today_delay - yest_delay)

delay.8a<-arrange(delay.8, increase_delay)
head(delay.8$increase_delay,5)

#9 two table verbs, merging options 
    #create new table called dest_delay that summarizes the destination-level average arr_delay
    #and summarizes the number of flights that flew into it
            # group_by, summarize and n()

dest_delay.9 <- dplyr::filter(flights, !is.na(dep_delay))%>%group_by(dest)%>%
summarize(
  arr = mean(arr_delay),
  n = n()
)
                           
   
  airports<- airports%>%
  rename(dest = iata, name = airport)

dest_delay.9a<-left_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9aa<-arrange(dest_delay.9a, desc(arr))
head(dest_delay.9aa,5)
nrow(dest_delay.9aa)

    #9b - do number of observation via left_john match with inner_join
dest_delay.9b<-inner_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9b<-arrange(dest_delay.9b, desc(arr))
head(dest_delay.9b,5)
nrow(dest_delay.9b)
cat("No. The number of observations do not match in part a and part b.  
      The left_join = 116 observations and inner_join = 114")
   

    # 9c - how many observations are in the new table?
            #- do any NA's appear in arr_delay? if so, why?
dest_delay.9c<-right_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9c<-arrange(dest_delay.9c, desc(arr))
head(dest_delay.9c,5)
nrow(dest_delay.9c)
cat("The right_join part c has 3376 observations. 
      There are NA's in the arrival delays.
    The NA's occur because airports have different number of observations so they don't match up")

    #9d - how many observations are in the new table?
            #- do any NA's appear in arr_delay? if so, why?
dest_delay.9d<-full_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9d<-arrange(dest_delay.9d, desc(arr))
head(dest_delay.9d,5)
nrow(dest_delay.9d)
cat("The full_join part c has 3378 observations. 
      There are NA's in the arrival delays here as well.-different number of obsevartions in airports")

  
#10 merge hourly_delay with the weather data.fram
    #print table summarizing which weather conditions are associated witht he biggest delays

hourly_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date, hour)%>%
  summarize(
    mean = mean(dep_delay,na.rm=TRUE)
  )
hourly_delay$date<-as.Date(hourly_delay$date)
weather$date<-as.Date(weather$date)
hour.10<-left_join(hourly_delay, weather, by=c("date"="date"))


#11 Tidy(r) Data 

    #11a - use tidyr and dplyr tools to reach the follwoing table (gather)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
    #use dplyr adn tidyr to map to
data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)


    #11b-
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
    #use dplyr adn tidyr to map to
data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))


    #11c - 
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
    #use dplyr adn tidyr to map to
data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m","m"),
  age = c(15,50,45,18),
  state = c("CA","NY","HI","DC"),
  value = c(3,4,5,6)
)


    #11d- 
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
    #use dplyr adn tidyr to map to
data.frame(
  subject = c(1,2,3,4),
  demo = c("f.11.DC", "f.55.NY", "m.65.WA", NA),
  value = c(3,4,5,6)
)





