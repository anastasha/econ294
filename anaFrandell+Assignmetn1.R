#Assignment 1- Ana Frandell 
#test

#0 - print name and ID
print ("Ana Frandell 1505099")
#1 - load data 
    r variability?
library(foreign)
df.dta <- read.dta (
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta"
)
df.csv <- read.csv (
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv"
)
      #change it to read.table to work
df.td <- read.table (
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt"
)
load (url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))


#2 #how big (in kg) is each file? Which is the smallest? Besides the .dta file, what accounts for their variability
    # size of df.dta = "452.3 Kb"
df.dta <- (format(object.size(df.dta),units="KB"))
print (df.dta)
    # size of df.csv = "188.5 Kb"
df.csv <- (format(object.size(df.csv),units="KB"))
print (df.csv)
    # size of df.tb  =  "506.4 Kb"
td <- (format(object.size(df.td),units="KB"))
print (td)
    # size of .rdata = "188.5 Kb"
NHIS_2007_RData <- (format(object.size(NHIS_2007_RData),units="KB"))
print (NHIS_2007_RData)

print ("df.dta and NHIS_2007_RData are the smallest")
print ("varaibility in the data differs by the number of characters used. For instance, using tab uses less characters but formats the data nicely")

# for the object df.rdata, what typeof and class of this data structure?
typeof(NHIS_2007_RData)
class(NHIS_2007_RData)

#3 length, dim, nrow, ncol and summary

length(NHIS_2007_RData)
print ("length of NHIS_2007_RData is 9")
dim(NHIS_2007_RData)
print("the dimension of NHIS_2007_RData is 4785 9")
nrow(NHIS_2007_RData)
print ("the number of rows in NHIS_2007_RData is 4785")
ncol(NHIS_2007_RData)
print("the number of columns in NHIS_2007_RData is 9")
summary(NHIS_2007_RData)

#4 Load org_example 

df <- read.table (
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)

#report str
str(df)
print (str(df))
print ("df has 1119754 observations and 30 variables")
min(df$rw, na.rm = TRUE)
mean(df$rw, na.rm=TRUE)
median(df$rw, na.rm=TRUE)
max(df$rw, na.rm = TRUE)
quantile(df$rw, na.rm=TRUE)
print (" the min is 1.814, the mean is 19.8114, the median is 15.875 and the max is 345.801")
print ("the 25 percentile is 10.704728, the 50th percentile is 15.875783, the 75th percentile is 24.358696 and the 100th percentile is 354.801361")

#how many NA's are there?
sum(is.na(df$rw))
print ("there are 521279 NA's")

#5 Why don't the number of values in the vector match the number reported in length?
v <- c(1,2,3,4,5,6,7,4,NULL,NA)
length(v)
print ("NULL is a reserved word- it ignores the argument and returns the word NULL")
mean(v, na.rm=TRUE)


#6
    #generic command:   matrix(data, nrow, ncol, byrow)
x <- matrix(c(1,4,7,2,5,8,3,6,9), nrow=3, ncol=3)
    #transpose 
t(x)
eigen(x)
y <- matrix(c(1,3,2,2,2,3,3,1,0), nrow=3)
yinv <- solve(y)
y %*% yinv
print ("this new matrix is called the identity")


#7
carat = c(5, 2, 0.5, 1.5, 5, NA, 3)
cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair")
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", NA)
price = c(850, 450, 450, "NULL", 750, 980, 420)

#df = data.frame(n,s)
diamonds = data.frame(carat, cut, clarity, price)
print(diamonds)
#what is the mean price?
pr <- as.numeric(as.character(diamonds$price))
mean(pr, na.rm=TRUE)
#what is the mean price of cut "fair"?
f <- subset(diamonds, cut=="fair")
fair <- as.numeric(as.character(f$price))
mean(fair)
#what is the mean price of cut "good", "very good" and "ideal"?
not <- subset(diamonds, cut!="fair")
fairnot <- as.numeric(as.character(not$price))
mean(fairnot, na.rm=T)
# diamonds with greater than 2 carats, and cut "Ideal" or "Very good", what is the median price?
diam <- subset(diamonds, carat>=2 & cut=="Ideal" | cut=="very good")
diam1 <- as.numeric(as.character(diam$price))
median(diam1, na.rm=T)
print ("median price with carat greater than 2 and cut Ideal or very good is 450")

