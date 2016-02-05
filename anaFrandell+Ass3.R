################## Assignment 3   ###############

print ("Ana Frandell")
print ("1505099")
print ("afrandel@ucsc.edu")

## 1 
library(foreign)
df.ex <- read.dta (file='https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta')

      #load this way for a dataframe 
class(df.ex)

## 2 - filter 
install.packages("dplyr")
library(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))

df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2))


## 3 - Arrange - sorted year and months ascending
df.ex.3a <- df.ex %>%
  dplyr::arrange(
    year, month
  )


##4  - Select 
df.ex.4a <- select(df.ex, year:age)
df.ex.4b <- select(df.ex, year, month, starts_with("i"))
head(df.ex)                           #shows all columns 
distinct (select(df.ex, state))       #prints values in the variable STATE


##5 - Mutate 
stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}
                                    #remove any NA-true
nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}
df.ex.5a <-df.ex %>%
  dplyr::mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
  )

df.ex.5b <- df.ex %>%
  dplyr::group_by(year, month) %>%
  dplyr::mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count    = n()
  )


##6- Summarize 
                  #quartile 1 - 25th percentile, quartile 3- 75th percentile
df.ex.6 <- df.ex %>%
  dplyr::group_by(year, month, state) %>%
  dplyr::summarise(
    rw.min = min(rw, na.rm = T),
    rw.1stq = quantile(rw, 0.25, na.rm = T),
    rw.mean = mean(rw, na.rm = T),
    rw.median = median(rw, na.rm = T),
    rw.3rdq = quantile(rw, 0.75, na.rm = T),
    rw.max = max(rw, na.rm = T),
    count = n()
  )
print(nrow(df.ex.6))   #show observations -4284
                           
max.mean <- (max(df.ex.6$rw.mean))
row <- filter(df.ex.6, rw.mean== max.mean)
row_max <- select (row, year, month, state)
print(row_max)              #print year, month, state observation-highest mean real wage

##7 - challenge (extra credit)
    # sort data- year, month ascending order; state descneding alphabetical order
df.ex.7a <- df.ex %>%
  dplyr::arrange(
    year, month, desc(as.character(df.ex$state))
  )


