##### Homework 2 - ANa Frandell ##########


# 0 - identify information
AnaFrandellAssignment2 <- list(
    fristName = "Ana",
    lastName = "Frandell",
    email = "anafrandell@hotmail.com",
    studentID = 212859817
    )

#1 - Load the following .RData file
diamonds <- get(  
  load(
    file = url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")
  )
)


AnaFrandellAssignment2$s1a <- nrow(diamonds)
AnaFrandellAssignment2$s1b <- ncol(diamonds)
AnaFrandellAssignment2$s1c <- names(diamonds)
AnaFrandellAssignment2$s1d <- summary(diamonds$price)

      #load tab-separared file
N <- read.table(file = "https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_TSV.txt", header=TRUE)

########## Question 2 ##########
AnaFrandellAssignment2$s2a <- nrow(N)
AnaFrandellAssignment2$s2b <- ncol(N)
AnaFrandellAssignment2$s2c <- names(N)
AnaFrandellAssignment2$s2d <- mean(N$weight)
AnaFrandellAssignment2$s2e <- median(N$weight)

col <- ifelse( test = N$weight>=996 & N$weight<=999,
               yes = NA,
               no = N$weight)
AnaFrandellAssignment2$s2 <- hist(col)
table(col)
AnaFrandellAssignment2$s2f <- mean(col, na.rm=TRUE)
AnaFrandellAssignment2$s2g <- median(col, na.rm=TRUE)

sum_m <- subset(N, weight<996 & SEX==1)
sum_f <- subset(N, weight<996 & SEX==2)

AnaFrandellAssignment2$s2h  <- summary(sum_f$weight, na.rm=TRUE)
AnaFrandellAssignment2$s2i <- summary(sum_m$weight, na.rm=TRUE)
  

 

############## Question 3 ############
v <- c(letters,LETTERS)
as.factor(v)
AnaFrandellAssignment2$s3a <-   even <- v[seq(2, length(v),2)]
AnaFrandellAssignment2$s3b <-   name <- paste(v[c(27,14,1)], collapse="")
arr <- array( c(letters,LETTERS), dim= c(3,3,3))
AnaFrandellAssignment2$s3c <-  arr[,1,2]
AnaFrandellAssignment2$s3d <-  arr[2,2,]
AnaFrandellAssignment2$s3e <-  paste(arr[3,3,3],arr[2,2,2],arr[1,1,1])


print(AnaFrandellAssignment2$s3e)
print(arr)


