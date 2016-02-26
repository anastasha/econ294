# # # # ## # # # # ASSIGNEMENT 5- r.lAB # # # # # # # # # #
# # # # Ana Frandell

# 1 gggplot2 diamond's dataset -replicate following plots 

    ##part a - scatter plot of diamond prices, volume(x*y*z) with point size carat, color clarity
    ## and x-axis and y-axis log scale (ex: scale_x_log10)

#ggplot(data, mapping) +
#layer(
  #state="",
  #geom="",
  #position="",
  #geom_parms=list(),
  #stat_params=;ist(),
#)
install.packages("ggplot2")
library(ggplot2)
head(diamonds)
s.1a<-ggplot(diamonds, 
          aes(x=log(x*y*z), y=log(price)))
s.1a+geom_point()
s.1a+geom_point(aes(colour=clarity, size=carat, alpha=0.5))
    #part b 
install.packages("reshape2")
library(reshape2)
h.1b<-ggplot(diamonds, 
             aes(x = carat, y = ..density..)) + facet_wrap(~cut)
h.1b+geom_histogram(aes(fill = clarity)) + facet_grid(cut ~ .)  #to make it stack vertically
       
    #part c 
v.1c<-ggplot(diamonds, 
                aes(x = cut, y = price))
v.1c + geom_violin() + geom_jitter(alpha = .05)  #alpha changes shading 

    ## Question 3
library(foreign)
original<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

library(dplyr)
    #part a - plot monthly medium rw in line plot
    #geom_ine, geom_ribbon, alpha, yline 
org <-original %>%
  dplyr::group_by(year, month) %>%         #want x-axis by yer
  dplyr::summarize(
    rw_median = median(rw, na.rm=TRUE),
    rw.75 = quantile(rw, 0.75, na.rm = TRUE),
    rw.25 = quantile(rw, 0.25, na.rm = TRUE),
    rw.10 = quantile(rw, prob=1/10, na.rm = TRUE, type = 5),
    rw.90 = quantile(rw, prob=9/10, na.rm = TRUE, type = 5)
  )
  #create date variable 
org$date<- as.Date(paste(org$month, org$year, "01", sep="."), format = "%m.%Y.%d")
  #plot line graph
p.2a <- ggplot(org,
  aes(x=date, y=rw_median))
p.2a+geom_line()+geom_ribbon(aes(ymin = rw.10, ymax = rw.90, alpha=0.2))+
geom_ribbon(aes(ymin = rw.25, ymax = rw.75, alpha=0.2))

    #part b - plot monthly real mean wage, broken down my education level
org.2 <-original %>%
  dplyr::group_by(year, month, educ) %>%     #group by 3 different variables 
  dplyr::summarize(
    rw_median= median(rw, na.rm=TRUE)
  )
org.2$date<- as.Date(paste(org.2$month, org.2$year, "01", sep="."), format = "%m.%Y.%d")
ggplot(data=org.2, aes(x=date, y=rw_median, group = educ, colour = educ)) + geom_line()

