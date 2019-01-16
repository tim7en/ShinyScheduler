#Small scheduler app for Ray and his hospital
library (openair)
library(data.table)

date <- seq(as.Date("2018/1/1"), as.Date(Sys.time()), "days")
state <- sample(0:4, length(date), replace=T)
my.df <- cbind.data.frame(date, state )


calendarPlot(selectByDate(my.df, month = c(3,6,10), year = 2018),cols = c("lightblue",'orange',"green", "yellow",  "red"),
             breaks = c(0, 1, 2, 3, 4,5),labels = c("Off",'P', "HOLD", "SendOff", "New"), 
             pollutant = "state", year = 2018)

l <- list ('Ray Labayen','Lola Labayen','Harper Weiczorak','Michael Weiczorak')
names(l)<-l

copy.df<- function (x){
  x <- my.df
  return (x)
}

l <- lapply (l,copy.df)
