#create data frames for testing
copy.df_1<- function (x){
  date <- seq(as.Date("2018/1/1"), as.Date(Sys.time()), "days")
  state <- sample(0:4, length(date), replace=T)
  my.df <- cbind.data.frame(date, state )
  my.df[nrow(my.df),2] <- 1
  x <- my.df
  return (x)
}

copy.df_0<- function (x){
  date <- seq(as.Date("2018/1/1"), as.Date(Sys.time()), "days")
  state <- sample(0:4, length(date), replace=T)
  my.df <- cbind.data.frame(date, state )
  my.df[nrow(my.df),2] <- 0
  x <- my.df
  return (x)
}

#Group of people with smallest number of overtimes that are not working,
#will be suggested
#
#
# call.in <- function (x){ #Based on least overtime
#   x <- selectByDate (x, month = c(m), year = c(y))
#   length(which(x[,2]==4)) #Overtime
# }

#Group of people with biggest number of overtimes that are working!
#will be suggested
#
#
send.home <- function (x){ #Based on overtime
  x <- selectByDate (x, month = c(m), year = c(y))
  length(which(x[,2]==4)) #Overtime
}