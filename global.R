#Small scheduler app for Ray and his hospital
library (openair)
library(data.table)
source ('functions.R')

l <- list ('Ray Labayen','Lola Labayen','Timur Sabitov', 'Harper Weiczorak','Michael Weiczorak')
names(l)<-l

#working nurses
l[c(1,2, 3)] <- lapply (l[c(1,2,3)],copy.df_1)

#resting nurses
l[c(4,5)] <- lapply (l[c(4,5)],copy.df_0)

#select month, year, number of nurses (call, lay off)
m <- c(1)
y <- c(2019)
N <- 2

#todays shift
today_shift <- c('Ray Labayen','Lola Labayen', 'Timur Sabitov')
my.df_work <- l[c(which (names(l) %in% today_shift))]

today_home <- c('Harper Weiczorak', 'Michael Weiczorak')
my.df_home <- l[c(which (names(l) %in% today_home))]


#call
nurses_to_call <-lapply (l[which(!names(l) %in% today_shift)], call.in)
print (paste0('Call for: ', names(sort(unlist(nurses_to_call), decreasing = FALSE)[c(1:N)])))

#send
nurses_to_send <-lapply (l[which(names(l) %in% today_shift)], send.home)
print (paste0('Lay off: ', names(sort(unlist(nurses_to_send), decreasing = T)[c(1:N)])))