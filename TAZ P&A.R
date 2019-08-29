install.packages("ggplot2")
install.packages("tidyverse")

library(ggplot2)
library(tidyverse)

getwd()
setwd("C:/Users/chris/Desktop/GT/CP6321")

#create matrix of households
hh = matrix(c(1850, 2942, 2536, 1490,3744, 3868,6142, 8362,242, 1384, 8356, 9934),nrow=4)
#create matrix to trips
trips = matrix(c(10980, 21050, 18500, 15090, 48210, 61290, 139890, 184110, 2060, 15010, 197820, 251060), nrow=4)
#calulate trip rates
trip_rate = matrix(trips/hh)

#flip over traip_rate
trip_rate = c(trip_rate)

#Forcasted number of households in TAZ
tazhh = matrix(c(400, 4000, 100, 0, 200, 2000, 1000,
                 400, 1000, 200, 0, 600, 2000, 1600,
                 600, 6000, 400, 200, 2000, 2000, 100,
                 100, 1000, 0, 0, 100, 4000, 2000,
                 600, 2000, 0, 0, 1000, 1200, 3000,
                 200, 2000, 560, 600, 3000, 1000, 1800,
                 40, 200, 0, 0, 0, 4000, 1600,
                 100, 1000, 0, 0, 200, 2000, 2000,
                 800, 5000, 300, 100, 5000, 1000, 100,
                 0, 200, 0, 0, 0, 4000, 1000,
                 20, 500, 0, 0, 0, 4000, 1000,
                 121, 1840, 200, 175, 8000, 400, 0), nrow=7)

#multiply household estimate matix by trip tate vector by row
est_trips = sweep(tazhh, MARGIN = 2, trip_rate, '*')
#sum trips by TAZ and transpose to a single column matrix, t = flip over
taz_trip_prodution =matrix(t(rowSums(est_trips)))


taz_hh_sum = rowSums(tazhh)

trip_attraction_rate = c(13, 1.7, 0.5, 1.45, 1)

zone_data = matrix(c(15400, 0, 20000, 16000, 500, 1000, 500,
                     13500, 500, 23000, 11000, 200, 500, 1000,
                     9000, 0, 15000, 15000, 0, 500, 200,
                     37900, 500, 58000, 42000, 700, 2000, 1700), nrow=7)

zone_data_hh = cbind(zone_data, taz_hh_sum)

#total number of attractions
taz_trip_attraction = sweep(zone_data_hh, MARGIN = 2, trip_attraction_rate, '*')
taz_trip_attraction_new =matrix(t(rowSums(taz_trip_attraction)))

##PANDA##
panda = cbind(taz_trip_prodution, taz_trip_attraction_new)

inter_external_trips = matrix(c(70000,30000, 8000,5000), nrow=2)

panda_full = rbind(panda, inter_external_trips)



###balancing Panda
adjust<-((sum(taz_trip_prodution)+sum(inter_external_trips[,1])-sum(inter_external_trips[,2]))/sum(taz_trip_attraction))

bal_taz_trip_attr<-taz_trip_attraction_new*adjust

bal_panda<-cbind(taz_trip_prodution,bal_taz_trip_attr)
bal_panda_full<-rbind(bal_panda,inter_external_trips)

#get sums
bal_panda_full_sums<-matrix(colSums(bal_panda_full),nrow=1) #these two numbers should be equal

bal_panda_full<-rbind(bal_panda_full,bal_panda_full_sums)

bal_panda_full
