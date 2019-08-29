#install.packages("ggplot2")
#install.packages("tidyverse")

library(ggplot2)
library(tidyverse)

getwd()
setwd("C:/Users/chris/Desktop/GT/CP6321")

#create matrix of households
hh = matrix(c(1850, 2942, 2536, 1490,3744, 3868,6142, 8362,242, 1384, 8356, 9934),nrow=4,byrow = T)
#create matrix to trips
trips = matrix(c(10980, 21050, 18500, 15090, 48210, 61290, 139890, 184110, 2060, 15010, 197820, 251060), nrow=4,byrow = T)
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
panda_full_sums = matrix(colSums(panda),nrow=1)

panda_full = rbind(panda, panda_full_sums)
panda_full

###balancing Panda
adjust<-((sum(taz_trip_prodution)+sum(inter_external_trips[,1])-sum(inter_external_trips[,2]))/sum(taz_trip_attraction))

bal_taz_trip_attr<-taz_trip_attraction_new*adjust

bal_panda<-cbind(taz_trip_prodution,bal_taz_trip_attr)
bal_panda_full<-rbind(bal_panda,inter_external_trips)

#get sums
bal_panda_full_sums<-matrix(colSums(bal_panda_full),nrow=1) #these two numbers should be equal

bal_panda_full_includesums<-rbind(bal_panda_full,bal_panda_full_sums)

##################################################################################################
###Trip Distribution ###
##Productions
prod<-matrix(bal_panda_full[ ,1],nrow=9)

##Attractions
act.attr<-matrix(bal_panda_full[ ,2],nrow=9)


################################
####Radial model####
################################

#initialize
iterations=5 #set the variable for the number of iterations

adj_attr<-matrix(NA, nrow = iterations+1, ncol = 9) #create an empty matrix to store the distributed trips each iteration
adj_attr[1,]<-t(act.attr) #set the first row base values

est_attr<-matrix(NA, nrow = iterations+1, ncol = 9) #create an empty matrix to store the estimated trips each iteration
est_attr[1,]<-t(act.attr) #set the first row base values

adj.fact<-1 #set first ajdustment factor 

for (i in 1:iterations) {
  ##Zone 1 = prod, attr, dist, spd, t.time, term time,ff
  z1<-data.frame(prod=rep(bal_panda_full[1,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(0,1,2,2.414,1,2,3,2,4), #the grid distance in number of links
                 dist_tran=c(0,6.828,9.656,4.828,2,5.828,7.656,NA,NA),    # distances calculated by the radial route map
                 t.time=c(0,1,2,2,1,2,3,2,4)*10, #dist*10
                 t.time_tran=c(0,6.828+0.5,9.656+0.5,4.828,2,5.828+0.5,7.656+0.5,99999,99999)*10, 
                 #calculate transit time based on
                 #(1)train route 20mon/link, bus 10min/link
                 #(2)identify the number of transfers needed for each intersection, +0.5 (multiply by 10 to get 5 mins)
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  #z1$t.time=z1$dist/z1$spd*60
  z1$f.f=c(0,133,133,1582,5473,459,459,39,133)
  
  ##Zone 2 = prod, attr, dist, spd, t.time, term time,ff
  z2<-data.frame(prod=rep(bal_panda_full[2,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(1,0,1,2.414,2,1,2,1,3),
                 dist_tran=c(6.828,0,6.828,2,4.828,1,4.828,NA,NA),
                 t.time=c(1,0,1,2,2,1,2,1,3)*10,
                 t.time_tran=c(6.828+0.5,0,6.828+0.5,2,4.828+0.5,1,4.828+0.5,99999,99999)*10,
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  z2$f.f=c(133,0,133,1582,459,5473,459,5473,133)
  
  ##Zone 3 = prod, attr, dist, spd, t.time, term time,ff
  z3<-data.frame(prod=rep(bal_panda_full[3,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(2,1,0,2.414,3,2,1,2,2),
                 dist_tran=c(9.656,6.868,0,4.828,6.828,5.828,2,NA,NA),
                 t.time=c(2,1,0,2,3,2,1,2,2)*10,
                 t.time_tran=c(9.656+0.5,6.868+0.5,0,4.828,6.828+0.5,5.828+0.5,2,99999,99999)*10,
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  z3$f.f=c(133,133,0,1582,459,459,5473,39,1582)
  
  ##Zone 4 = prod, attr, dist, spd, t.time, term time,ff
  z4<-data.frame(prod=rep(bal_panda_full[4,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(2.414,2,2.414,0,1.414,1,1.414,3,2.414),
                 dist_tran=c(4.828,2,4.282,0,2.828,1,2.828,NA,NA),
                 t.time=c(3,2,3,0,1,1,1,3,2)*10,
                 t.time_tran=c(4.828,2,4.282,0,2.828,1,2.828,99999,99999)*10,
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(5,5,5,5,5,5,5,5,5))
  
  z4$f.f=c(964,964,964,0,3328,3328,3328,280,964)
  
  ##Zone 5 = prod, attr, dist, spd, t.time, term time,ff
  z5<-data.frame(prod=rep(bal_panda_full[5,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(1,2,3,1.414,0,1,2,3,3),
                 dist_tran=c(2,4.828,7.656,2.828,0,3.828,5.656,NA,NA),
                 t.time=c(1,2,3,1,0,1,2,3,3)*10,
                 t.time_tran=c(2,4.828+0.5,7.656+0.5,2.828,0,3.828+0.5,5.656+0.5,99999,99999)*10,
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  z5$f.f=c(5473,459,459,5473,0,1582,1582,133,459)
  
  ##Zone 6 = prod, attr, dist, spd, t.time, term time,ff
  z6<-data.frame(prod=rep(bal_panda_full[6,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(2,1,2,1,1,0,1,2,2),
                 dist_tran=c(5.828,1,5.828,1,3.828,0,3.828,NA,NA),
                 t.time=c(2,1,2,1,1,0,1,2,2)*10,
                 t.time_tran=c(5.828+0.5,1,5.828+0.5,1,3.828+0.5,0,3.828+0.5,99999,99999)*10,
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  z6$f.f=c(459,5473,459,5473,1582,0,1582,1582,459)
  
  ##Zone 7 = prod, attr, dist, spd, t.time, term time,ff
  z7<-data.frame(prod=rep(bal_panda_full[7,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(3,2,1,1.414,2,1,0,3,1),
                 dist_tran=c(7.656,4.828,2,2.828,5.656,3.828,0,NA,NA),
                 t.time=c(3,2,1,1,2,1,0,3,1)*10,
                 t.time_tran=c(7.656+0.5,4.828+0.5,2,2.828,5.656+0.5,3.828+0.5,0,99999,99999)*10,
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  z7$f.f=c(459,459,5473,5473,1582,1582,0,133,5473)
  
  ##Zone 8 = prod, attr, dist, spd, t.time, term time,ff
  z8<-data.frame(prod=rep(bal_panda_full[8,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(2,1,2,3,3,2,3,0,4),
                 dist_tran=c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 t.time=c(2,1,2,3,3,2,3,0,4)*10,
                 t.time_tran=c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  z8$f.f=c(39,99999,39,459,133,1582,133,0,0)
  
  ##Zone 9 = prod, attr, dist, spd, t.time, term time,ff
  z9<-data.frame(prod=rep(bal_panda_full[9,1],each=9),
                 attr=c(est_attr[i,])*adj.fact,
                 dist=c(4,4,2,2.414,3,2,1,4,0),
                 dist_tran=c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 t.time=c(4,4,2,2.414,3,2,1,4,0)*10,
                 t.time_tran=c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 OVTT=c(4,4,4,4,4,4,4,4,4),
                 term.time=c(1,1,1,1,1,1,1,1,1))
  
  z9$f.f=c(133,133,1582,1582,459,1582,99999,0,0)
  
  
  
  ##absolute attractiveness (attr*ff)
  z1$ab.attract <- z1$attr*z1$f.f
  z2$ab.attract <- z2$attr*z2$f.f
  z3$ab.attract <- z3$attr*z3$f.f
  z4$ab.attract <- z4$attr*z4$f.f
  z5$ab.attract <- z5$attr*z5$f.f
  z6$ab.attract <- z6$attr*z6$f.f
  z7$ab.attract <- z7$attr*z7$f.f
  z8$ab.attract <- z8$attr*z8$f.f
  z9$ab.attract <- z9$attr*z9$f.f
  
  ##relative attractiveness (attr*ff)
  z1$rel.attract <- z1$ab.attract/sum(z1$ab.attract)
  z2$rel.attract <- z2$ab.attract/sum(z2$ab.attract)
  z3$rel.attract <- z3$ab.attract/sum(z3$ab.attract)
  z4$rel.attract <- z4$ab.attract/sum(z4$ab.attract)
  z5$rel.attract <- z5$ab.attract/sum(z5$ab.attract)
  z6$rel.attract <- z6$ab.attract/sum(z6$ab.attract)
  z7$rel.attract <- z7$ab.attract/sum(z7$ab.attract)
  z8$rel.attract <- z8$ab.attract/sum(z8$ab.attract)
  z9$rel.attract <- z9$ab.attract/sum(z9$ab.attract)
  
  ##trips (between i and j)
  z1$t.ij<- z1$prod*z1$rel.attract
  z2$t.ij<- z2$prod*z2$rel.attract
  z3$t.ij<- z3$prod*z3$rel.attract
  z4$t.ij<- z4$prod*z4$rel.attract
  z5$t.ij<- z5$prod*z5$rel.attract
  z6$t.ij<- z6$prod*z6$rel.attract
  z7$t.ij<- z7$prod*z7$rel.attract
  z8$t.ij<- z8$prod*z8$rel.attract
  z9$t.ij<- z9$prod*z9$rel.attract
  
  #trip interchange table
  t.inter<-matrix(c((z1$t.ij),(z2$t.ij),(z3$t.ij),(z4$t.ij),(z5$t.ij),(z6$t.ij),(z7$t.ij),(z8$t.ij),(z9$t.ij)),byrow=T,nrow=9)
  
  #new panda
  new.panda<-matrix(c(rowSums(t.inter),colSums(t.inter)),nrow=9)
  new.panda<-cbind(new.panda,act.attr)
  colnames(new.panda)<-(c("prod","attr","actual.attr"))
  
  adj_attr[i+1,]<-(new.panda[,2])
  
  adj.fact<-act.attr/adj_attr[i+1,]
  
  est_attr[i+1,]<-z1$attr
  
} #end loop

######################################################################################
#####mode choice----

#auto cost/mile
z1$t.cost<-z1$dist*.5
z2$t.cost<-z2$dist*.5
z3$t.cost<-z3$dist*.5
z4$t.cost<-z4$dist*.5+2.5
z5$t.cost<-z5$dist*.5
z6$t.cost<-z6$dist*.5
z7$t.cost<-z7$dist*.5
z8$t.cost<-z8$dist*.5
z9$t.cost<-z9$dist*.5

#Auto OVTT
z1$ovtt<-z1$term.time+z1$OVTT
z2$ovtt<-z2$term.time+z2$OVTT
z3$ovtt<-z3$term.time+z3$OVTT
z4$ovtt<-z4$term.time+z4$OVTT
z5$ovtt<-z5$term.time+z5$OVTT
z6$ovtt<-z6$term.time+z6$OVTT
z7$ovtt<-z7$term.time+z7$OVTT
z8$ovtt<-z8$term.time+z8$OVTT
z9$ovtt<-z9$term.time+z9$OVTT

#Auto IVTT
z1$ivtt<-z1$t.time
z2$ivtt<-z2$t.time
z3$ivtt<-z3$t.time
z4$ivtt<-z4$t.time
z5$ivtt<-z5$t.time
z6$ivtt<-z6$t.time
z7$ivtt<-z7$t.time
z8$ivtt<-z8$t.time
z9$ivtt<-z9$t.time

#transit OVTT
z1$transit.ovtt<-2.5+4+4
z2$transit.ovtt<-2.5+4+4
z3$transit.ovtt<-2.5+4+4
z4$transit.ovtt<-2.5+4+4
z5$transit.ovtt<-2.5+4+4
z6$transit.ovtt<-2.5+4+4
z7$transit.ovtt<-2.5+4+4
z8$transit.ovtt<-2.5+4+4
z9$transit.ovtt<-2.5+4+4

#transit Cost
z1$transit.cost<-1
z2$transit.cost<-1
z3$transit.cost<-1
z4$transit.cost<-1
z5$transit.cost<-1
z6$transit.cost<-1
z7$transit.cost<-1
z8$transit.cost<-1
z9$transit.cost<-1

#transit IVTT
z1$transit.ivtt<-z1$t.time_tran
z2$transit.ivtt<-z2$t.time_tran
z3$transit.ivtt<-z3$t.time_tran
z4$transit.ivtt<-z4$t.time_tran
z5$transit.ivtt<-z5$t.time_tran
z6$transit.ivtt<-z6$t.time_tran
z7$transit.ivtt<-z7$t.time_tran
z8$transit.ivtt<-z8$t.time_tran
z9$transit.ivtt<-z9$t.time_tran

#mode utility
z1$v.auto<-.1*z1$ivtt-.2*z1$ovtt-.05*z1$t.cost
z2$v.auto<-.1*z2$ivtt-.2*z2$ovtt-.05*z2$t.cost
z3$v.auto<-.1*z3$ivtt-.2*z3$ovtt-.05*z3$t.cost
z4$v.auto<-.1*z4$ivtt-.2*z4$ovtt-.05*z4$t.cost
z5$v.auto<-.1*z5$ivtt-.2*z5$ovtt-.05*z5$t.cost
z6$v.auto<-.1*z6$ivtt-.2*z6$ovtt-.05*z6$t.cost
z7$v.auto<-.1*z7$ivtt-.2*z7$ovtt-.05*z7$t.cost
z8$v.auto<-.1*z8$ivtt-.2*z8$ovtt-.05*z8$t.cost
z9$v.auto<-.1*z9$ivtt-.2*z8$ovtt-.05*z9$t.cost

z1$v.transit<- -.3-.1*z1$transit.ivtt-.2*z1$transit.ovtt-.05*z1$transit.cost
z2$v.transit<- -.3-.1*z2$transit.ivtt-.2*z2$transit.ovtt-.05*z2$transit.cost
z3$v.transit<- -.3-.1*z3$transit.ivtt-.2*z3$transit.ovtt-.05*z3$transit.cost
z4$v.transit<- -.3-.1*z4$transit.ivtt-.2*z4$transit.ovtt-.05*z4$transit.cost
z5$v.transit<- -.3-.1*z5$transit.ivtt-.2*z5$transit.ovtt-.05*z5$transit.cost
z6$v.transit<- -.3-.1*z6$transit.ivtt-.2*z6$transit.ovtt-.05*z6$transit.cost
z7$v.transit<- -.3-.1*z7$transit.ivtt-.2*z7$transit.ovtt-.05*z7$transit.cost
z8$v.transit<- -99999
z9$v.transit<- -99999

#mode split
z1$p.auto<-exp(z1$v.auto)/(exp(z1$v.auto)+exp(z1$v.transit))
z1$p.transit<-exp(z1$v.transit)/(exp(z1$v.auto)+exp(z1$v.transit))

z2$p.auto<-exp(z2$v.auto)/(exp(z2$v.auto)+exp(z2$v.transit))
z2$p.transit<-exp(z2$v.transit)/(exp(z2$v.auto)+exp(z2$v.transit))

z3$p.auto<-exp(z3$v.auto)/(exp(z3$v.auto)+exp(z3$v.transit))
z3$p.transit<-exp(z3$v.transit)/(exp(z3$v.auto)+exp(z3$v.transit))

z4$p.auto<-exp(z4$v.auto)/(exp(z4$v.auto)+exp(z4$v.transit))
z4$p.transit<-exp(z4$v.transit)/(exp(z4$v.auto)+exp(z4$v.transit))

z5$p.auto<-exp(z5$v.auto)/(exp(z5$v.auto)+exp(z5$v.transit))
z5$p.transit<-exp(z5$v.transit)/(exp(z5$v.auto)+exp(z5$v.transit))

z6$p.auto<-exp(z6$v.auto)/(exp(z6$v.auto)+exp(z6$v.transit))
z6$p.transit<-exp(z6$v.transit)/(exp(z6$v.auto)+exp(z6$v.transit))

z7$p.auto<-exp(z7$v.auto)/(exp(z7$v.auto)+exp(z7$v.transit))
z7$p.transit<-exp(z7$v.transit)/(exp(z7$v.auto)+exp(z7$v.transit))

z8$p.auto<-exp(z8$v.auto)/(exp(z8$v.auto)+exp(z8$v.transit))
z8$p.transit<-exp(z8$v.transit)/(exp(z8$v.auto)+exp(z8$v.transit))

z9$p.auto<-exp(z9$v.auto)/(exp(z9$v.auto)+exp(z9$v.transit))
z9$p.transit<-exp(z9$v.transit)/(exp(z9$v.auto)+exp(z9$v.transit))

#final mode split
z1$auto.trips<-z1$t.ij*z1$p.auto
z1$transit.trips<-z1$t.ij*z1$p.transit

z2$auto.trips<-z2$t.ij*z2$p.auto
z2$transit.trips<-z2$t.ij*z2$p.transit

z3$auto.trips<-z3$t.ij*z3$p.auto
z3$transit.trips<-z3$t.ij*z3$p.transit

z4$auto.trips<-z4$t.ij*z4$p.auto
z4$transit.trips<-z4$t.ij*z4$p.transit

z5$auto.trips<-z5$t.ij*z5$p.auto
z5$transit.trips<-z5$t.ij*z5$p.transit

z6$auto.trips<-z6$t.ij*z6$p.auto
z6$transit.trips<-z6$t.ij*z6$p.transit

z7$auto.trips<-z7$t.ij*z7$p.auto
z7$transit.trips<-z7$t.ij*z7$p.transit

z8$auto.trips<-z8$t.ij*z8$p.auto
z8$transit.trips<-z8$t.ij*z8$p.transit

z9$auto.trips<-z9$t.ij*z9$p.auto
z9$transit.trips<-z9$t.ij*z9$p.transit

#mode matrix
auto.mode<-matrix(round(c(z1$auto.trips,z2$auto.trips,z3$auto.trips,z4$auto.trips,z5$auto.trips,z6$auto.trips,z7$auto.trips,z8$auto.trips,z9$auto.trips)),byrow = T,nrow = 9)
transit.mode<-matrix(round(c(z1$transit.trips,z2$transit.trips,z3$transit.trips,z4$transit.trips,z5$transit.trips,z6$transit.trips,z7$transit.trips,z8$transit.trips,z9$transit.trips)),byrow = T,nrow = 9)

write.csv(auto.mode,file="Radial_auto")
write.csv(transit.mode,file="Radial_transit")

sumtable=auto.mode+transit.mode
sumtable=cbind(sumtable,rowSums(sumtable))
sumtable=rbind(sumtable,colSums(sumtable))
sumtable