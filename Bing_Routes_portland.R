#list needed packages
list.of.packages <- c("rgdal","RCurl","RJSONIO","rgeos","maptools","broom","ggplot2","dplyr","rjson","chron","sp","leaflet","reshape","KernSmooth","htmlwidgets","data.table")

#load packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(list.of.packages, require, character.only = TRUE)

#set working directory
setwd("C:/Users/chris/Desktop/GT/APA MAP/Portland")

#set Bing maps API key (https://msdn.microsoft.com/en-us/library/ff428642.aspx)
BingMapsKey<-"AvJCPTU1_i2nkUhZlIAEqqPdJpsCi9vJ7gAWmkgDvKHarIwwbr5ZGfbijBrAoY3P"

#get portland data

portland_raw_jan<-read.csv("2018_01.csv", header=T,stringsAsFactors = F)
portland_raw_feb<-read.csv("2018_02.csv", header=T,stringsAsFactors = F)
portland_raw_mar<-read.csv("2018_03.csv", header=T,stringsAsFactors = F)
portland_raw_apr<-read.csv("2018_04.csv", header=T,stringsAsFactors = F)
portland_raw_may<-read.csv("2018_05.csv", header=T,stringsAsFactors = F)
portland_raw_jun<-read.csv("2018_06.csv", header=T,stringsAsFactors = F)

portland_raw<-rbind(portland_raw_jan,portland_raw_feb,portland_raw_mar,portland_raw_apr,portland_raw_may,portland_raw_jun)
#portland_raw<-portland_raw_may

#get one lat/lon for hubs
force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}
portland_raw_hubloc<-force_bind(portland_raw[,c("StartHub","StartLatitude","StartLongitude")],portland_raw[,c("EndHub","EndLatitude","EndLongitude")])
portland_raw_hubloc<-unique(portland_raw_hubloc)
portland_raw_hubloc[portland_raw_hubloc==""] <- NA #get rid of dockless trips
portland_raw_hubloc <- na.omit(portland_raw_hubloc)

portland_raw_hubloc<-portland_raw_hubloc[!duplicated(portland_raw_hubloc$StartHub),] #keep only first instance of a hub
  colnames(portland_raw_hubloc)<-c("Hub","Latitude","Longitude")

#convert duration to time
portland_raw$Duration <- chron(times=portland_raw$Duration)

#reduce trips that are very short or very long
portland_raw<-portland_raw[which(minutes(portland_raw$Duration)>3 & minutes(portland_raw$Duration)<60),]

#just get needed data
portland_raw<-portland_raw[c("StartHub","StartLatitude","StartLongitude","EndHub","EndLatitude","EndLongitude")]
  
  #replace missing hubs with "Dockless rental"
  portland_raw$StartHub <- sub("^$", "DOCKLESS RENTAL", portland_raw$StartHub)
  portland_raw$EndHub <- sub("^$", "DOCKLESS RENTAL", portland_raw$EndHub)

  #create unique pair ID
  portland_raw$Unique.Path.ID<-paste0(portland_raw$StartHub,"_",portland_raw$EndHub)
 
   #Filter out Dockless to Dockless Rentals - need to figure out how to handle these better
  portland_raw<-portland_raw[!grepl("DOCKLESS RENTAL", portland_raw$Unique.Path.ID),]
  portland_raw<-portland_raw%>%filter_("StartHub!=EndHub")
    
    #count the number of OD trips  
   portland_raw_count<-portland_raw %>% 
                             group_by(Unique.Path.ID) %>% 
                             summarise(n = n()) %>% 
                             arrange(desc(n))
  
   #add the counts back to the OD data - here we will sub in the standard x/y for the reported
   portland_raw_od <- unique(portland_raw[,c("Unique.Path.ID","StartHub","EndHub"), drop=FALSE])
   portland_raw_od <- left_join(portland_raw_od, portland_raw_count)
   portland_raw_od<-left_join(portland_raw_od, portland_raw_hubloc,by=c("StartHub"="Hub"))
      setnames(portland_raw_od, old = c('Latitude','Longitude'), new = c('StartLatitude','StartLongitude'))
      portland_raw_od<-left_join(portland_raw_od, portland_raw_hubloc,by=c("EndHub"="Hub"))
      setnames(portland_raw_od, old = c('Latitude','Longitude'), new = c('EndLatitude','EndLongitude'))
      
    #Walking Routes (this sets up the API query for walking travel)
walking <- function(origin,destination, BingMapsKey){
  require(RCurl)
  require(RJSONIO)
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/V1/Routes/Walking?wp.0=",origin,"&wp.1=",destination,"&key=",BingMapsKey))
  d <- getURL(u)
  j <- RJSONIO::fromJSON(d,simplify = FALSE) 
  if (j$statusCode == 200) {
    j<-j
  }
  else {    
    j <- NA
  }
  j
}


#option to subset OD pair for testing
portland_pairs_sub<-slice(portland_raw_od,1:500)
#portland_pairs_sub<-portland_raw_od

#Create a data frame to hold all the maneuvers lat/long for each OD
mlat.df<-data.frame(matrix(0, nrow = nrow(portland_pairs_sub), ncol = 100,
                  dimnames = list(NULL, paste0("mlat", 1:100))))
mlon.df<-data.frame(matrix(0, nrow = nrow(portland_pairs_sub), ncol = 100,
                  dimnames = list(NULL, paste0("mlon", 1:100))))

#get the walk-based routes between OD pairs - looping through each pair - Using Bing API
for (a in 1:nrow(portland_pairs_sub)) {
  tryCatch({
    orig<-paste(portland_pairs_sub[a,5],portland_pairs_sub[a,6],sep=",")
    dest<-paste(portland_pairs_sub[a,7],portland_pairs_sub[a,8],sep=",")
    w<-walking(orig,dest,BingMapsKey)
    ml<-length(w$resourceSets[[1]]$resources[[1]]$routeLegs[[1]]$itineraryItems)
        mlat.df[a,1]<-portland_pairs_sub[a,5] #add start lat
        mlon.df[a,1]<-portland_pairs_sub[a,6] #add start lon
          mlat.df[a,ml+2]<-portland_pairs_sub[a,7] #add end lat
          mlon.df[a,ml+2]<-portland_pairs_sub[a,8] #add end lon
    #loop through each maneuver storing the lat/lon. Place the maneuver lat/lon after the start lat/lon
    for (b in 1:length(w$resourceSets[[1]]$resources[[1]]$routeLegs[[1]]$itineraryItems)) {
      mlat.df[a,b+1]<-w$resourceSets[[1]]$resources[[1]]$routeLegs[[1]]$itineraryItems[[b]]$maneuverPoint$coordinates[1]
      mlon.df[a,b+1]<-w$resourceSets[[1]]$resources[[1]]$routeLegs[[1]]$itineraryItems[[b]]$maneuverPoint$coordinates[2]
    } #end maneuver loop
          
  }, error=function(e){})
} #end OD pair loop

  #convert all the lat/lon data to numeric
  mlat.df <- mutate_all(mlat.df, function(x) as.numeric(as.character(x)))
  mlon.df <- mutate_all(mlon.df, function(x) as.numeric(as.character(x)))

    #get rid of empty manuever points
    mlat.df<-mlat.df[ ,which(colSums(mlat.df)>0)]
    mlon.df<-mlon.df[ ,which(colSums(mlon.df)<0)]
       mxy.df<-cbind(mlat.df,mlon.df) #join the lat and lon maneuvers

          portland_pairs_sub<-cbind(portland_pairs_sub,mxy.df) #join the OD pair data to the maneuver lat/long data

##check to see if there are any OD pairs that share the same route
dup_routes<-which(duplicated(portland_pairs_sub) | duplicated(portland_pairs_sub, fromLast = TRUE))

##reshape the data for mapping - convert from long to wide
mlat.dfx<-cbind(portland_pairs_sub[1],mlat.df) #first, join th unique OD id to the maneuver lat data 
  mlat.dfx<-melt(mlat.dfx,"Unique.Path.ID") #reshape the data to long format using melt
    mlat.dfx<-mlat.dfx %>% arrange(desc(Unique.Path.ID,variable)) #sort by the unique ID and maneuver order
    mlat.dfx$variable<-as.character(mlat.dfx$variable) #convert the manuever variable to character in order to drop the "mlat" portion
    mlat.dfx$variable<-substring(mlat.dfx$variable, nchar(mlat.dfx$variable))
    colnames(mlat.dfx)[2:3]<-c("maneuver","mlat")
mlon.dfx<-cbind(portland_pairs_sub[1],mlon.df) #join th unique OD id to the maneuver lon data 
  mlon.dfx<-melt(mlon.dfx,"Unique.Path.ID") #reshape the data to long format using melt
    mlon.dfx<-mlon.dfx %>% arrange(desc(Unique.Path.ID,variable)) #sort by the unique ID and maneuver order
    colnames(mlon.dfx)[3]<-c("mlon") #remane "value" column to "mlon"
mlat.dfxy<-cbind(mlat.dfx,mlon.dfx[3]) #merge detailed lat data with mlon column to get complete maneuver lat/lon

mlat.dfxy<-mlat.dfxy[which(mlat.dfxy$mlat>0),] #filter our any additional bad lat/long data
mlat.dfxy<-merge(mlat.dfxy,portland_raw_od)

##Find common route points - essentially network locations that are used by more than one OD pair
mlat.dfxy.common<- as.data.frame(mlat.dfxy %>% 
  group_by(mlat,mlon) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n)))
#remove hubs
mlat.dfxy.common<-merge(mlat.dfxy.common,portland_raw_hubloc,by.x="mlat",by.y="Latitude",all.x=T)

mlat.dfxy.common<-mlat.dfxy.common[which(is.na(mlat.dfxy.common$Hub)=="TRUE"),] 
mlat.dfxy.common<-mlat.dfxy.common[,1:3]
write.csv(mlat.dfxy, file = "Portlandpoints.csv")
##optional -- plot the common points
hs<-leaflet(data = mlat.dfxy.common) %>% addTiles() %>%
  addCircleMarkers(~mlon, ~mlat, label = ~as.character(n),
                   radius = ~(n/100),
                   stroke = FALSE, fillOpacity = 0.9,
                   color="red")
hs
saveWidget(hs, file="Portland_hs.html")

xy=mlat.dfxy.common[,c(2,1)]
spdf <- SpatialPointsDataFrame(coords = xy, data = mlat.dfxy.common)

plot(spdf)

writeOGR(spdf, ".", "Portland_points_new", driver="ESRI Shapefile") #export as shapefile



##bike heat map
  ## MAKE CONTOUR LINES
    ## Note, bandwidth choice is based on MASS::bandwidth.nrd()
  setDT(mlat.dfxy)
  kde <- bkde2D(mlat.dfxy[ , list(mlon, mlat)],
                bandwidth=c(.0045, .0068), gridsize = c(100,100))
  CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

  ## EXTRACT CONTOUR LINE LEVELS
  LEVS <- as.factor(sapply(CL, `[[`, "level"))
  NLEV <- length(levels(LEVS))
  
  ## CONVERT CONTOUR LINES TO POLYGONS
  pgons <- lapply(1:length(CL), function(i)
    Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
  spgons = SpatialPolygons(pgons)
  spgons_2 =SpatialPolygonsDataFrame(spgons)
  
  writeOGR(spgons, ".", "Portland_polygons_new", driver="ESRI Shapefile") #export as shapefile
  
  ## Leaflet map with polygons and points
  hm<-leaflet(spgons) %>% addTiles() %>% 
    addPolygons(color = heat.colors(NLEV, NULL)[LEVS], fillOpacity = 0.1) 
  # %>% addCircles(lng = mlat.dfxy$mlon, lat = mlat.dfxy$mlat,
  #            radius = .05, opacity = .2, col = "blue")
  hm
  saveWidget(hm, file="Portland_hm.html")
#convert the DF into a linelist and polyline to map with leaflet
  mlat.dfxy<-as.data.frame(mlat.dfxy)
  
  mlat.dfxy1<- as.data.frame(mlat.dfxy %>% 
                                     group_by(Unique.Path.ID) %>% 
                                     summarise(n = n())) 
                                   
lines.list <- list()                 
for( i in unique(mlat.dfxy$Unique.Path.ID) ) {
  l <- list(as.matrix(mlat.dfxy[mlat.dfxy$Unique.Path.ID == i,][c(4,3)]))
  lines.list[[i]] <- SpatialLines(list(Lines(list(Line(l)), ID=as.character(i))))
}                  
my.lines <- do.call("rbind", lines.list)

plot(my.lines)

my.lines.spdf <- SpatialLinesDataFrame(my.lines, 
                                       data.frame(row.names = as.character(unique(mlat.dfxy$Unique.Path.ID)), 
                                                  ID = 1:length(my.lines)))
                                       
my.lines.spdf@data$Unique.Path.ID<-row.names(my.lines.spdf)
my.lines.spdf@data<-merge(my.lines.spdf@data,portland_raw_count)

bb=merge(my.lines.spdf@data$Unique.Path.ID,portland_raw_count)
bb<-bb[,2:3]

  #my.lines.spdf@data<-merge(my.lines.spdf@data,portland_raw_count, by=c('Unique.Path.ID'))
  
  writeOGR(my.lines.spdf, ".", "Portland_routes_new", driver="ESRI Shapefile") #export as shapefile
  
  
## leaflet generated map
r_routes <- leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>% 
  setView(-122.676317,45.522708, 12) %>% 
  addPolylines(data=my.lines.spdf,popup =~as.character(my.lines.spdf@data$n),weight=~sqrt(my.lines.spdf@data$n/5),color = "blue",fillOpacity = .4)
r_routes
saveWidget(r_routes, file="Portland_routes.html")


