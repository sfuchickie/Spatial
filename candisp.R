#
#
#    Cannabis dispensaries
#    Supervision: Dr. M J Milloy
#
#    Saida Amirova 
#    May 28, 2015
#    
#
#
###############################################################
###############################################################

#rm(list=ls())
setwd("\\Users\\Maimizo\\Desktop\\CANDISP")


## Load spatial packages

library(dplyr)
library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
#library(spgwr)        ## GWR
library(rgdal)
library(ggmap)
library(plotrix)
library(spatstat)
library(splancs)
localDir <- '\\Users\\Maimizo\\Desktop\\CANDISP'

#list.files(localDir)

# Below csv are conversions from kml to csv using kmlcsv converter

list.files()
candisp <- read.csv('dispfromkml.csv',header=TRUE,strip.white=TRUE,as.is=TRUE)
schools <- read.csv('schools-elm-high.csv',header=TRUE,as.is=TRUE,
                    strip.white=TRUE,fill=TRUE)
crimes  <- read.csv('crimes.csv',header=TRUE,as.is=TRUE,
                    strip.white=TRUE,fill=TRUE)
beer    <- read.csv('cold.csv',header=TRUE,as.is=TRUE,
                    strip.white=TRUE,fill=TRUE)
income  <- read.csv('income2006.csv',header=TRUE,as.is=TRUE,
                    strip.white=TRUE,fill=TRUE)
postal  <- read.csv('postalpoly.csv',header=TRUE,as.is=TRUE,
                    strip.white=TRUE,fill=TRUE)
names(income)
colnames(income)[1] <- "CFSAUID"
colnames(income)[2] <- "Total.pop"
income<-income[-1,]

beer[,4]<- NULL
crimes$TYPE <- as.factor(crimes$TYPE)

subinc  <- income[,c(1,2,3,4,5,8,32,38,42)]


## Projection: UTM Zone 10
## Note: To use SpatialPointsDataFrame in ggplot, 
#  geom_point(df@data,aes(x,y...)) argument has to be used to avoid an error
## Shape files Enhanced POI for BC had to be redownloaded for 2010
## POLICE only had RCMP locations 
shp     <- readOGR(dsn=".",layer="BritishColumbiaGAF")
nhb     <- readOGR(dsn=".",layer="CANnbh")
bcpoi   <- readOGR(dsn=".",layer="BC")
sky     <- readOGR(dsn=".",layer="SkyTStation")
line    <- readOGR(dsn=".",layer="SkyTline")
cansky  <- readOGR(dsn=".",layer="CanLine_Stn")
canline <- readOGR(dsn=".",layer="CanLine")
vanCSD  <- readOGR(dsn=".",layer="201203_VANCOUVER_CSD")
vangaf  <- readOGR(dsn=".",layer="MetroVanDissBloxWithGAF")
inc     <- readOGR(dsn=".",layer="MetroVancDA_MedIncome")
postshp <- readOGR(dsn=".",layer="gfsa000b11a_e")
busb    <- readOGR(dsn=".",layer="BusStops_09March2010")


postshp$CFSAUID <- as.character(postshp$CFSAUID) # THIS IS A MUST FOR PROPER SUBSET
busb            <- spTransform(busb, CRS("+proj=longlat +datum=WGS84"))
#Subsetting for vancouver only NOTE: WATCH OUT FOR UPPER AND LOWER CASE CITY NAME
vancity   <- subset (shp,shp$CSDNAME       %in% "Vancouver")
prov      <- subset (nhb,nhb$PROV          %in% "BC")
city      <- subset (prov,prov$CPC_NAME    %in% "Vancouver")
city2     <- subset (bcpoi,bcpoi$CITY      %in% "VANCOUVER")
police    <- subset (city2,city2$EPOI_TYPE %in% "POL")
hcr       <- subset (city2,city2$EPOI_TYPE %in% "HCR")
subvanCSD <- subset (vanCSD,vanCSD$NAME    %in% "Vancouver")
liq       <- subset (city2,city2$SIC_1     %in% "59210000")
vanpost   <- postshp[grepl("^V", postshp$CFSAUID),] # CFSAUID has to be character class not factor

bus    <- subset (busb,busb@coords[,2] < 49.35 & busb@coords[,2] > 49.212 & busb@coords[,1] > -123.24 & busb@coords[,1] < -123.00)


table(vanpost$CFSAUID)
# Looking at the postal match

table(subinc$CFSAUID %in% vanpost$CFSAUID)
table(vanpost$CFSAUID)

# Merging for postal and income 

postalincome <- merge(vanpost, subinc,by='CFSAUID', all = TRUE)
incpost.f    <- fortify(postalincome,region = "CFSAUID")
incpost.f    <- merge(incpost.f, postalincome,by.x="id",by.y="CFSAUID")


names(postalincome)[11] <-"Median.after.tax.inc"
names(incpost.f)[17] <-"Median.after.tax.inc"

skysp                <- spTransform(sky, CRS("+proj=longlat +datum=WGS84"))
cansky               <- spTransform(cansky, CRS("+proj=longlat +datum=WGS84"))

# Getting area for the city neighborhoods watch out for data conversion
# between NAD83 and WGS84. NAD83 is used by US agencies WGS84 is used by
# Northwest pacific
proj4string(city) <- CRS("+proj=longlat +datum=WGS84")
cityproj          <- spTransform(city, CRS("+init=epsg:32610"))
res1              <- lapply(slot(cityproj, "polygons"), function(x)
                            sapply(slot(x, "Polygons"), slot, "area")/1000000)
sum(unlist(res1)) # Area of vancouver is ~ 115km^2 as per google search as well

res2              <- lapply(slot(city,"polygons"),function(x)
                      sapply(slot(x,"Polygons"),slot,"coords"))
res2


proj                 <- CRS("+proj=utm +zone=10 +datum=WGS84")
sp_crime             <- cbind(crimes$lon,crimes$lat)
colnames(sp_crime)   <- c("LONG","LAT")
crimes.sp            <- SpatialPointsDataFrame(coords=sp_crime,crimes,proj4string=proj)


sp_schools           <- cbind(schools$lon,schools$lat)
colnames(sp_schools) <- c("LONG","LAT")
schools.sp           <- SpatialPointsDataFrame(coords=sp_schools,schools,proj4string=proj)

sp_candisp           <- cbind(candisp$lon,candisp$lat)
colnames(sp_candisp) <- c("LONG","LAT")
candisp.sp           <- SpatialPointsDataFrame(coords=sp_candisp,candisp,proj4string=proj)

#den          <- postalincome$CFSAUID/(income$CFSAUID %in% "BC")
#str(bcpoi$EPOI_TYPE)
#str(city2$SIC_1)
#table(city2$SIC_1 %in% "59210000")
#str(vangaf)
#table(is.na(new_vangaf$uaname))
#new_vangaf <- vangaf[!is.na(vangaf$uaname),]
#table(new_vangaf$uaname)
#vangaf.sub <- subset(vangaf,vangaf$uaname %in% "Vancouver")    


## Getting density of Vancouver area

density <- function(df,polygons,Total.pop,area,CFSAUID){
  n <- length(df@polygons)
  nah <- NULL
  wh <- NULL
  for (i in 1:n){
    nah[i] <- df@data$Total.pop[i]/df@polygons[[i]]@area
    wh [i] <- df@data$CFSAUID[i]
  }
  
  newb<-as.data.frame(cbind(nah,wh))
  colnames(newb)<-c("density","CFSAUID")
  return(newb)
}
parea <- density(postalincome,polygons,Total.pop,area,CFSAUID)
parea$density <- as.integer(parea$density)
incpost.f    <- merge(incpost.f, parea,by.x="id",by.y="CFSAUID")

#Plotting

vanloc      <- geocode("Vancouver,BC")

vanmap      <- get_map(c(lon=vanloc$lon, lat=vanloc$lat),zoom = 12, maptype = "toner", source = "google")
vanmapped   <- ggmap(vanmap)
vanmapped

vanmapped1  <- vanmapped + geom_polygon(aes(x=long, y=lat, group=group), fill='red', 
                                       size=.05,color='red', data=vancity, alpha=0)
vanmapped1

vannhb      <- vanmapped + geom_polygon(aes(x=long, y=lat, group=group), fill='red', 
                                       size=.5,color='red', data=city, alpha=0)
vannhb

dispmap     <- vanmapped + geom_point(data=candisp,aes(x=lon, y=lat,color="Dispensary"))+
                scale_color_manual("POI",values=c('Dispensary'="green"))
dispmap


# Postal map with income 

postmap     <- vanmapped + geom_polygon(aes(x=long,y=lat), fill='red', 
                                  size=.5,color='red', data=vanpost, alpha=0)
postmap

incmap      <- vanmapped+geom_polygon(data=incpost.f,aes(x=long,y=lat,fill=Median.after.tax.inc,group=group,map_id=id))+ 
               labs(x = "latitude", y = "longitude",fill="Median income")+
               ggtitle("Income by postal code")
 
incmap 


gradient.income <- incmap  +scale_fill_gradient(low = "white",high = "black",limits=c(10000,70000))
  
gradient.income

max(parea$density,na.rm=TRUE)
#Prep data for plotting 

den.plot      <- vanmapped + geom_polygon(data=incpost.f,aes(x=long,y=lat, fill=density,group=group,map_id=id))+
                labs(x = "latitude", y = "longitude",fill="Density")+
                 ggtitle("Population density \n by postal code")
den.plot

gradient.density <- den.plot  +scale_fill_gradient(low = "white",high = "black",limits=c(0,200))

gradient.density
library(gridExtra)
grid.arrange(gradient.density, gradient.income, ncol=2)

ggsave('gradient.income.png',width=6,height=4,dpi=600)


# Plotting vannhb doesnt produce strange polygons in the South but below
# map produces strange triangles
dispmap2     <- vannhb + geom_point(data=candisp.sp@data,aes(x=lon, y=lat))
dispmap2

school.disp  <- dispmap + geom_point(data=schools,aes(x=lon,y=lat,color="School"))+
                  ggtitle("Locations of Can.dispensaries \n and schools in Vancouver")+
                  scale_color_manual("POI",values=c('Dispensary'="green",'School'="red"))
school.disp

#all2 <- dispmap + geom_point(data=schools,aes(x=lon,y=lat),color="blue")+
#        ggtitle("Locations of Can.dispensaries \n and schools in Vancouver")
#all2

all3  <- all  + geom_point(data=skysp@data,aes(x=skysp$coords.x1,y=skysp$coords.x2),color="green")
all3

all4  <- all3 + geom_point(data=cansky@data,aes(x=cansky$coords.x1,y=cansky$coords.x2),color="green")
all4

#Police
pol  <- vanmapped + geom_point(data=police@data,aes(x=police$X,y=police$Y),color="red")
pol

#Hospitals
hos  <- vanmapped + geom_point(data=hcr@data,aes(x=hcr$X,y=hcr$Y),color="orange",size=1)
hos

#Hospitals and police
hosandpol <- vanmapped + geom_point(data=hcr@data,aes(x=hcr$X,y=hcr$Y),color="orange",size=3)+
             geom_point(data=police@data,aes(x=police$X,y=police$Y),color="red",size=3)

hosandpol


#Plot dispensaires hospitals and police stations NOTE: SOME ROWS REMOVED (NOT SURE WHY)
hospoldisp <- vanmapped + geom_point(data=hcr@data,aes(x=hcr$X,y=hcr$Y),color="orange",size=3)+
              geom_point(data=police@data,aes(x=police$X,y=police$Y),color="red",size=3)+
              geom_point(data=candisp.sp@data,aes(x=lon, y=lat),color="dark green")

hospoldisp

#CRIMES

myColors <- brewer.pal(7,"Set1")
names(myColors) <- levels(crimes$TYPE)
colScale <- scale_colour_manual(name = "Type",values = myColors)

crime.den  <- vanmapped + geom_density2d(data=crimes,aes(x=crimes$lon,y=crimes$lat))
crime.den


den.blah   <- vanmapped + stat_density2d(aes(x = lon, y = lat, 
                                 fill = ..level.. , alpha = ..level..),size = 2, bins = 4, 
                                 data = crimes, geom = 'polygon')+ 
              scale_fill_gradient('Crime\nDensity') +
              scale_alpha(range = c(.4, .75), guide = FALSE) +
              guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
den.blah
crime.poi  <- vanmapped + geom_point(data=crimes,aes(x=crimes$lon,y=crimes$lat,color=TYPE))
crime.poi



crime.sz   <- vanmapped + geom_point(data=crimes,aes(x=crimes$lon,y=crimes$lat,color=TYPE,size=TYPE))
crime.sz
crime.poi1 <- crime.poi + colScale

# Police (municipal and RCMP) and dispensaries overlay of crime density

polcrimedisp <- crime.den + geom_point(data=police@data,aes(x=police$X,y=police$Y,color="Police"))+
                geom_point(data=candisp.sp@data,aes(x=lon, y=lat, colour="Dispensary"))+
                ggtitle("Crime density overlay over police \n and dispensary locations")+
                scale_colour_manual(name='', 
                                    values=c('Police'='red', 'Dispensary'='green'),guide='legend')


polcrimedisp


#NOTE: Need  to look into Cold Beer and Wine as well.
crime.alc.pol.dp  <- crime.den + geom_point(data=liq@data,aes(x=liq$X,y=liq$Y,color="Liquor store"),size=3)+
                     geom_point(data=candisp.sp@data,aes(x=lon, y=lat,colour="Dispensary"),size=3)+
                     geom_point(data=police@data,aes(x=police$X,y=police$Y,color="Police"),size=3)+
                     scale_colour_manual(name='', 
                     values=c('Police'='red', 'Dispensary'='green', "Liquor store"="orange"),guide='legend')

crime.alc.pol.dp 

                     

col <- findColours(classIntervals(
  inc$MedIncome, 100, style="quantile"),
  smoothColors("#FFFFD7",98,"#F3674C"))


leg <- findColours(classIntervals(
  round(inc$MedIncome,0), 4, style="quantile"),
  smoothColors("#FFFFD7",2,"#F3674C"),
  under="under", over="over", between="-", 
  cutlabels=FALSE)

## Looking at distances

or.disp <- matrix(c(candisp$lat, candisp$lon), ncol=2)
dest.school <- matrix(c(schools$lat, schools$lon), ncol=2)

# Looking at minimum distance between dispensary as origin and destination of interest
# as destination 

origin <- matrix(c(canOne$lat, canOne$lon), ncol=2)
destination <- matrix(c(subSC$lat, subSC$lon), ncol=2)
canOne <- candisp[1,]
subSC <- schools[1:3,]
canOne <- matrix(canOne,ncol=2)

#The origin has to be longer than the destination EX: if dispensary has 88 obs and school only 80, you want
# to go through all disp and look at distance to school to pick min distance
# if there is only 10 police stations  you want to go through all dispensaries and look at 
minDistance <- function(origin, destination) {
  ndestin  <- nrow(destination)
  norigin  <- nrow(origin)
  if(nrow(destination) < nrow(origin)) stop("# of obs. in origin has to be greater than destination for the correct results", call. = F)

  dest     <- data.frame(2,1:norigin)
  disp     <- data.frame(2,1:norigin)
  distance <- data.frame(1:norigin)
  km       <- data.frame(1:norigin)
  blah     <- data.frame(1:norigin)
  for(i in 1:ndestin){
    # loops through the coordinates and finds minimum distance
    blah     <- spDistsN1(origin, destination[i,], longlat=TRUE)
    km[i,]    <- min(blah) 
    where    <- which.min(blah)
    disp[i,] <- origin[where,]
    dest[i,] <- destination[i,]
  }
  #browser()
  newNames <- c("km","latd","lond","lato","lono")
  # browser()
  distance <- cbind(km,dest,disp)
  names(distance) <- newNames
  return(data.frame(distance))
}
sch.disp.dist <-minDistance(origin=dest.school,destination=or.disp)
newb <-minDistance(origin=or.disp,destination=dest.school)
str(sch.disp.dist)
names(sch.disp.dist)

#Transforming into spatial points dataframe
sp_close.sch       <- cbind(sch.disp.dist$lond,sch.disp.dist$latd)
colnames(sp_close.sch) <- c("lond","latd")
sp_close.sch <- SpatialPointsDataFrame(coords=sp_close.sch,sch.disp.dist,proj4string=proj)

sp_close.sch.disp       <- cbind(sch.disp.dist$lono,sch.disp.dist$lato)
colnames(sp_close.sch.disp) <- c("lono","lato")
sp_close.sch.disp <- SpatialPointsDataFrame(coords=sp_close.sch.disp,sch.disp.dist,proj4string=proj)



close.sch.disp <- vanmapped + geom_point(data=sp_close.sch@data,aes(x=lond, y=latd,colour="School"),size=2)+
                  geom_point(data=sp_close.sch.disp@data,aes(x=lono, y=lato,colour="Dispensary"),size=2)+
                  scale_colour_manual(name='POI', 
                      values=c('School'='red', 'Dispensary'='green'),guide='legend')+
                  ggtitle("Closest proximity schools and \n dispensaries")+
                  stat_density2d(sp_close.sch.disp@data,aes(x = lono, y = lato, 
                     fill = ..level.. , alpha = ..level..),size = 2, bins = 4, 
                 data = crimes, geom = 'polygon') 

close.sch.disp


grid.arrange(school.disp, close.sch.disp, ncol=2)
 ## Transforming all the data into Spatial points data frame
 ## Namely: dispensaries, schools, police, hospitals, liquor, income


## NOW FOR SPATIAL ANAlYSIS
## Heterogeneous Poissson, poisson cluster, Markov
## point process geterogenous Ppoisson proccess
## Knox method and k function
## Playing around with examples
summary(candisp)
class(candisp.sp[,1])
disp.patter <- ppp(candisp[,1], candisp[,2], c(-123.20,-123.03), c(49.19,49.32))
plot(disp.patter)
plot(Kest(disp.patter))


school.pattern <- ppp(school)


spdisp <- elide(candisp.sp, scale = TRUE)
sppol  <- elide(police, scale = TRUE)
spsch  <- elide(schools.sp, scale = TRUE)

r <- seq(0, sqrt(2)/6, by = 0.005)
envdisp <- envelope(as(spdisp, "ppp"), fun = Gest,r = r, nrank = 2, nsim = 99)
envschool <- envelope(as(spsch, "ppp"), fun = Gest, r = r, nrank = 2, nsim = 99)
envpolice <- envelope(as(sppol, "ppp"), fun = Gest,r = r, nrank = 2, nsim = 99)
Gresults <- rbind(envdisp, envschool, envpolice)
Gresults <- cbind(Gresults, DATASET = rep(c("disp","school", "pol"), each = length(r)))


# Point Pattern Processes: Complete Spatial Randomness
# Homogeneous poisson process
candisp_ppp <- as(candisp.sp,"ppp")

myPoly <- spTransform(city, CRS("+init=epsg:4121")) 
proj4string(myPolygonSpatial) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
gArea(myPoly)


flbord <- city@polygons[[1]]@Polygons[[1]]@coords
str(city)

flinvxy<-coordinates(candisp.sp)


flbdry<-as(city,"owin")
flppp<-ppp(candisp_ppp$x,candisp_ppp$y,window=flbdry)

plot(flppp, axes=T)
qt <- quadrat.test(flppp,nx = 10, ny = 10)
plot(qt, add = TRUE, cex =.5)


mse<-mse2d(flinvxy,flbord, -150, 150)
plot(mse$h, mse$mse, xlab="Bandwidth", ylab="MSE",type="l", xlim=c(100,600), ylim=c(-30,50))
