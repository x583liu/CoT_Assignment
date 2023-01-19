#install packages
install.packages("dplyr")
install.packages("readr")
install.packages("rjson")
install.packages("ggmap")
install.packages("dplyr")
install.packages("tidyjson")
install.packages("raster")
install.packages("rgdal")
install.packages("sf")
install.packages("ggplot2")
install.packages("mapview")
install.packages("tmap")

#
library(dplyr)
library(readr)
library(ggmap)
library(rjson)
library(dplyr)
library(tidyjson)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(mapview)
library(tmap)

#Quesion 1 & 2
#read and merge tickets data
tickets <- list.files(path="C:/Users/Lumi/Desktop/data/parking-tickets", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

#remove duplicated rows
tickets <- tickets %>% distinct()
#extract/subset useful columns to a new data frame, to reduce the data size
tickets_df1 <- tickets %>% dplyr::select(infraction_code, infraction_description, location2)

#extract the infraction type and their occurrences, and the locations
##getting frequency of infraction types
tck_frqcy <- tickets_df1 %>% count(tickets_df1$infraction_code)

#CHANGE the column names
colnames(tck_frqcy) <- c('infraction_code', 'frequency')
#only keep the top 20 frequency and its corresponding infraction code,
tck_frqcy1 <- tck_frqcy %>% arrange(desc(frequency)) %>% slice(1:20)
#extract the infraction code of top 20 occurrences. 
topinfract <- as.numeric(tck_frqcy1$infraction_code)

#select rows from tickets_df1 with infraction number in tck_frqcy1
tickets_df2 <- subset(tickets_df1, infraction_code %in% topinfract)

#find the most occurrences location based on infraction code, first step is to get the No.of occr
location <- tickets_df2 %>% group_by(infraction_code) %>% count(tickets_df2$location2)
#extract the location
location1 <- location %>% group_by(location$infraction_code) %>% slice_max(n)

#map the top20 occurrences with street names
register_google(key = 'AIzaSyCFww4U_yRkJpow-Ekno8WlsmDrYS7MnKM',write = TRUE)
location_df <- mutate_geocode(location1, tickets_df2$location2)
#'185 FREDERICK ST'(-79.36962, 43.65132), and '33 COLLEGE ST' (-79.38471, 43.66104)failed to be geocoded, 
#'so manually enter the lon and lat, using https://developer.mapquest.com/documentation/tools/latitude-longitude-finder/
location_df[11, 5] <- -79.36962
location_df[11, 6] <- 43.65132
location_df[18, 5] <- -79.38471
location_df[18, 6] <- 43.66104
#find 2 invalid lat, 99 ATLANTIC AVE should be (-79.42074, 43.6388), 2 SUSSEX AVE should be (-79.39946, 43.66532)
location_df[5, 5] <- -79.39946
location_df[5, 6] <- 43.66532
location_df[8, 5] <- -79.42074
location_df[8, 6] <- 43.6388

#map the 20 locations
location_sf <- st_as_sf(location_df, coords = c("lon", "lat"), crs = 4326)
mapview(location_sf)

#read green park data
green_pk_raw <- fromJSON(file='C:/Users/Lumi/Desktop/data/green-p-parking-2019.json')

#extract the data node
green_pk <- green_pk_raw[['carparks']]
#
green_pk_df <- as.data.frame(green_pk %>% spread_all %>% dplyr::select(id, address, lat, lng))
# convert lat and lon in green_pk_df from character to numeric, for the following distance calculation
green_pk_df[,3] <- as.numeric(green_pk_df[,3])
green_pk_df[,4] <- as.numeric(green_pk_df[,4])

#calculate the distances between top 20 tickets and green parking
location_df1 = location_df
d <- pointDistance(location_df1[,5:6], green_pk_df[,4:3], lonlat=TRUE, allpairs=T) 
i <- apply(d, 1, which.min)

location_df1$id = green_pk_df$id[i]
location_df1$distance = d[cbind(1:nrow(d), i)]

# add the lat and lon of green parking to location_df1 based on the gp_id
locations <- merge(location_df1, green_pk_df, by="id")

#map two sets of points together with ggmap()
p <- ggmap(get_googlemap(center = c(lon = -79.437443, lat = 43.688841),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
p +
  geom_point(data = locations,
             aes(x = lon, y = lat.x, color = "infraction_code"), 
             alpha = 0.9) +
  geom_point(data = locations,
             aes(x = lng, y = lat.y, color = "Green Parking"),
             alpha = 0.9) 

#Question 3
#load the neighborhood shapefile
Toronto_nb <- st_read("C:/Users/Lumi/Desktop/data/Neighbourhoods - historical 140.shp")
mapview(Toronto_nb)

#read neighbourhood file csv
nb_profile <- read.csv("C:/Users/Lumi/Desktop/data/neighbourhood-profiles-2016-140-model.csv")

#the first row of nb_profile is the same as Area_code of Trt_nb
#subset nb_profile, and only extract some information under class of population 
trtnb <- nb_profile[c(1,3,5,6,8,11:15), c(5, 7:146)]

#join trtnb to the shapefile Toronto_nb
#first, rotate the trtnb
trtnb_rtt <- as.data.frame(t(trtnb))
#change the first row as column header, 
names(trtnb_rtt) <- trtnb_rtt[1,]
#then delete the 1st row:
trtnb_rtt <- trtnb_rtt[-1,]
#convert population density (5th column) in trtnb_rtt from character to number
trtnb_rtt[,5] <- as.numeric(gsub(",", "", trtnb_rtt[,5]))
#join:
trtjoin <- merge(Toronto_nb, trtnb_rtt, by.x = "FIELD_5", by.y = "Neighbourhood Number")


#map the neighbor based on density, with tmap package
# and the 20 locations in location_sf
#static mapping
tm_shape(trtjoin) + 
  tm_polygons("Population density per square kilometre", 
              style = "jenks", n = 10, palette = "Spectral",
              title = "Population Density per Square Kimometre") +
  tm_legend(outside = TRUE) +
  tm_shape(location_sf) + tm_dots(size= .3, col = "pink") 
#interactive mapping
tmap_mode("view")
tm_shape(trtjoin) + 
  tm_polygons("Population density per square kilometre", 
              style = "jenks", n = 10, palette = "Spectral",
              title = "Population Density per Square Kimometre") +
  tm_legend(outside = TRUE) +
  tm_shape(location_sf) + tm_dots(size= .3, col = "pink")
