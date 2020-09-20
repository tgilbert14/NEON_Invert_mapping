##mapping lat/long data
library(neonUtilities)  ##Loading Libraries##
library(tidyverse)
library(ggmap)
library(tidyr)
library(RColorBrewer)
library(leaflet)
library(mapview)
library(plyr)
library(htmlwidgets)

site<- 'SYCA'
#Inverts
dpid<- "DP1.20120.001"

#Downloading NEON portal data since 2017 to present w/ dpID
raw <- loadByProduct(dpID = dpid, site = site, startdate = '2017-01', package = 'basic', check.size = 'F' )
data.coord<- as_tibble(raw$inv_fieldData)
#View(data.coord)
data<- data.coord %>% 
  select(sampleID, decimalLongitude,decimalLatitude) %>% 
  filter(!is.na(sampleID)) %>% 
  filter(!is.na('1'))
#head(data)
colnames(data)[2]<- 'Longitude'
colnames(data)[3]<- 'Latitude'
#head(data)
data$plotID <-  str_sub(data$sampleID, 15, 30) 

data<- data %>% 
  select(plotID, Longitude, Latitude) %>% 
  filter(!is.na(""))
#head(data)

data.raw <- as_tibble(raw$inv_taxonomyProcessed)    #Getting raw data
data.raw$plotID <-  str_sub(data.raw$sampleID, 15, 30) 
#View(data.raw)

species<- data.raw$scientificName
species<- unique(species)
#species

plot<- data.raw$plotID
plot<- unique(plot)
#plot

mylist<- list()

x=1
while(x< length(plot)+1) {
i=1
per_sp<- data.raw %>% 
  select(plotID, scientificName, individualCount) %>% 
  filter(scientificName == species[i]) %>% 
  filter(plotID == plot[x]) %>% 
  mutate(totalCount = sum(individualCount)) %>%
  select(plotID, scientificName, totalCount)
mylist[[x]]<- unique(per_sp)
#mylist[[x]]

i=2
while(i< length(species)+1) {
per_sp<- data.raw %>% 
  select(plotID, scientificName, individualCount) %>% 
  filter(scientificName == species[i]) %>% 
  filter(plotID == plot[x]) %>% 
  mutate(totalCount = sum(individualCount)) %>%
  select(plotID, scientificName, totalCount)
a<- unique(per_sp)
mylist[[x]]<- union(mylist[[x]], a)
i=i+1
}

if (x == 1) {mydata<- mylist[[1]]}

mydata<- union(mylist[[x]],mydata)
x=x+1

}

#View(mydata)




#Sample points- 
#plot[1]  #s.1
#plot[13] #s.2
#plot[6]  #s.3
#plot[14] #s.4
#plot[2]  #s.5
#plot[9]  #s.6
#plot[7]  #s.7
#plot[3]  #s.8

#plot[12] #c.1
#plot[4]  #c.2
#plot[11] #c.3
#plot[5]  #c.4
#plot[8]  #c.6
#plot[10] #c.8


geo_per_plot <- left_join(mydata, data, by = 'plotID')
#View(geo_per_plot)

##For each species captured per plot
new_geo<- unique(geo_per_plot)

##SYCA reach coordinates-- plot location estimation
##Reach Markers from downstream to up##
B.long<- -111.508692
B.lat<- 33.749033

R1.long<- -111.508122
R1.lat<- 33.748902

R2.long<- -111.507101
R2.lat<- 33.748306

R3.long<- -111.506717
R3.lat<-   33.749106

R4.long<- -111.507325
R4.lat<- 33.749875

R5.long<- -111.507921
R5.lat<- 33.750538

S2.long<- -111.508124
S2.lat<- 33.750886

R6.long<- -111.508489
R6.lat<- 33.751257

S1.long<- -111.508597
S1.lat<- 33.751703

R7.long<- -111.508647
R7.lat<- 33.752254

R8.long<- -111.507854
R8.lat<- 33.753151

R9.long<- -111.506881
R9.lat<- 33.753434

R10.long<- -111.506025
R10.lat<- 33.753788

T.long<- -111.50559
T.lat<- 33.754084
#---

B<- new_geo %>% 
  filter(plotID == plot[1]) %>% 
  mutate(Longitude = B.long) %>% 
  mutate(Latitude = B.lat)

R1<- new_geo %>% 
  filter(plotID == plot[12]) %>% 
  mutate(Longitude = R1.long) %>% 
  mutate(Latitude = R1.lat)
new_data<- union(B,R1)

R2<- new_geo %>% 
  filter(plotID == plot[13]) %>% 
  mutate(Longitude = R2.long) %>% 
  mutate(Latitude = R2.lat)
new_data<- union(R2,new_data)

R3<- new_geo %>% 
  filter(plotID == plot[4]) %>% 
  mutate(Longitude = R3.long) %>% 
  mutate(Latitude = R3.lat)
new_data<- union(R3,new_data)

R4<- new_geo %>% 
  filter(plotID == plot[6]) %>% 
  mutate(Longitude = R4.long) %>% 
  mutate(Latitude = R4.lat)
new_data<- union(R4,new_data)

R5<- new_geo %>% 
  filter(plotID == plot[11]) %>% 
  mutate(Longitude = R5.long) %>% 
  mutate(Latitude = R5.lat)
new_data<- union(R5,new_data)

S2<- new_geo %>% 
  filter(plotID == plot[14]) %>% 
  mutate(Longitude = S2.long) %>% 
  mutate(Latitude = S2.lat)
new_data<- union(S2,new_data)

R6<- new_geo %>% 
  filter(plotID == plot[5]) %>% 
  mutate(Longitude = R6.long) %>% 
  mutate(Latitude = R6.lat)
new_data<- union(R6,new_data)

S1<- new_geo %>% 
  filter(plotID == plot[2]) %>% 
  mutate(Longitude = S1.long) %>% 
  mutate(Latitude = S1.lat)
new_data<- union(S1,new_data)

R7<- new_geo %>% 
  filter(plotID == plot[9]) %>% 
  mutate(Longitude = R7.long) %>% 
  mutate(Latitude = R7.lat)
new_data<- union(R7,new_data)

R8<- new_geo %>% 
  filter(plotID == plot[8]) %>% 
  mutate(Longitude = R8.long) %>% 
  mutate(Latitude = R8.lat)
new_data<- union(R8,new_data)

R9<- new_geo %>% 
  filter(plotID == plot[7]) %>% 
  mutate(Longitude = R9.long) %>% 
  mutate(Latitude = R9.lat)
new_data<- union(R9,new_data)

R10<- new_geo %>% 
  filter(plotID == plot[3]) %>% 
  mutate(Longitude = R10.long) %>% 
  mutate(Latitude = R10.lat)
new_data<- union(R10,new_data)

T1<- new_geo %>% 
  filter(plotID == plot[10]) %>% 
  mutate(Longitude = T.long) %>% 
  mutate(Latitude = T.lat)
new_data<- union(T1,new_data)

new_geo<- new_data

nb.cols <- length(new_geo$scientificName)
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

pal <- colorFactor(
  palette = mycolors,
  domain = new_geo$scientificName)

#View(per_sp)    

###Breakdown of species--
# Prepare the text for the tooltip:
mytext2 <- paste(
  "ScientificName: ", new_geo$scientificName, "<br/>", 
  "Count: ", new_geo$totalCount, "<br/>",
  "PlotID: ", new_geo$plotID, "<br/>", 
  "Long: ", new_geo$Longitude,"<br/>",
  "Lat: ", new_geo$Latitude, sep="") %>%
  lapply(htmltools::HTML)
meanLong<- mean(data$Longitude)
meanLat<- mean(data$Latitude)

m <- leaflet(new_geo) %>% 
  addTiles()  %>% 
  setView( lat=meanLat, lng=meanLong , zoom=16) %>%
  addProviderTiles("TomTom.Hybrid") %>%
  addCircleMarkers(~Longitude, ~Latitude,
                   fillColor =~pal(new_geo$scientificName), opacity = .7, fillOpacity = .7, radius=~totalCount/6, popup = new_geo$plotID, stroke = T, weight = 1,  color = 'white', 
                   label = mytext2,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>% 
  addLegend( pal=pal, values=~new_geo$scientificName, opacity=0.6, title = "Species Diversity per Plot", position = "bottomright" )

m 

saveWidget(m, file="SYCA_inverts_map.html")
