library(shiny)
library(leaflet)
library(tidyverse)
library(geosphere)
library(viridisLite)
library(lubridate)


###################
lonLatTz <- function(address = NULL)
{
        if(suppressWarnings(is.null(address)))
                return(data.frame(lon = NA, lat = NA,tzone = NA, found=NA))
        # Stored values to make app start faster
        
        if(address == "Caracas, Venezuela") return(data.frame(lon = -66.9146017, lat = 10.506098, tzone = "America/Caracas", found="Caracas, Parroquia Catedral, Municipio Libertador, Distrito Capital, Venezuela"))
        if(address == "San Francisco, California") return(data.frame(lon = -122.4199061, lat = 37.7790262, tzone = "America/Los_Angeles", found="San Francisco, San Francisco City and County, California, United States of America"))
                        
        # Geolocation
        # Code borrowed from: https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
        tryCatch(
                d <- jsonlite::fromJSON( 
                        gsub("\\@addr\\@", 
                             gsub("\\s+", "\\%20", address), 
                             "http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1"
                        )
                ), error = function(c) return(data.frame(lon = NA, lat = NA, tzone = NA, found=NA))
        )
        if(length(d) == 0) return(data.frame(lon = NA, lat = NA, tzone = NA, found=NA))
        
        # Time Zone
        # See http://www.geonames.org/export/web-services.html
        tryCatch(
                tz <- jsonlite::fromJSON( 
                        gsub('\\@lon\\@',d$lon,
                             gsub('\\@lat\\@', 
                                  d$lat, 
                                  'http://api.geonames.org/timezoneJSON?lat=@lat@&lng=@lon@&username=schulercar'
                             )
                        )
                ), error = function(c) return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat),tzone = NA))
        )
        return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat),tzone = tz$timezoneId, found = d$display_name))
}

###################
mapAddresses <- function(addressList) {
        result <- sapply(addressList$address,lonLatTz)
        result <- data.frame(matrix(unlist(result),nrow=length(result)/4,ncol=4,byrow=TRUE), stringsAsFactors=FALSE)
        names(result) <- c("lng","lat","TZone","found")
        return (cbind(addressList,result))
}


###################
timeDifference <- function (here,there) {
        if(is.na(here) || is.na(there)) return(NA)
        currTime <- now()
        hereTime <-  force_tz(currTime,here)
        thereTime <- force_tz(currTime,there)
        return(hereTime - thereTime)
}

###################
makeMap <- function (addressList){
        mapData<-mapAddresses(addressList)
        mapData <- mapData %>% 
                mutate(lng = as.numeric(lng),
                       lat = as.numeric(lat))
        
        #initialize loop
        currTime <- now()
        mapData$tDiff <- currTime-currTime
        mapData$distance <- 0
        
        if(nrow(mapData)>1) {
                for(i in 1: (nrow(mapData)-1)) {
                        mapData$tDiff[i+1] <- timeDifference(mapData$TZone[i],mapData$TZone[i+1])
                        if(is.na(mapData$lng[i]) || is.na(mapData$lng[i+1])) {
                                mapData$distance[i+1] <- NA
                        } else {
                                start <- c(mapData$lng[i], mapData$lat[i])
                                end <- c(mapData$lng[i+1], mapData$lat[i+1])
                                # Calculate Distance
                                mapData$distance[i+1] <- distGeo(start,end)/1000
                        }
                        
                }
        }
        
        mapData <- mapData %>% 
                mutate(popup = paste(
                        "Location:", mapData$address,
                        "<br>Lng: ", mapData$lng,
                        "Lat:", mapData$lat,
                        "<br>Time Zone:",mapData$TZone,
                        "<br>Adjust Clock:",abs(mapData$tDiff),ifelse(mapData$tDiff<0,"hours back","hours forward")
                )
                )
        domain <- range(as.numeric(mapData$type))
        pal <- colorNumeric(palette = viridis(100), domain = domain)
        
        goodMapData <- mapData %>% filter(!is.na(lng))
        myMap <- leaflet(goodMapData) %>%
                addTiles() %>%
                addCircleMarkers(color=~pal(as.numeric(type)),
                                 popup=~popup,
                                 options = popupOptions(closeButton = FALSE))
        
        
        if(nrow(mapData)>1) {
                for(i in 1: (nrow(mapData)-1)) {
                        # Connect with Lines
                        
                        if(is.na(mapData$lng[i]) || is.na(mapData$lng[i+1])) {
                                mapData$distance[i+1] <- NA
                        } else {
                                start <- c(mapData$lng[i], mapData$lat[i])
                                end <- c(mapData$lng[i+1], mapData$lat[i+1])
                                line <- gcIntermediate(start,end, n=100, addStartEnd=TRUE, breakAtDateLine=TRUE)
                                #Deal with antimeridian artifact
                                if(!is.list(line))  {
                                        myMap <- myMap %>%  addPolylines(lng=line[,1],lat=line[,2])
                                } else {
                                        myMap <- myMap %>%  addPolylines(lng=line[[1]][,1],lat=line[[1]][,2])
                                        myMap <- myMap %>%  addPolylines(lng=line[[2]][,1],lat=line[[2]][,2])
                                }
                        }
                }
        }
        dataList <- list(mapData=mapData,myMap=myMap)
        return(dataList)
}



###################
shinyServer( function(input, output) {
        output$ofrom <- renderText({
                mapObject()[["mapData"]][1,"found"]
        })
        output$oto <- renderText({
                mapObject()[["mapData"]][2,"found"]
        })
        x1 <- eventReactive(input$goButton,{
                paste("Button was clicked",input$goButton, input$from, input$to)
        })
        output$text3 <- renderText({
                x1()
        })
        mapObject <- eventReactive(input$keyPressed,{
                addressList<-data.frame(address=c(input$from,input$to), 
                                        type=factor(c("Origin", "Destination")))
                makeMap(addressList)
        }, ignoreNULL = FALSE)
        
        output$theMap <- renderLeaflet({
                mapObject()[["myMap"]]
        })
})