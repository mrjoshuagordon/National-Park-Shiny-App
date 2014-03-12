

#libraries 
library(maps)
library(mapproj)
library(shiny)


#National Park Data Set

parks = read.csv("national_park_service.csv")

State.Abbrev = read.csv("state.csv")
# Set How Many Miles Radius You want to Search and the Minimum Population of nearby Cities
names(State.Abbrev)=c("State", "Abbreviation")


shinyServer(function(input, output) {
  
  
  
  output$main_plot  <- renderPlot({
    
    
    population.minumum  = input$population
    miles = input$distance
    
    
    
    
    #Us Cities Data and Merge in State Name for Mapping
    data(us.cities)
    us.cities1 = us.cities[us.cities$pop>=population.minumum,] 
    us.cities1 = merge(us.cities1,State.Abbrev, by.x = "country.etc", by.y="Abbreviation" )
    
    
    
    # Count the Number of National Parks within specified miles and store the parks near each city fitting the criteria 
    
    count = c()
    park.names = list()
    
    for(j in 1:nrow(us.cities1)){
      
      lat.delta = rep(us.cities1$lat[j], length(parks$Latitude))  -parks$Latitude 
      lon.delta = rep(us.cities1$lon[j] , length(parks$Longitude)) - parks$Longitude 
      
      park.distance = sqrt(( lat.delta ) ^2 + ( lon.delta )^2 ) 
      
      count[j] = length(which( park.distance  <=miles/69.172))
      
      distance.from.city = park.distance[which( park.distance  <=miles/69.172)  ]
      Miles.From.City = distance.from.city*69.172
      parks.near =  data.frame(parks[which( park.distance  <=miles/69.172),],Miles.From.City )
      # names(parks.near)[11] = "Distance from City"
      park.names[[j]] = parks.near[order(parks.near$Miles.From.City),]
      
    }
    
    names(park.names) = us.cities1$name
    
    
    
    
    
    
    # Show List of Parks for City with the most parks.  
    
    us.park = data.frame(us.cities1,count)
    us.cities1 = data.frame(us.cities1,count)
    us.park = us.park[order(us.park$count, decreasing=T),]
    row.names(us.park) = c(1:nrow(us.park))
    
    
    
    #park.names[[us.park$name[park.select]]]
    
    
    #us.park$lat
    
    park.select = input$integer
    lat.delta = rep(us.park$lat[park.select], length(us.cities1$lat))  - us.cities1$lat 
    lon.delta = rep(us.park$lon[park.select], length(us.cities1$lon))  - us.cities1$lon 
    
    city.distance = sqrt(( lat.delta ) ^2 + ( lon.delta )^2 )  
    city.to.map = us.cities1[which(city.distance<=(1.3*miles)/69.172),]
    map.input = as.vector(city.to.map$State) 
    
    
    # Region Map
    map('state', region=map.input, col="black")
    title(main = paste("Map of", "National Parks within", miles, "Miles of" ,    us.park$name[input$integer] ,  sep=" "))
    legend("topright", pch=16, col="green", legend=paste(us.park[input$integer,]$count, "Parks", sep=" "))
    
    #Parks and Park Names
    points(park.names[[us.park$name[park.select]]]$Longitude, park.names[[us.park$name[park.select]]]$Latitude, col="green", pch=16)
    
    #Central City
    points(city.to.map$lon, city.to.map$lat, col="red")
    text(city.to.map$lon, city.to.map$lat, label = city.to.map$name, cex=.7)
    
    
    
  })
  
  
  output$mytable3 = renderDataTable({
    
    
    population.minumum  = input$population
    miles = input$distance
    
    
    
    
    #Us Cities Data and Merge in State Name for Mapping
    data(us.cities)
    us.cities1 = us.cities[us.cities$pop>=population.minumum,] 
    us.cities1 = merge(us.cities1,State.Abbrev, by.x = "country.etc", by.y="Abbreviation" )
    
    
    
    # Count the Number of National Parks within specified miles and store the parks near each city fitting the criteria 
    
    count = c()
    park.names = list()
    
    for(j in 1:nrow(us.cities1)){
      
      lat.delta = rep(us.cities1$lat[j], length(parks$Latitude))  -parks$Latitude 
      lon.delta = rep(us.cities1$lon[j] , length(parks$Longitude)) - parks$Longitude 
      
      park.distance = sqrt(( lat.delta ) ^2 + ( lon.delta )^2 ) 
      
      count[j] = length(which( park.distance  <=miles/69.172))
      
      distance.from.city = park.distance[which( park.distance  <=miles/69.172)  ]
      
      Miles.From.City = distance.from.city*69.172
      parks.near =  data.frame(parks[which( park.distance  <=miles/69.172),],Miles.From.City )
      # names(parks.near)[11] = "Distance from City"
      park.names[[j]] = parks.near[order(parks.near$Miles.From.City),]
      
      
    }
    
    names(park.names) = us.cities1$name
    
    
    
    
    
    
    # Show List of Parks for City with the most parks.  
    
    us.park = data.frame(us.cities1,count)
    us.cities1 = data.frame(us.cities1,count)
    us.park = us.park[order(us.park$count, decreasing=T),]
    row.names(us.park) = c(1:nrow(us.park))
    
    
    
    #park.names[[us.park$name[park.select]]]
    
    
    #us.park$lat
    
    park.select = input$integer
    lat.delta = rep(us.park$lat[park.select], length(us.cities1$lat))  - us.cities1$lat 
    lon.delta = rep(us.park$lon[park.select], length(us.cities1$lon))  - us.cities1$lon 
    
    city.distance = sqrt(( lat.delta ) ^2 + ( lon.delta )^2 )  
    city.to.map = us.cities1[which(city.distance<=(1.3*miles)/69.172),]
    map.input = as.vector(city.to.map$State) 
    
    
    
    # names(park.names[[us.park$name[input$integer]]])[11]="Distance.From.City" 
    park.names[[us.park$name[input$integer]]][,c(2,3,4,5,6,11)] 
  }, options = list(aLengthMenu = c(5, 20, 30), iDisplayLength = 5))   
  
  ############### Cities ##################################################                               
  output$mytable2 = renderDataTable({
    
    
    population.minumum  = input$population
    miles = input$distance
    
    
    
    
    #Us Cities Data and Merge in State Name for Mapping
    data(us.cities)
    us.cities1 = us.cities[us.cities$pop>=population.minumum,] 
    us.cities1 = merge(us.cities1,State.Abbrev, by.x = "country.etc", by.y="Abbreviation" )
    
    
    
    # Count the Number of National Parks within specified miles and store the parks near each city fitting the criteria 
    
    count = c()
    park.names = list()
    
    for(j in 1:nrow(us.cities1)){
      
      lat.delta = rep(us.cities1$lat[j], length(parks$Latitude))  -parks$Latitude 
      lon.delta = rep(us.cities1$lon[j] , length(parks$Longitude)) - parks$Longitude 
      
      park.distance = sqrt(( lat.delta ) ^2 + ( lon.delta )^2 ) 
      
      count[j] = length(which( park.distance  <=miles/69.172))
      
      distance.from.city = park.distance[which( park.distance  <=miles/69.172)  ]
      
      Miles.From.City = distance.from.city*69.172
      parks.near =  data.frame(parks[which( park.distance  <=miles/69.172),],Miles.From.City )
      # names(parks.near)[11] = "Distance from City"
      park.names[[j]] = parks.near[order(parks.near$Miles.From.City),]
      
      
    }
    
    names(park.names) = us.cities1$name
    
    
    
    
    
    
    # Show List of Parks for City with the most parks.  
    
    us.park = data.frame(us.cities1,count)
    us.cities1 = data.frame(us.cities1,count)
    us.park = us.park[order(us.park$count, decreasing=T),]
    row.names(us.park) = c(1:nrow(us.park))
    
    
    
    #park.names[[us.park$name[park.select]]]
    
    
    #us.park$lat
    
    park.select = input$integer
    lat.delta = rep(us.park$lat[park.select], length(us.cities1$lat))  - us.cities1$lat 
    lon.delta = rep(us.park$lon[park.select], length(us.cities1$lon))  - us.cities1$lon 
    
    city.distance = sqrt(( lat.delta ) ^2 + ( lon.delta )^2 )  
    city.to.map = us.cities1[which(city.distance<=(1.3*miles)/69.172),]
    map.input = as.vector(city.to.map$State) 
    
    
    
    # names(park.names[[us.park$name[input$integer]]])[11]="Distance.From.City" 
    city.list = data.frame(c(1:nrow(us.park)),us.park$name, us.park$count)
    names(city.list) = c("Rank", "Name", "# of Parks")
    head(city.list,9)
  }, options = list( iDisplayLength = 9))   
  
  
  
})