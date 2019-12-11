context("geoNet function")
library(testthat)

test_that("errors generated from bad input", {
  data(airports)
  data(Southwest)

  sw_nodes = unique(c(Southwest$ORIGIN, Southwest$DEST))
  k2 = airports$AIRPORT %in% sw_nodes
  airports=as.data.frame(airports[k2,])
  flights<-subset(Southwest, select=c(3,4,2))

  airports$intvariable <- sample(9, size = nrow(airports), replace = TRUE)
  airports$factorvariable <- sample(4, size = nrow(airports), replace = TRUE)
  airports$factorvariable <- as.factor(airports$factorvariable)

  flights$weight <- sample(7, size = nrow(flights), replace = TRUE)


  expect_error(geoNet(), '"df" is missing')
  expect_error(geoNet(df=flights, geodata=airports, node_size = "factorvariable",
                          name = "AIRPORT", lat = "LATITUDE",lng = "LONGITUDE",
                          orig = "ORIGIN", dest = "DEST" ),
                          'The node_size argument has to be a numeric variable')
  expect_error(geoNet(df=flights, geodata=airports, edge_size = "YEAR",
                          name = "AIRPORT", lat = "LATITUDE",lng = "LONGITUDE",
                          orig = "ORIGIN", dest = "DEST" ),
               'The edge_size argument has to be a numeric variable')
  expect_error(geoNet(df=as.list(flights), geodata=airports, edge_size = "weight",
                          name = "AIRPORT", lat = "LATITUDE",lng = "LONGITUDE",
                          orig = "ORIGIN", dest = "DEST" ),
               'The input data has to be a data frame, a matrix, a tidygraph or an igraph')

})


test_that("output correct answer", {
  data(airports)
  data(Southwest)

  sw_nodes = unique(c(Southwest$ORIGIN, Southwest$DEST))
  k2 = airports$AIRPORT %in% sw_nodes
  airports=as.data.frame(airports[k2,])
  flights<-subset(Southwest, select=c(3,4,2))

  # making map without using function

  airports$intvariable <- sample(9, size = nrow(airports), replace = TRUE)
  airports$factorvariable <- sample(4, size = nrow(airports), replace = TRUE)
  airports$factorvariable <- as.factor(airports$factorvariable)

  flights$weight <- sample(7, size = nrow(flights), replace = TRUE)

  m <- leaflet()
  m <- addTiles(m)

  name = "AIRPORT"
  lat = "LATITUDE"
  lng = "LONGITUDE"
  orig = "ORIGIN"
  dest= "DEST"
  names(airports)[names(airports) == name] <- "name"
  names(airports)[names(airports) == lat] <- "lat"
  names(airports)[names(airports) == lng] <- "lng"
  names(flights)[names(flights) == orig] <- "origin"
  names(flights)[names(flights) == dest] <- "dest"

  airports$name <- as.character(airports$name)
  flights$origin <- as.character(flights$origin)
  flights$dest <- as.character(flights$dest)


  for(i in 1:length(row.names(airports))){
    m <- addCircleMarkers(m, radius = 3, lng = airports$lng[i],
                          lat = airports$lat[i], popup = airports$name[i], color = "red")

  }

  for(i in 1:nrow(flights)){
    originlat = airports$lat[which(airports$name == flights$origin[i])]
    originlng = airports$lng[which(airports$name == flights$origin[i])]
    destlat = airports$lat[which(airports$name == flights$dest[i])]
    destlng = airports$lng[which(airports$name == flights$dest[i])]
    m <- addPolylines(m, lat = c(originlat,	destlat), weight = 4,
                      lng = c(originlng,destlng), color = "navy")
  }

  #reinitializing data so we can pass it to our function
  data(airports)
  data(Southwest)

  sw_nodes = unique(c(Southwest$ORIGIN, Southwest$DEST))
  k2 = airports$AIRPORT %in% sw_nodes
  airports=as.data.frame(airports[k2,])
  flights<-subset(Southwest, select=c(3,4,2))

  airports$intvariable <- sample(9, size = nrow(airports), replace = TRUE)
  airports$factorvariable <- sample(4, size = nrow(airports), replace = TRUE)
  airports$factorvariable <- as.factor(airports$factorvariable)

  flights$weight <- sample(7, size = nrow(flights), replace = TRUE)

  k<-geoNet(df=flights, geodata=airports, name = "AIRPORT",
            lat = "LATITUDE",lng = "LONGITUDE",
            orig = "ORIGIN", dest = "DEST" )
  expect_equal(class(k), class(m))

  #Note: The network graphs' x variable values are slightly different, which means that
  #any expect_equal of expect_equivalent tests directly comparing the two graphs will fail,
  #but everything else is the same and the plots look identical
  expect_equal(k$x$options, m$x$options)
  expect_equal(k$width, m$width)
  expect_equal(k$height, m$height)
  expect_equal(k$sizingPolicy, m$sizingPolicy)
  expect_equal(k$dependencies, m$dependencies)
  expect_equal(k$elementId, m$elementId)
})

