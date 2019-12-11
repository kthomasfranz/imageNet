#' @title Creates a graph of a geographic network
#' @description \code{geoNet} accepts a network in the form of a set of nodes and
#' edges, where each node corresponds to an actual geographic location. It returns an
#' interactive geographic map with this network overlaid on it. It also
#' allows the user to specify other variables according that modify the color and size
#' of the nodes and edges of the network.
#' @param df This is the primary data set. It corresponds to the edges of the network.
#' It accepts an igraph or tidygraph object, or data frames or an adjacency matrices.
#' If this is an igraph or tidygraph object, the geodata parameter would be ignored
#' as all the node attributes (including the geographic data) would be extracted from
#' the object itself. It this is a data frame or and adjacency matrix, then geodata
#' cannot be NULL.
#' @param geodata This is a data frame that describes the attributes of the nodes,
#' including its geographic attributes. Default: NULL
#' @param node_color This accepts a character vector that is either a color or the
#' name of the variable amongst the node attributes that should be used to decide the
#' color of each node. The variable name should correspond to a variable that is either
#' a factor variable or is numeric. Default: 'red'
#' @param node_size This accepts either an integer or a character vector that is
#' the name of the variable amongst the node attributes that should be used to decide
#' the size of each node. The variable name should correspond to a variable that is
#' numeric in nature. Default: 3
#' @param edge_color This accepts a character vector that is either a color or the
#' name of the variable amongst the edge attributes that should be used to decide the
#' color of each edge The variable name should correspond to a variable that is either
#' a factor variable or is numeric. Default: 'navy'
#' @param edge_size This accepts either an integer or a character vector that is
#' the name of the variable amongst the edge attributes that should be used to decide
#' the width of each edge. The variable name should correspond to a variable that is
#' numeric in nature. Default: 4
#' @param name This accepts a character vector that is the name of the variable
#' amongst the node attributes that describes the name of each node. Default: 'name'
#' @param lat This accepts a character vector that is the name of the variable
#' amongst the node attributes that describes the latitude of each node. Default: 'lat'
#' @param lng This accepts a character vector that is the name of the variable
#' amongst the node attributes that describes the longitude of each node. Default: 'lng'
#' @param orig This accepts a character vector that is the name of the variable
#' amongst the edge attributes that describes one end of each edge. Default: 'origin'
#' @param dest This accepts a character vector that is the name of the variable
#' amongst the edge attributes that describes the second end of each edge. Default: 'destination'
#' @return This function returns an interactive map with the network on it. This map is of the
#' class Leaflet.
#' @details Unless users have information on the centrality measures of their network that are
#' already featured in their nodes/edges tables, if they would like to customize their geoNet graph
#' using centrality these measurements they must input their network into the baseNet function and then
#' input the tidygraph output into the geoNet function, making sure to include all node/edge specifications
#' in the argument as necessary.
#'
#' @examples
#' \dontrun{
#' data(airports)
#' data(Southwest)
#'
#' sw_nodes = unique(c(Southwest$ORIGIN, Southwest$DEST))
#' k2 = airports$AIRPORT %in% sw_nodes
#' airports=as.data.frame(airports[k2,])
#' flights<-subset(Southwest, select=c(3,4,2))
#'
#' m<-geoNet(df = flights, geodata = airports, node_color = "red", node_size = 3,
#' edge_color = 'cornflowerblue', edge_size = 4, name = "AIRPORT", lat = "LATITUDE", lng = "LONGITUDE",
#' orig = "ORIGIN", dest = "DEST")
#'
#' m
#' }
#' @rdname geoNet
#' @import dplyr
#' @import leaflet
#' @import igraph
#' @export

geoNet <- function(df, geodata = NULL, node_color = 'red',
                      node_size = 3, edge_color = 'navy', edge_size = 4, name = "name",
                      lat = "lat", lng = "lng", orig = "origin", dest = "destination"){
  library(leaflet)
  library(igraph)
  dfgeonetvis <- function(df, geodata, node_color,
                          node_size, edge_color, edge_size, name, lat, lng, orig, dest){
    names(geodata)[names(geodata) == name] <- "name"
    names(geodata)[names(geodata) == lat] <- "lat"
    names(geodata)[names(geodata) == lng] <- "lng"
    names(df)[names(df) == orig] <- "origin"
    names(df)[names(df) == dest] <- "destination"
    geodata$name <- as.character(geodata$name)
    df$origin <- as.character(df$origin)
    df$desination <- as.character(df$destination)

    m <- leaflet()
    m <- addTiles(m)

    if(!(node_color %in% names(geodata)) & !(node_size %in% names(geodata)))
    {
      for(i in 1:length(row.names(geodata)))
      {
        m <- addCircleMarkers(m, radius = node_size, lng = geodata$lng[i],
                              lat = geodata$lat[i], popup = geodata$name[i], color = node_color)

      }
    }
    else if((node_color %in% names(geodata)) & !(node_size %in% names(geodata)))
    {
      names(geodata)[names(geodata) == node_color] <- "node_color"
      if("factor" %in% class(geodata$node_color))
      {
        factpal <- colorFactor(topo.colors(length(levels(geodata$node_color))), geodata$node_color)

        for (i in 1:nrow(geodata))
        {
          m<-addCircleMarkers(m, radius = node_size, lng = geodata$lng[i],
                              lat = geodata$lat[i], color = factpal(geodata$node_color[i]))
        }
        m <- addLegend(m, "bottomright", pal = factpal, values = geodata$node_color,
                       title = node_color, opacity = 1)
      }
      else
      {
        pal <- colorNumeric(palette = "Reds", domain = geodata$node_color)
        for (i in 1:nrow(geodata))
        {
          m<-addCircleMarkers(m, radius = node_size, lng = geodata$lng[i],
                              lat = geodata$lat[i],
                              color = pal(geodata$node_color[i]))
        }
        m <- addLegend(m, "bottomright", pal = pal, values = geodata$node_color,
                       title = node_color, opacity = 1)
      }
    }
    else if(!(node_color %in% names(geodata)) & (node_size %in% names(geodata)))
    {
      names(geodata)[names(geodata) == node_size] <- "node_size"

      if(!("integer" %in% class(geodata$node_size) | "numeric" %in% class(geodata$node_size)))
      {
        stop("The node_size argument has to be a numeric variable")
      }

      minrange = range(geodata$node_size)[1]
      maxrange = range(geodata$node_size)[2]
      range = maxrange - minrange

      for(i in 1:length(row.names(geodata)))
      {
        m <- addCircleMarkers(m, radius = (geodata$node_size[i] - minrange)*(10/range) + 2 , lng = geodata$lng[i],
                              lat = geodata$lat[i], popup = geodata$name[i], color = node_color)
      }
    }
    else if((node_color %in% names(geodata)) & (node_size %in% names(geodata)))
    {
      if(node_color != node_size)
      {
        names(geodata)[names(geodata) == node_color] <- "node_color"
        names(geodata)[names(geodata) == node_size] <- "node_size"
      }
      else
      {
        names(geodata)[names(geodata) == node_color] <- "node_color"
        geodata$node_size <- geodata$node_color
      }

      if(!("integer" %in% class(geodata$node_size) | "numeric" %in% class(geodata$node_size)))
      {
        stop("The node_size argument has to be a numeric variable")
      }

      minrange = range(geodata$node_size)[1]
      maxrange = range(geodata$node_size)[2]
      range = maxrange - minrange

      if("factor" %in% class(geodata$node_color))
      {
        pal <- colorFactor(topo.colors(length(levels(geodata$node_color))), geodata$node_color)
      }
      else
      {
        pal <- colorNumeric(palette = "Reds", domain = geodata$node_color)
      }

      for (i in 1:nrow(geodata))
      {
        m<-addCircleMarkers(m, radius = (geodata$node_size[i] - minrange)*(10/range) + 2 , lng = geodata$lng[i],
                            lat = geodata$lat[i], color = pal(geodata$node_color[i]))
      }
      m <- addLegend(m, "bottomright", pal = pal, values = geodata$node_color,
                     title = node_color, opacity = 1)
    }


    if(!(edge_color %in% names(df)) & !(edge_size %in% names(df)))
    {
      for(i in 1:nrow(df))
      {
        originlat = geodata$lat[which(geodata$name == df$origin[i])]
        originlng = geodata$lng[which(geodata$name == df$origin[i])]
        destlat = geodata$lat[which(geodata$name == df$dest[i])]
        destlng = geodata$lng[which(geodata$name == df$dest[i])]
        m <- addPolylines(m, lat = c(originlat,	destlat), weight = edge_size,
                          lng = c(originlng,destlng), color = edge_color)
      }
    }
    else if(!(edge_color %in% names(df)) & (edge_size %in% names(df)))
    {
      names(df)[names(df) == edge_size] <- "edge_size"
      if(!("integer" %in% class(df$edge_size) | "numeric" %in% class(df$edge_size)))
      {
        stop("The edge_size argument has to be a numeric variable")
      }

      minrange = range(df$edge_size)[1]
      maxrange = range(df$edge_size)[2]
      range = maxrange - minrange

      for(i in 1:nrow(df))
      {
        originlat = geodata$lat[which(geodata$name == df$origin[i])]
        originlng = geodata$lng[which(geodata$name == df$origin[i])]
        destlat = geodata$lat[which(geodata$name == df$dest[i])]
        destlng = geodata$lng[which(geodata$name == df$dest[i])]
        m <- addPolylines(m, lat = c(originlat,	destlat), weight = (df$edge_size[i] - minrange)*(12/range) + 3,
                          lng = c(originlng,destlng), color = edge_color)
      }
    }
    else if((edge_color %in% names(df)) & !(edge_size %in% names(df)))
    {
      names(df)[names(df) == edge_color] <- "edge_color"
      if("factor" %in% class(df$edge_color))
      {
        factpal <- colorFactor(topo.colors(length(levels(df$edge_color))), df$edge_color)

        for (i in 1:nrow(df))
        {
          originlat = geodata$lat[which(geodata$name == df$origin[i])]
          originlng = geodata$lng[which(geodata$name == df$origin[i])]
          destlat = geodata$lat[which(geodata$name == df$dest[i])]
          destlng = geodata$lng[which(geodata$name == df$dest[i])]
          m <- addPolylines(m, lat = c(originlat,	destlat), weight = edge_size,
                            lng = c(originlng,destlng), color = factpal(df$edge_color[i]))
        }
        m <- addLegend(m, "topright", pal = factpal, values = df$edge_color,
                       title = edge_color, opacity = 1)
      }
      else
      {
        pal <- colorNumeric(palette = "Blues", domain = df$edge_color)
        for (i in 1:nrow(df))
        {
          originlat = geodata$lat[which(geodata$name == df$origin[i])]
          originlng = geodata$lng[which(geodata$name == df$origin[i])]
          destlat = geodata$lat[which(geodata$name == df$dest[i])]
          destlng = geodata$lng[which(geodata$name == df$dest[i])]
          m <- addPolylines(m, lat = c(originlat,	destlat), weight = edge_size,
                            lng = c(originlng,destlng), color = pal(df$edge_color[i]))
        }
        m <- addLegend(m, "topright", pal = pal, values = df$edge_color,
                       title = edge_color, opacity = 1)
      }
    }
    else if((edge_color %in% names(df)) & (edge_size %in% names(df)))
    {
      if(edge_color != edge_size)
      {
        names(df)[names(df) == edge_color] <- "edge_color"
        names(df)[names(df) == edge_size] <- "edge_size"
      }
      else
      {
        names(df)[names(df) == edge_color] <- "edge_color"
        df$edge_size <- df$edge_color
      }
      if(!("integer" %in% class(df$edge_size) | "numeric" %in% class(df$edge_size)))
      {
        stop("The edge_size argument has to be a numeric variable")
      }

      minrange = range(df$edge_size)[1]
      maxrange = range(df$edge_size)[2]
      range = maxrange - minrange

      if("factor" %in% class(df$edge_color))
      {
        factpal <- colorFactor(topo.colors(length(levels(df$edge_color))), df$edge_color)

        for (i in 1:nrow(df))
        {
          originlat = geodata$lat[which(geodata$name == df$origin[i])]
          originlng = geodata$lng[which(geodata$name == df$origin[i])]
          destlat = geodata$lat[which(geodata$name == df$dest[i])]
          destlng = geodata$lng[which(geodata$name == df$dest[i])]
          m <- addPolylines(m, lat = c(originlat,	destlat), weight = (df$edge_size[i] - minrange)*(12/range) + 3,
                            lng = c(originlng,destlng), color = factpal(df$edge_color[i]))
        }
        m <- addLegend(m, "topright", pal = factpal, values = df$edge_color,
                       title = edge_color, opacity = 1)
      }
      else
      {
        pal <- colorNumeric(palette = "Blues", domain = df$edge_color)
        for (i in 1:nrow(df))
        {
          originlat = geodata$lat[which(geodata$name == df$origin[i])]
          originlng = geodata$lng[which(geodata$name == df$origin[i])]
          destlat = geodata$lat[which(geodata$name == df$dest[i])]
          destlng = geodata$lng[which(geodata$name == df$dest[i])]
          m <- addPolylines(m, lat = c(originlat,	destlat), weight = (df$edge_size[i] - minrange)*(12/range) + 3,
                            lng = c(originlng,destlng), color = pal(df$edge_color[i]))
        }
        m <- addLegend(m, "topright", pal = pal, values = df$edge_color,
                       title = edge_color, opacity = 1)
      }
    }


    return(m)

  }
  if(!(is.data.frame(df) | is.matrix(df) | is.igraph(df))){
    stop("The input data has to be a data frame, a matrix, a tidygraph or an igraph")
  }
  if(is.data.frame(df)){
    if(!(is.data.frame(geodata)))
    {
      stop("If the edges data is passed as a data frame, then the nodes data
           along with geographical information should also be passed as a data frame")
    }
    m <- dfgeonetvis(df, geodata, node_color = node_color, node_size = node_size,
                     edge_color = edge_color, edge_size = edge_size, name = name,
                     lat = lat, lng = lng, orig = orig, dest = dest)
  }

  if(is.matrix(df)){
    d<-graph_from_adjacency_matrix(df)
    e <- as_data_frame(d, "edges")
    m <- dfgeonetvis(e, geodata, node_color = node_color, node_size = node_size,
                     edge_color = edge_color, edge_size = edge_size, name = name,
                     lat = lat, lng = lng, orig = "from", dest = "to")
  }

  if(is.igraph(df)){
    v<-as_data_frame(df, "vertices")
    v$name <- row.names(v)
    if(!(lat %in% names(v) & lng %in% names(v))){
      stop("Longitude and latitude must be attributes of the vertices of the igraph and the corresponding attribute names must be in the arguments")
    }
    e <- as_data_frame(df, "edges")

    m <- dfgeonetvis(df = e, geodata = v, node_color = node_color, node_size = node_size,
                     edge_color = edge_color, edge_size = edge_size, name = "name",
                     lat = lat, lng = lng, orig = "from", dest = "to")
  }

  return(m)
}



