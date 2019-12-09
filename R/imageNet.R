#' @title Creates an image of network in R
#' @description \code{imageNet} helps provide users with clear, concise visualizations of
#' their networks so that they can perform effective analysis.
#' @param data A network object that can be converted to a tidygraph tbl_graph
#' (acceptable classes include 'data.frame', 'igraph', 'list', 'matrix', and 'network').
#' See ?tbl_graph for more information about accepted classes of networks.
#' @param nodes A data.frame containing information about the nodes in the graph, Default: NULL
#' @param layout The type of layout used to plot the network using ggraph. Either a valid string, a function, a matrix,
#' or a data.frame. See ?ggraph for more information. Default: 'lgl'
#' @param label The variable from the nodes table that should be used to label the nodes in
#' the plotted network.  Default: first column of the nodes table
#' @param directed 	Indicates if the constructed graph should be directed, Default: F
#' @param node_color PARAM_DESCRIPTION, Default: "cornflowerblue"
#' @param node_size PARAM_DESCRIPTION, Default: 8
#' @param edge_color PARAM_DESCRIPTION, Default: "slategrey"
#' @param edge_size PARAM_DESCRIPTION, Default: 0.6
#' @return list containing data in the form of a tidygraph & ggmap of network
#' @details The \code{imageNet} function converts the user's network into a \code{tidygraph} object
#' and plots the network using \code{ggmap}. In order to operate this fucntion effectively,
#' it is expected that the user has an elementary understanding of network analysis and knows how
#' to perform a basic evaluation of some type of class of networks
#' (data.frame, igraph, network, etc.) in R.
#' @import dplyr
#' @import tidygraph
#' @import igraph
#' @import ggraph
#' @import ggthemes
#' @import ggplot2
#' @import readr
#' @import igraphdata
#' @examples
#' library(igraphdata)
#' data("karate")
#' imageNet(karate, label=name, layout="nicely")
#' @rdname imageNet
#' @export

imageNet<-function(data, nodes=NULL, layout = "lgl", label, directed=F, node_color,
                   node_size, edge_color, edge_size){
  title<-paste0("Network Graph of ", deparse(substitute(data)))
  title<-paste0(title, " data")

  if(is.data.frame(data)){
    data = tbl_graph(edges=data, nodes=nodes, directed = directed)
  }
  if(is.igraph(data)){
    data = as_tbl_graph(data, directed = directed)
  }
  else{
    data = as_tbl_graph(data, directed = directed)
  }

  label_test<-deparse(substitute(label))
  node_s_test<-deparse(substitute(node_size))
  node_c_test<-deparse(substitute(node_color))
  edge_s_test<-deparse(substitute(edge_size))
  edge_c_test<-deparse(substitute(edge_color))

  ##Add Centrality Measures to Nodes/Edges Tables
  df<-data%>%
    activate(edges)
  df<-as_tibble(df)%>%
    group_by(from, to)%>%
    summarise(edge_weight=n())%>%
    ungroup()
  data<-data%>%
    activate(edges) %>%
    left_join(df, by = c("from", "to"))
  data<-data%>%
    activate(nodes)%>%
    mutate(betweenness = centrality_betweenness(directed=directed)) %>%
    mutate(closeness = centrality_closeness())%>%
    mutate(degree=centrality_degree())%>%
    mutate(eigen=centrality_eigen(directed=directed))

  ##Check for Appropriate Edge and Node Inputs
  test_df<-data%>%
    activate(nodes)%>%
    as_tibble()
  if (!missing(label)){
    if (!is.character(substitute(label))){
      if (is.null(test_df[[label_test]])){
        stop("Label error: your label must be a character vector or the name of a variable from the nodes table")
      }
    }
  }
  if(!missing(node_color)){
    if (!is.character(substitute(node_color))){
      if (is.null(test_df[[node_c_test]])){
        stop("Node color error: your input for node color must be a character vector or the name of a variable from the nodes table")
      }
    }
  }
  if(!missing(node_size)){
    if (!is.numeric(substitute(node_size))){
      if (is.null(test_df[[node_s_test]])){
        stop("Node size error: your input for node size must be an integer or the name of a variable from the nodes table")
      }
    }
  }
  if(!missing(edge_color)){
    if (!is.character(substitute(edge_color))){
      if (is.null(df[[edge_c_test]])){
        stop("Edge color error: your input for node color must be a character vector or the name of a variable from the edges table")
      }
    }
  }
  if(!missing(edge_size)){
    if (!is.numeric(substitute(edge_size))){
      if (is.null(df[[edge_s_test]])){
        stop("Edge size error: your input for node size must be an integer or the name of a variable from the edges table")
      }
    }
  }

  ##Node Customization
  node_c<-enquo(node_color)
  node_s<-enquo(node_size)

  if (missing(node_color) & missing(node_size)){
    point <- function() geom_node_point(color="cornflowerblue", size=8)
  }
  else if (missing(node_color) & !missing(node_size)){
    if (is.numeric(substitute(node_size))){
      point <- function() geom_node_point(size=node_size, color="cornflowerblue")
    }else{
      point <- function() geom_node_point(aes(size=!!node_s), color="cornflowerblue")
    }
  }
  else if (!missing(node_color) & missing(node_size)){
    if (is.character(substitute(node_color))){
      point <- function() geom_node_point(color=node_color, size=8)
    }else{
      point <- function() geom_node_point(aes(color=!!node_c), size=8)
    }
  }
  else if (is.character(substitute(node_color)) & is.numeric(substitute(node_size))){
    point <- function() geom_node_point(size=node_size, color=node_color)}
  else if (is.character(substitute(node_color)) & !is.numeric(substitute(node_size))){
    point <- function() geom_node_point(aes(size=!!node_s), color=node_color)}
  else if (!is.character(substitute(node_color)) & is.numeric(substitute(node_size))){
    point <- function() geom_node_point(aes(color=!!node_c), size=node_size)
  }
  else{
    point <- function() geom_node_point(aes(color=!!node_c, size=!!node_s))
  }

  ##Edge Customization
  edge_c<-enquo(edge_color)
  edge_s<-enquo(edge_size)

  if (directed==T){
    edge_arrow=arrow(angle = 15, type="closed", length = unit(0.4, "cm"))
  }else{
    edge_arrow=NULL
  }
  if (missing(edge_color) & missing(edge_size)){
    link <- function() geom_edge_link(edge_color="slategrey", edge_width=0.6, arrow=edge_arrow)
  }
  else if (missing(edge_color) & !missing(edge_size)){
    if (is.numeric(substitute(edge_size))){
      link <- function() geom_edge_link(edge_width=edge_size, edge_color="slategrey", arrow=edge_arrow)
    }else{
      link <- function() geom_edge_link(aes(edge_width=!!edge_s), edge_color="slategrey", arrow=edge_arrow)
    }
  }
  else if (!missing(edge_color) & missing(edge_size)){
    if (is.character(substitute(edge_color))){
      link <- function() geom_edge_link(edge_color=edge_color, edge_width=0.6, arrow=edge_arrow)
    }else{
      link <- function() geom_edge_link(aes(edge_color=!!edge_c), edge_width=0.6, arrow=edge_arrow)
    }
  }
  else if (is.character(substitute(edge_color)) & is.numeric(substitute(edge_size))){
    link <- function() geom_edge_link(edge_width=edge_size, edge_color=edge_color, arrow=edge_arrow)}
  else if (is.character(substitute(edge_color)) & !is.numeric(substitute(edge_size))){
    link <- function() geom_edge_link(aes(edge_width=!!edge_s), edge_color=edge_color, arrow=edge_arrow)}
  else if (!is.character(substitute(edge_color)) & is.numeric(substitute(edge_size))){
    link <- function() geom_edge_link(aes(edge_color=!!edge_c), edge_width=edge_size, arrow=edge_arrow)
  }
  else{
    link <- function() geom_edge_link(aes(edge_color=!!edge_c, edge_width=!!edge_s), arrow=edge_arrow)
  }

  ##label
  if (missing(label)){
    print(vertex_attr_names(data)[1])
    text <- function() geom_node_text(aes(label=get(vertex_attr_names(data)[1])),
                                      colour = 'navy', vjust = 0.4)
  }
  else{
    label=enquo(label)
    text <- function() geom_node_text(aes(label=!!label),
                                      colour = 'navy', vjust = 0.4)
  }

  ##graph
  g<-data %>%
    ggraph(layout = layout)+
    link() +
    point() +
    text()+
    ggtitle(title)+
    theme_graph()


  all_data<-list("data"=data,"network"=g)
  return(all_data)
}
