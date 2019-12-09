context("imageNet function")

test_that("errors generated from bad input", {
  library(igraphdata)
  data(karate)
  data(mtcars)

  expect_error(imageNet(), '"data" is missing')
  expect_error(imageNet(mtcars), "Invalid vertex id")
  expect_error(imageNet(karate, layout=c), "layout function must return an object")
  expect_error(imageNet(karate, label=beef), "Label error")
  expect_error(imageNet(karate, node_color=beef), "Node color error")
  expect_error(imageNet(karate, node_size="purple"), "Node size error")
  expect_error(imageNet(karate, edge_color=beef), "Edge color error")
  expect_error(imageNet(karate, edge_size="purple"), "Edge size error")
})

test_that("output correct answer", {
  library(igraphdata)
  data(karate)

  ##Convert data to tidygraph format & add centrality measures to data
  data = as_tbl_graph(karate)
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
    mutate(betweenness = centrality_betweenness(directed=F)) %>%
    mutate(closeness = centrality_closeness())%>%
    mutate(degree=centrality_degree())%>%
    mutate(eigen=centrality_eigen(directed=F))

  g<-data %>%
    ggraph(layout = "kk")+
    geom_edge_link(edge_color="slategrey", edge_width=0.6) +
    geom_node_point(color="cornflowerblue", size=8) +
    geom_node_text(aes(label = name), colour = 'navy', vjust = 0.4)+
    ggtitle("Network Graph of karate data") +
    theme_graph()

  ##Testthat
  expect_equal(imageNet(karate)$data, data)
  expect_equal(class(imageNet(karate, label=name, layout = 'kk')$network), class(g))
  expect_equivalent(imageNet(karate, label=name, layout = "kk")$network, g)
})
