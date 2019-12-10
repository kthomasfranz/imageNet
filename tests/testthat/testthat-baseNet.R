context("baseNet function")

test_that("errors generated from bad input", {
  library(igraphdata)
  data(karate)
  data(mtcars)

  expect_error(baseNet(), '"data" is missing')
  expect_error(baseNet(mtcars), "Invalid vertex id")
  expect_error(baseNet(karate, layout="bananas"), "not found")
  expect_error(baseNet(karate, label=none), "Label error")
  expect_error(baseNet(karate, node_color=left), "Node color error")
  expect_error(baseNet(karate, node_size="purple"), "Node size error")
  expect_error(baseNet(karate, edge_color=beef), "Edge color error")
  expect_error(baseNet(karate, edge_size="purple"), "Edge size error")
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
  expect_equal(baseNet(karate)$data, data)
  expect_equal(class(baseNet(karate, label=name, layout = 'kk')$network), class(g))
  expect_equivalent(baseNet(karate, label=name, layout = "kk")$network, g)
})
