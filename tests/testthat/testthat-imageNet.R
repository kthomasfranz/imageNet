context("imageNet function")

test_that("errors generated for bad input", {
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

# test_that("output correct answer", {
#   library(igraphdata)
#   data(karate)
#
#
# })
