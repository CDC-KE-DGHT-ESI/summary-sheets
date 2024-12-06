library(testthat)
library(tidyverse)

sample_data <- data.frame(group = c("A", "B", "A", "B"),
                          value = c(10, 15, 20, 25))

sample_data

sample_data %>%
  group_by(group) %>%
  summarise(total = sum(value))

test_that("Data aggregation produces correct sums", {
  result <- sample_data %>%
    group_by(group) %>%
    summarise(total = sum(value))
  expected <- data.frame(group = c("A", "B"), total = c(30, 40)) %>%
    as.data.frame()
  expect_equal(result, expected)
})

test_that("trigonometric functions match identities", {
  expect_equal(sin(pi / 4), 1 / sqrt(2))
  expect_equal(cos(pi / 4), 1 / sqrt(2))
  expect_equal(tan(pi / 4), 1)
})


# Recreate Bostock Sankey diagram: http://bost.ocks.org/mike/sankey/
# Load energy projection data
URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
              "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)

# Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)


# Load igraph
library(igraph)

# Use igraph to make the graph and find membership
karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate)
members <- membership(wc)

# Convert to object suitable for networkD3
karate_d3 <- igraph_to_networkD3(karate, group = members)

# Create force directed network plot
forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
             Source = 'source', Target = 'target', NodeID = 'name', 
             Group = 'group')


library(magrittr)

simpleNetwork(networkData) %>% saveNetwork(file = 'Net1.html')