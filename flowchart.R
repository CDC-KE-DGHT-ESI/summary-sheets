# Load required library
if (!require("DiagrammeR")) install.packages("DiagrammeR")
library(DiagrammeR)


# Create the updated flowchart
DiagrammeR::grViz("
digraph {
  graph [layout = dot, rankdir = TB]
  
  # Define node styles
  node [shape = rectangle, style = filled, fontcolor = white]
  rec1 [label = 'Tests within NDW \n 3,527,322', fillcolor = '#1f78b4']
  rec1b [label = 'Tests within KHIS', fillcolor = '#6a3d9a']
  rec2 [label = 'Tests with a NUPI \n 478,832', fillcolor = '#33a02c']
  rec3 [label = 'Distinct Clients \n Based on NUPI \n 412,005 \n (86.04%)', fillcolor = '#ff7f00']
  
  # Define edge styles
  edge [color = '#b2df8a', penwidth = 2.5]
  
  # Edge definitions with the node IDs
  rec1 -> rec2 -> rec3
  rec1 -> rec1b [constraint = false]
}", 
                  height = 500, 
                  width = 700)

