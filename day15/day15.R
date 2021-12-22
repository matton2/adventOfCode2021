# day 15

library(tidyverse)
library(adventdrob)


demo <- tibble(x = c('1163751742',
                     '1381373672',
                     '2136511328',
                     '3694931569',
                     '7463417111',
                     '1319128137',
                     '1359912421',
                     '3125421639',
                     '1293138521',
                     '2311944581'))

demoGraph <- adventdrob::grid_graph(demo, "x", sep="") 
