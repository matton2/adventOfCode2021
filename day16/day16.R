library(tidyverse)


demoText <- "target area: x=20..30, y=-10..-5"

xminDemo <- 20
xmaxDemo <- 30
ymaxDemo <- 10
yminDemo <- -5

problemText <- "target area: x=281..311, y=-74..-54"

xmin <- 281
xmax <- 311
ymin <- -74
ymax <- -54

solve1 <- function(xmin, xmax, ymax, ymin) {
  
  # solution stolen from drob, my initial solution for part 1 was no where near this clean and had a lot of weird for and while loops....
  
  # this creates an initial tibble with all x, y, and time values
  points <- crossing(xv0 = seq(1, xmax), yv0 = seq(ymin, 200),
                   time = 1:500) %>% 
    # figure out the x,y position at any given time
    mutate(xv = pmax(xv0 - time + 1, 0),
           yv = yv0 - time + 1) %>% 
    # group by the initial velo
    group_by(xv0, yv0) %>% 
    # figure out the actual x y position for each point over time
    mutate(x = cumsum(xv),
           y = cumsum(yv)) %>% 
    # take any those points that fall within the grid
    filter(any(between(x, xmin, xmax) & between(y, ymin, ymax))) %>% 
    summarize(maxY = max(y),
              .groups = "drop") %>% 
    summarize(part1 = max(maxY),
              part2 = n())
  
  print(points)
  
}

debugonce(solve1)
solve1(xminDemo, xmaxDemo, yminDemo, ymaxDemo)
solve1(xmin, xmax, ymax, ymin)
