# day2 problem
library(tidyverse)


demoProblem <- tibble(X1 = c('forward 5',
                 'down 5',
                 'forward 8',
                 'up 3',
                 'down 8',
                 'forward 2'))

hor <- 0
depth <- 0

for(i in 1:NROW(demoProblem)) {
  direction <- str_split(demoProblem$X1[i], " ")[[1]][1]
  speed <- str_split(demoProblem$X1[i], " ")[[1]][2] %>% as.numeric()
  
  if(direction == "forward") {
    hor <- hor + speed
  } else if (direction == "down") {
    depth <- depth + speed
  } else {
    depth <- depth - speed
  }
  
}

depth * hor

# solve problem 1

day2 <- read_csv("day2/day2problem.csv", col_names = FALSE)

# we can turn this thing into a function

prob1 <- function(problemData) {
  
  hor <- 0
  depth <- 0
  
  for(i in 1:NROW(problemData)) {
    direction <- str_split(problemData[[1]][i], " ")[[1]][1]
    speed <- str_split(problemData[[1]][i], " ")[[1]][2] %>% as.numeric()
    
    if(direction == "forward") {
      hor <- hor + speed
    } else if (direction == "down") {
      depth <- depth + speed
    } else {
      depth <- depth - speed
    }
    
  }
  
  depth * hor
  
  
}


prob1(day2)


# problem 2

prob2 <- function(problemData) {
  
  hor <- 0
  depth <- 0
  aim <- 0
  
  for(i in 1:NROW(problemData)) {
    direction <- str_split(problemData[[1]][i], " ")[[1]][1]
    speed <- str_split(problemData[[1]][i], " ")[[1]][2] %>% as.numeric()
    
    if(direction == "forward") {
      hor <- hor + speed
      depth <- (speed * aim) + depth
    } else if (direction == "down") {
      aim <- aim + speed
    } else {
      aim <- aim - speed
    }
    
  }
  
  hor * depth
  
  
}

prob2(demoProblem)

prob2(day2)
