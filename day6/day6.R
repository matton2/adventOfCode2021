# day 6

library(tidyverse)

demo <- c(3,4,3,1,2)

solve1 <- function(fish, dayCount) {
  
  currentDay <- 0
  
  while(currentDay < dayCount) {
    fishToAdd <- 0
    for(i in 1:length(fish)) {
      fish[i] <- fish[i] - 1
      if(fish[i] == -1) {
        fish[i] <- 6
        fishToAdd <- fishToAdd + 1
      } 
    }
    if(fishToAdd > 0) {
      for(j in 1:fishToAdd) {
        fish <- c(fish, 8)
      }
    }
    currentDay <- currentDay + 1
  }
  
  return(length(fish))
  
}

solve1(demo, 18)
solve1(demo, 80)

problemData <- c(3,5,1,5,3,2,1,3,4,2,5,1,3,3,2,5,1,3,1,5,5,1,1,1,2,4,1,4,5,2,1,2,4,3,1,2,3,4,3,4,4,5,1,1,1,1,5,5,3,4,4,4,5,3,4,1,4,3,3,2,1,1,3,3,3,2,1,3,5,2,3,4,2,5,4,5,4,4,2,2,3,3,3,3,5,4,2,3,1,2,1,1,2,2,5,1,1,4,1,5,3,2,1,4,1,5,1,4,5,2,1,1,1,4,5,4,2,4,5,4,2,4,4,1,1,2,2,1,1,2,3,3,2,5,2,1,1,2,1,1,1,3,2,3,1,5,4,5,3,3,2,1,1,1,3,5,1,1,4,4,5,4,3,3,3,3,2,4,5,2,1,1,1,4,2,4,2,2,5,5,5,4,1,1,5,1,5,2,1,3,3,2,5,2,1,2,4,3,3,1,5,4,1,1,1,4,2,5,5,4,4,3,4,3,1,5,5,2,5,4,2,3,4,1,1,4,4,3,4,1,3,4,1,1,4,3,2,2,5,3,1,4,4,4,1,3,4,3,1,5,3,3,5,5,4,4,1,2,4,2,2,3,1,1,4,5,3,1,1,1,1,3,5,4,1,1,2,1,1,2,1,2,3,1,1,3,2,2,5,5,1,5,5,1,4,4,3,5,4,4)

tictoc::tic()
solve1(problemData, 80)
tictoc::toc()

# solve 1 is going to take way to long to solve part2

aBetterSolve <- function(fish, dayCount) {
  
  # set the day to 0
  currentDay <- 0
  
  # build the count table
  fishCount <- tibble(fishNum = c(-1:8), count = 0)
  
  # set the count table to starting number of fish
  fishCount$count[1] <- length(which(fish == -1))
  fishCount$count[2] <- length(which(fish == 0))
  fishCount$count[3] <- length(which(fish == 1))
  fishCount$count[4] <- length(which(fish == 2))
  fishCount$count[5] <- length(which(fish == 3))
  fishCount$count[6] <- length(which(fish == 4))
  fishCount$count[7] <- length(which(fish == 5))
  fishCount$count[8] <- length(which(fish == 6))
  fishCount$count[9] <- length(which(fish == 7))
  fishCount$count[10] <- length(which(fish == 8))
  
  # this could be a for loop as well
  while(currentDay < dayCount) {
    
    # move all the fish counts down 1
    fishCount <- fishCount %>% 
      mutate(count = lead(count))
    
    # get the total number of new fish
    fishToAdd <- fishCount$count[1]
    # set that count back to 0
    fishCount$count[1] <- 0
    # those fish that are creating new fish get added to the current 6 day old fish
    fishCount$count[8] <- fishCount$count[8] + fishToAdd
    # those new fish get added to day 8 fish
    fishCount$count[10] <- fishToAdd
    
    # increase the day
    currentDay <- currentDay + 1
    
    
  }
  
  # simply return the table, i could do the math here as well
  return(fishCount)
  
}

aBetterSolve(demo, 18)
aBetterSolve(demo, 80)

tictoc::tic()
prob1 <- aBetterSolve(problemData, 80)
tictoc::toc()
sum(prob1$count)

tictoc::tic()
prob2 <- aBetterSolve(problemData, 256)
tictoc::toc()
sum(prob2$count)


