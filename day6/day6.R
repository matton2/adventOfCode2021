# day 6

library(tidyverse)

demo <- c(3,4,3,1,2)

solve1 <- function(fish, dayCount) {
  
  while(dayCount != 80) {
    for(i in 1:length(fish)) {
      fishToAdd <- 0
      if(fish[i] == 0) {
        fish[i] <- 6
        fishToAdd <- fishToAdd + 1
      } else {
        fish[i] <- fish[i] - 1
      }
    }
    if(fishToAdd > 0) {
      for(j in 1:length(fishToAdd)) {
        fish <- c(fish, 8)
      }
    }
    dayCount <- dayCount + 1
  }
  
  return(length(fish))
  
}

solve1(demo, 18)
