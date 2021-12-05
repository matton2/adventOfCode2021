library(tidyverse)

demo <- tibble(X1 = c("0,9 -> 5,9",
'8,0 -> 0,8',
'9,4 -> 3,4',
'2,2 -> 2,1',
'7,0 -> 7,4',
'6,4 -> 2,0',
'0,9 -> 2,9',
'3,4 -> 1,4',
'0,0 -> 8,8',
'5,5 -> 8,2')) %>% 
  separate(X1, into = c("first", 'arrow', 'second'), sep = " ") %>% 
  select(-arrow) %>% 
  separate(first, into = c("X1", "Y1"), sep = ",") %>% 
  separate(second, into = c("X2", "Y2"), sep = ",") %>% 
  mutate(across(.cols = everything(), as.numeric))

ggplot(demo) +
  geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2)) +
  scale_y_reverse()



solve1 <- function(data) {
  
  fullGrid <- data.frame(matrix(0,
                                nrow = 1000,
                                ncol = 1000))
  
  for (i in 1:NROW(data)) {
    if(data[i,]$X1 == data[i,]$X2) {
      # fill in the y coord at X
      xCoord <- data[i,]$X1 + 1
      yToFill <- seq(min(data[i,]$Y1, data[i,]$Y2),max(data[i,]$Y1, data[i,]$Y2), 1) + 1
      for(j in 1:length(yToFill)) {
        fullGrid[xCoord, yToFill[j]] <- fullGrid[xCoord, yToFill[j]] + 1
      }
    }
    
    if(data[i,]$Y1 == data[i,]$Y2) {
      # fill in the x coord of y
      yCoord <- data[i,]$Y1 + 1
      xToFill <- seq(min(data[i,]$X1, data[i,]$X2), max(data[i,]$X1, data[i,]$X2), 1) + 1
      for(j in 1:length(xToFill)) {
        fullGrid[xToFill[j], yCoord] <- fullGrid[xToFill[j], yCoord] + 1
      }
      
    }
    
  }
  
  total <- 0
  
  for(i in 1:NROW(fullGrid)) {
    for(j in 1:NCOL(fullGrid)) {
      spot <- fullGrid[i,j]
      if(spot > 1) {
        total <- total + 1
      }
    }
    }
  
  print(total)
  
}

demoSolve <- solve1(demo)


# read in the problem data

problemDataRaw <- read_delim("day5/day5.csv", delim = "\n", col_names = FALSE) 
  

problemData <- problemDataRaw %>% 
  separate(X1, into = c("first", 'arrow', 'second'), sep = " ") %>% 
  select(-arrow) %>% 
  separate(first, into = c("X1", "Y1"), sep = ",") %>% 
  separate(second, into = c("X2", "Y2"), sep = ",") %>% 
  mutate(across(.cols = everything(), as.numeric)) 

ggplot(problemData) +
  geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2)) +
  scale_y_reverse()

solve1(problemData)


solve2 <- function(data) {
  
  fullGrid <- data.frame(matrix(0,
                                nrow = 1000,
                                ncol = 1000))
  
  for (i in 1:NROW(data)) {
    if(data[i,]$X1 == data[i,]$X2) {
      # fill in the y coord at X
      xCoord <- data[i,]$X1 + 1
      yToFill <- seq(min(data[i,]$Y1, data[i,]$Y2),max(data[i,]$Y1, data[i,]$Y2), 1) + 1
      for(j in 1:length(yToFill)) {
        fullGrid[xCoord, yToFill[j]] <- fullGrid[xCoord, yToFill[j]] + 1
      }
    } else if(data[i,]$Y1 == data[i,]$Y2) {
      # fill in the x coord of y
      yCoord <- data[i,]$Y1 + 1
      xToFill <- seq(min(data[i,]$X1, data[i,]$X2), max(data[i,]$X1, data[i,]$X2), 1) + 1
      for(j in 1:length(xToFill)) {
        fullGrid[xToFill[j], yCoord] <- fullGrid[xToFill[j], yCoord] + 1
      }
      
    } else {
      # check for diagonals now
      xDir <- (data[i,]$X2-data[i,]$X1)/abs((data[i,]$X2-data[i,]$X1))
      yDir <- (data[i,]$Y2-data[i,]$Y1)/abs((data[i,]$Y2-data[i,]$Y1))
      xToFill <- seq(data[i,]$X1, data[i,]$X2, by = xDir) + 1
      yToFill <- seq(data[i,]$Y1, data[i,]$Y2, by = yDir) + 1
      for(j in 1:length(xToFill)) {
        fullGrid[xToFill[j], yToFill[j]] <- fullGrid[xToFill[j], yToFill[j]] + 1
      }
    }
    
    
    
  }
  
  total <- 0
  
  for(i in 1:NROW(fullGrid)) {
    for(j in 1:NCOL(fullGrid)) {
      spot <- fullGrid[i,j]
      if(spot > 1) {
        total <- total + 1
      }
    }
  }
  
  print(total)
  
}

solve2(demo)

solve2(problemData)

