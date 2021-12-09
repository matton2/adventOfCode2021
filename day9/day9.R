library(tidyverse)

demo <- c('2199943210',
          '3987894921',
          '9856789892',
          '8767896789',
          '9899965678')

problemData <- read_csv('day9/day9.csv', col_names = FALSE) %>% pull(X1)


findLowPoints <- function(data) {
  
  totalLowPoints <- 0
  
  temp <- str_split(data, '')
  
  # for the first line, i just need check it and the row below
  # for the last line, i just need to check the previous line
  # for all other lines i need to look all around
  
  rowLeftBound <- 1
  rowRightBound <- length(temp[[1]])
  firstRow <- 1
  lastRow <- length(temp)
  
  for(i in 1:lastRow) {
    if(i == 1) {
      currentRow <- temp[[i]] %>% as.numeric()
      lowerRow <- temp[[i + 1]] %>% as.numeric()
      for(j in 1:length(currentRow)) {
        # if the first row only check to the right and below
        if(j == 1) {
          if((currentRow[j] < currentRow[j+1]) & (currentRow[j] < lowerRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
          
        # last row check to the left and below
        } else if (j == length(currentRow)) {
          if((currentRow[j] < currentRow[j-1]) & (currentRow[j] < lowerRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
          
        # check left, right and below
        } else {
          if((currentRow[j] < currentRow[j-1]) & 
             (currentRow[j] < currentRow[j+1]) &
             (currentRow[j] < lowerRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
        }
      }
      
    } else if(i == lastRow) {
      currentRow <- temp[[i]] %>% as.numeric()
      upperRow <- temp[[i-1]] %>% as.numeric()
      for(j in 1:length(currentRow)) {
        # if the first row only check to the right and below
        if(j == 1) {
          if((currentRow[j] < currentRow[j+1]) & (currentRow[j] < upperRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
          
          # last row check to the left and below
        } else if (j == length(currentRow)) {
          if((currentRow[j] < currentRow[j-1]) & (currentRow[j] < upperRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
          
          # check left, right and below
        } else {
          if((currentRow[j] < currentRow[j-1]) & 
             (currentRow[j] < currentRow[j+1]) &
             (currentRow[j] < upperRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
        }
      }
      
    } else {
      currentRow <- temp[[i]] %>% as.numeric()
      upperRow <- temp[[i-1]] %>% as.numeric()
      lowerRow <- temp[[i+1]] %>% as.numeric()
      for(j in 1:length(currentRow)) {
        # if the first row only check to the right, up, and below
        if(j == 1) {
          if((currentRow[j] < currentRow[j+1]) & 
             (currentRow[j] < lowerRow[j]) &
             (currentRow[j] < upperRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
          
          # last row check to the left and below
        } else if (j == length(currentRow)) {
          if((currentRow[j] < currentRow[j-1]) &
             (currentRow[j] < lowerRow[j]) &
             (currentRow[j] < upperRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
          
          # check left, right and below
        } else {
          if((currentRow[j] < currentRow[j-1]) & 
             (currentRow[j] < currentRow[j+1]) &
             (currentRow[j] < lowerRow[j]) &
             (currentRow[j] < upperRow[j])) {
            totalLowPoints <- totalLowPoints + (currentRow[j] + 1)
          }
        }
      }
    }
  }
  
  
  return(totalLowPoints)
  
}

findLowPoints(demo)
findLowPoints(problemData)
