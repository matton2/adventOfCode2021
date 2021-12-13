library(tidyverse)
library(adventdrob)


demo <- c('5483143223',
          '2745854711',
          '5264556173',
          '6141336146',
          '6357385478',
          '4167524645',
          '2176841721',
          '6882881134',
          '4846848554',
          '5283751526')

demoTibble <- tibble(X1 = demo) %>% 
  grid_tidy(X1) 

demoTibble %>% adventdrob:::adjacent_join(diagonal = TRUE)


cleanTemp <- cleanData(demo)

solve1 <- function(octo, steps) {
  
  total9 <- 0
  
  # step 1, increase everything by 1
  for(a in 1:steps) {
    # step 1, increase everything by 1
    octo <- map(octo, ~.x+1)
    
    # step 2 trigger the light sequence
    any9s <- map(octo, ~.x>9 & .x<100) %>% flatten()
    
    while(some(any9s, isTRUE)) {
      for(i in 1:length(octo)) {
        currentLine <- octo[[i]]
        for(j in 1:length(currentLine)) {
          if((currentLine[j] > 9) & (currentLine[j] < 100)) {
            total9 <- total9 + 1
            # if the fist line
            if (i == 1) {
              octo[[i]][j] <- 100
              
              if(j == length(currentLine)) {
                
                # octo[[i-1]][j+1] <- octo[[i-1]][j+1] + 1
                # octo[[i-1]][j] <- octo[[i-1]][j] + 1
                # octo[[i-1]][j-1] <- octo[[i-1]][j-1] + 1
                
                 octo[[i]][j-1] <- octo[[i]][j-1] + 1
                # octo[[i]][j+1] <- octo[[i]][j+1] + 1
                
                # octo[[i+1]][j+1] <- octo[[i+1]][j+1] + 1
                 octo[[i+1]][j] <- octo[[i+1]][j] + 1
                 octo[[i+1]][j-1] <- octo[[i+1]][j-1] + 1
                
              } else {
                
                # octo[[i-1]][j+1] <- octo[[i-1]][j+1] + 1
                # octo[[i-1]][j] <- octo[[i-1]][j] + 1
                # octo[[i-1]][j-1] <- octo[[i-1]][j-1] + 1
                
                 octo[[i]][j-1] <- octo[[i]][j-1] + 1
                 octo[[i]][j+1] <- octo[[i]][j+1] + 1
                
                 octo[[i+1]][j+1] <- octo[[i+1]][j+1] + 1
                 octo[[i+1]][j] <- octo[[i+1]][j] + 1
                 octo[[i+1]][j-1] <- octo[[i+1]][j-1] + 1
                
              }
              
            } else if(i == length(octo)) {
              octo[[i]][j] <- 100
              
              if(j == length(currentLine)) {
                
                # octo[[i-1]][j+1] <- octo[[i-1]][j+1] + 1
                 octo[[i-1]][j] <- octo[[i-1]][j] + 1
                 octo[[i-1]][j-1] <- octo[[i-1]][j-1] + 1
                
                 octo[[i]][j-1] <- octo[[i]][j-1] + 1
                # octo[[i]][j+1] <- octo[[i]][j+1] + 1
                
                # octo[[i+1]][j+1] <- octo[[i+1]][j+1] + 1
                # octo[[i+1]][j] <- octo[[i+1]][j] + 1
                # octo[[i+1]][j-1] <- octo[[i+1]][j-1] + 1
                
              } else {
                
                 octo[[i-1]][j+1] <- octo[[i-1]][j+1] + 1
                 octo[[i-1]][j] <- octo[[i-1]][j] + 1
                 octo[[i-1]][j-1] <- octo[[i-1]][j-1] + 1
                
                 octo[[i]][j-1] <- octo[[i]][j-1] + 1
                 octo[[i]][j+1] <- octo[[i]][j+1] + 1
                
                # octo[[i+1]][j+1] <- octo[[i+1]][j+1] + 1
                # octo[[i+1]][j] <- octo[[i+1]][j] + 1
                # octo[[i+1]][j-1] <- octo[[i+1]][j-1] + 1
                
              }
              
              
            } else {
              octo[[i]][j] <- 100
              
              if(j == length(currentLine)) {
                
                # octo[[i-1]][j+1] <- octo[[i-1]][j+1] + 1
                octo[[i-1]][j] <- octo[[i-1]][j] + 1
                octo[[i-1]][j-1] <- octo[[i-1]][j-1] + 1
                
                octo[[i]][j-1] <- octo[[i]][j-1] + 1
                # octo[[i]][j+1] <- octo[[i]][j+1] + 1
                
                # octo[[i+1]][j+1] <- octo[[i+1]][j+1] + 1
                octo[[i+1]][j] <- octo[[i+1]][j] + 1
                octo[[i+1]][j-1] <- octo[[i+1]][j-1] + 1
                
                
                
              } else {
                
                octo[[i-1]][j+1] <- octo[[i-1]][j+1] + 1
                octo[[i-1]][j] <- octo[[i-1]][j] + 1
                octo[[i-1]][j-1] <- octo[[i-1]][j-1] + 1
                
                octo[[i]][j-1] <- octo[[i]][j-1] + 1
                octo[[i]][j+1] <- octo[[i]][j+1] + 1
                
                octo[[i+1]][j+1] <- octo[[i+1]][j+1] + 1
                octo[[i+1]][j] <- octo[[i+1]][j] + 1
                octo[[i+1]][j-1] <- octo[[i+1]][j-1] + 1
                
               
              }
              
            }
            any9s <- map(octo, ~.x>10 & .x<100) %>% flatten()
            
          }
          
        }
        
      }
    }
    
    # step 3, reset all those greater than 9 to 0
    octo <- map(octo, ~ifelse(.x > 9, 0, .x))
    
    
  }
  
  #return(total9)
  return(octo)
}

debugonce(solve1)
demoResult <- solve1(cleanTemp, 2)
