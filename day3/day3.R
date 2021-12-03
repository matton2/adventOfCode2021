library(tidyverse)


demo <- tibble(X1 = c('00100',
          '11110',
          '10110',
          '10111',
          '10101',
          '01111',
          '00111',
          '11100',
          '10000',
          '11001',
          '00010',
          '01010'))


prob1 <- function(problem) {
  
  cleanItUp <- separate(problem, X1, into = letters[1:(length(str_split(problem[[1]][[1]], "")[[1]]) + 1)], sep = "") %>% 
    select(-a)
  
  gammaString <- ""
  epsilonString <- ""
  
  for (i in 1:NCOL(cleanItUp)) {
    
    currentCol <- cleanItUp[[i]]
    
    count0 <- 0
    count1 <- 0
    
    for (j in 1:length(currentCol)) {
      
      if (currentCol[j] == "0") {
        count0 <- count0 + 1
      } else {
        count1 <- count1 + 1
      }
      
    }
    
    if(count0 > count1) {
      gammaString <- str_c(gammaString, "0")
      epsilonString <- str_c(epsilonString, "1")
    } else {
      gammaString <- str_c(gammaString, "1")
      epsilonString <- str_c(epsilonString, "0")
    }
    
    
    
    
  }
  
  print(gammaString)
  
  print(epsilonString)
  
  return (strtoi(gammaString, base = 2) * strtoi(epsilonString, base = 2))
  
  
}

prob1(demo)


# read in the data

data <- read_csv("day3/day3.csv", col_names = FALSE)

prob1(data)


prob2 <- function(problem) {
  
  cleanItUp <- separate(problem, X1, into = LETTERS[1:(length(str_split(problem[[1]][[1]], "")[[1]]) + 1)], sep = "") %>% 
    select(-A)
  
  oxygen <- cleanItUp
  co <- cleanItUp
  
  for (i in 1:NCOL(cleanItUp)) {
    
    if (NROW(oxygen) > 1) {
      oxyCurrentCol <- oxygen[[i]]
      
      oxyCount0 <- 0
      oxyCount1 <- 0
      
      for(j in 1:length(oxyCurrentCol)) {
        
        if (oxyCurrentCol[j] == "0") {
          oxyCount0 <- oxyCount0 + 1
        } else {
          oxyCount1 <- oxyCount1 + 1
        }
        
        
      }
      
      if(oxyCount0 > oxyCount1) {
        oxygen <- oxygen %>% filter(across(i, ~ . == "0"))
      } else {
        oxygen <- oxygen %>% filter(across(i, ~ . == "1"))
      }
    }
    
    if(NROW(co) > 1) {
      coCurrentCol <- co[[i]]
      
      coCount0 <- 0
      coCount1 <- 0
      
      for(j in 1:length(coCurrentCol)) {
        
        if (coCurrentCol[j] == "0") {
          coCount0 <- coCount0 + 1
        } else {
          coCount1 <- coCount1 + 1
        }
        
        
      }
      
      if(coCount0 > coCount1) {
        co <- co %>% filter(across(i, ~ . == "1"))
      } else {
        co <- co %>% filter(across(i, ~ . == "0"))
      }
      
    }
    
  }
  
  print(oxygen)
  
  print(co)
  
  oxygen <- oxygen %>% unite('final',1:ncol(oxygen),sep="")
  co <- co %>% unite('final',1:ncol(co),sep="")
  
  return(strtoi(oxygen$final[1], base = 2) * strtoi(co$final[1], base = 2))
  
  
}

prob2(demo)
prob2(data)
