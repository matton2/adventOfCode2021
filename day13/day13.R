library(tidyverse)


demo <- tibble(x = c('6,10',
          '0,14',
          '9,10',
          '0,3',
          '10,4',
          '4,11',
          '6,0',
          '6,12',
          '4,1',
          '0,13',
          '10,12',
          '3,4',
          '3,0',
          '8,4',
          '1,10',
          '2,14',
          '8,10',
          '9,0')) %>% 
  separate(col = x, into = c('x', 'y')) %>% 
  mutate_all(as.numeric)


problemData <- tibble(x = read_lines("day13/day13.txt", n_max = 950)) %>% 
  separate(col = x, into = c('x', 'y')) %>% 
  mutate_all(as.numeric)


instructions <- read_lines("day13/day13.txt", skip = 951)

onlyYInstruction <- str_extract_all(instructions, "y=[:digit:]{1,}")

# fold along y=7

solve1 <- function(data, yFold = NA, xFold = NA) {
  
  yFoldFinal <- tibble()
  xFoldFinal <- tibble()
  foldFinal <- tibble()
  
  if(!is.na(yFold)) {
    
    temp <- data %>%
      arrange(desc(y)) %>% 
      filter(y > yFold) %>% 
      mutate(y = y - ((y - yFold) * 2))
    
    yFoldFinal <- data %>% 
      filter(y < yFold) %>% 
      bind_rows(temp) %>% 
      distinct()
    
    return(yFoldFinal)
      
    
  }
  
  
  if(!is.na(xFold)) {
    
    temp <- data %>%
      arrange(desc(x)) %>% 
      filter(x > xFold) %>% 
      mutate(x = x - ((x - xFold) * 2))
    
    xFoldFinal <- data %>% 
      filter(x < xFold) %>% 
      bind_rows(temp) %>% 
      distinct()
    
    return(xFoldFinal)
    
  }
  
  #return(NROW(foldFinal))
  
  
  
}

debugonce(solve1)
solve1(demo, 7)

NROW(solve1(problemData, xFold = 655))


#solve2 

solve2 <- function(data, instructions) {
  
  cleanInstruction <- str_extract_all(instructions, "(x|y)=[:digit:]{1,}")
  
  finalData <- data
  
  for(i in 1:length(cleanInstruction)) {
    currentInstruction <- cleanInstruction[i][[1]]
    
    foldNumber <- str_extract(currentInstruction, "[:digit:]{1,}") %>% as.numeric()
    
    if(str_detect(currentInstruction, "x")) {
      finalData <- solve1(finalData, xFold = foldNumber)
    } else {
      finalData <- solve1(finalData, yFold = foldNumber)
    }
    
  }
  
  finalData <- finalData %>% 
    arrange(x, y) %>% 
    mutate(xDiff = x - lead(x),
           xDiff = ifelse(xDiff == -2, xDiff, NA),
           color = NA)
  
  currentCount <- 1
  
  for(i in 1:NROW(finalData)) {
    
    if(is.na(finalData$xDiff[i])) {
      finalData$color[i] <- currentCount
    } else {
      currentCount <- currentCount + 1
      finalData$color[i] <- currentCount
    }
    
    
  }
  
  return(finalData)
  
  
}

# debugonce(solve2)
part2 <- solve2(problemData, instructions)

ggplot(part2, aes(x = x, y = y, color = as.factor(color))) +
  geom_point() +
  scale_y_reverse() +
  theme_classic()
