library(tidyverse)
library(R6DS)

demo <- c('[({(<(())[]>[[{[]{<()<>>',
          '[(()[<>])]({[<{<<[]>>(',
          '{([(<{}[<>[]}>{[]{[(<()>',
          '(((({<>}<{<{<>}{[]{[]{}',
          '[[<[([]))<([[{}[[()]]]',
          '[{[{({}]{}}([{[{{{}}([]',
          '{<[[]]>}<{[{[{[]{()[[[]',
          '[<(<(<(<{}))><([]([]()',
          '<{([([[(<>()){}]>(<<{{',
          '<{([{{}}[<[[[<>{}]]]>[]]')

problemData <- read_csv("day10/day10.csv", col_names = FALSE) %>% 
  pull(X1)

mapping <- tibble(open = c("(", "{", "[", "<"),
                  close = c(")", "}", "]", ">"))

scores <- tibble(close = c(")", "}", "]", ">"),
                 score = c(3, 1197, 57, 25137))


solve1 <- function(data, mapping, scores) {
  
  errors <- c()
  linesWithErrors <- c()
  s <- RStack$new()
  
  
  temp <- str_split(data, '')
  
  for(i in 1:length(temp)) {
    currentLine <- temp[[i]]
    
    
    
    for(j in 1:length(currentLine)) {
      
      # if its a staring one push onto the stack
      if(currentLine[j] %in% c("(", "{", "[", "<")) {
        s$push(currentLine[j])
      } else {
        currentOpen <- s$pop()
        currentClose <- currentLine[j]
        rightClose <- mapping %>% filter(open == currentOpen) %>% pull(close)
        if(rightClose != currentClose) {
          errors <- c(errors, currentClose)
          linesWithErrors <- c(linesWithErrors, i)
          break
        }
        
        
      }
      
    }
    
    
    
    
    
  }
  
  errorFinal <- tibble(close = errors) %>% 
    group_by(close) %>% 
    summarize(count = n()) %>% 
    left_join(scores) %>% 
    mutate(finalScore = count * score) %>% 
    pull(finalScore) %>% 
    sum(.)
  
  # uncomment to get problem final score
  # return(errorFinal)
  
  
  return(linesWithErrors)
  
  
}

# debugonce(solve1)
solve1(demo, mapping, scores)
solve1(problemData, mapping, scores)

demoLineErrors <- solve1(demo, mapping, scores)
demoIncompleteLines <- demo[-demoLineErrors]

lineErrors <- solve1(problemData, mapping, scores)

incompleteLines <- problemData[-lineErrors]

scores2 <- tibble(close = c(")", "}", "]", ">"),
                 score = c(1, 3, 2, 4))

solve2 <- function(data, mapping, scores2) {
  
  scores <- c()
  s <- RStack$new()
  
  
  temp <- str_split(data, '')
  
  for(i in 1:length(temp)) {
    currentLine <- temp[[i]]
    errorString <- c()
    currentTotal <- 0
    
    for(j in 1:length(currentLine)) {
      
      # if its a staring one push onto the stack
      if(currentLine[j] %in% c("(", "{", "[", "<")) {
        s$push(currentLine[j])
      } else {
        currentOpen <- s$pop()
      }
        
        
    }
    # once I am sorting, i just to need pop off the stack to get the correct match
    while(s$size > 0) {
      rightClose <- mapping %>% filter(open == s$pop()) %>% pull(close)
      errorString <- c(errorString, rightClose)
      
    }
    
    for(j in 1:length(errorString)) {
      currentTotal <- currentTotal * 5
      currentTotal <- currentTotal + 
        (scores2 %>% filter(close == errorString[j]) %>% pull(score))
    }
    
    scores <- c(scores, currentTotal)
    
    
    
      
    }
    
    
  return(median(scores))
    
    
    
}
  
  


debug(solve2)
solve2(demoIncompleteLines, mapping, scores2)
solve2(incompleteLines, mapping, scores2)
