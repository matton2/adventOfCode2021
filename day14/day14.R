# day 14

library(tidyverse)

demoTemplate <- "NNCB"

rules <- tibble(x = c('CH -> B',
                      'HH -> N',
                      'CB -> H',
                      'NH -> C',
                      'HB -> C',
                      'HC -> B',
                      'HN -> C',
                      'NN -> C',
                      'BH -> H',
                      'NC -> B',
                      'NB -> B',
                      'BN -> B',
                      'BB -> N',
                      'BC -> B',
                      'CC -> N',
                      'CN -> C')) %>% 
  separate(x, into = c('pair', 'insert')) %>% 
  mutate(pair1 = pair,
         start = str_sub(pair1, 1, 1),
         end = str_sub(pair1, 2, 2),
         notFinalPairing = paste0(start, insert),
         final = paste0(start,insert,end)) 

problemTemplate <- read_lines('day14/day14.txt', n_max = 1)

problemRules <- tibble(x = read_lines('day14/day14.txt', skip = 2)) %>% 
  separate(x, into = c('pair', 'insert')) %>% 
  mutate(pair1 = pair,
         start = str_sub(pair1, 1, 1),
         end = str_sub(pair1, 2, 2),
         notFinalPairing = paste0(start, insert),
         final = paste0(start,insert,end)) 

solve1 <- function(template, rules, steps) {
  
  finalTemplate <- template
  
  for(i in 1:steps) {
    
    print(paste("Starting step:", i))
    
    tempTemplate <- tibble(x = finalTemplate) %>% 
      separate(col = x, into = paste("X", 1:(1+nchar(finalTemplate)), sep = ""), sep = "") %>% 
      select(-X1) %>% 
      pivot_longer(cols = everything(), values_to = 'code') %>% 
      select(-name) %>% 
      mutate(pairings = paste0(lag(code), code), 
             row = row_number()) %>%
      filter(row > 1) %>% 
      left_join(rules, by = c("pairings" = 'pair')) %>% 
      mutate(row = row_number(),
             finalString = ifelse(row != max(row), notFinalPairing, final))
    
    
    print(paste("Done step:", i))
    
    
    finalTemplate <- paste0(tempTemplate$finalString, collapse = "")
    
  }
  
  finalTibble <- tibble(x = finalTemplate) %>% 
    separate(col = x, into = paste("X", 1:(1+nchar(finalTemplate)), sep = ""), sep = "") %>% 
    select(-X1) %>% 
    pivot_longer(cols = everything(), values_to = 'letters') %>% 
    group_by(letters) %>% 
    summarize(sum = n())
  
  finalResult <- max(finalTibble$sum) - min(finalTibble$sum)
  
  return(finalResult)
  
  
  
}

debugonce(solve1)

solve1(demoTemplate, rules, 10)

tictoc::tic()
solve1(problemTemplate, problemRules, 10)
tictoc::toc()

tictoc::tic()
part2 <- solve1(problemTemplate, problemRules, 15)
tictoc::toc()