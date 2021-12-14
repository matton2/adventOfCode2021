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
         pairing1 = paste0(start,insert),
         pairing2 = paste0(insert, end),
         notFinalPairing = paste0(start, insert),
         final = paste0(start,insert,end)) 

# uncomment below to try to get solve2 to work
# %>% 
#   select(pair, pairing1, pairing2) %>% 
#   pivot_longer(cols = c(pairing1, pairing2), values_to = 'newPairs') %>% 
#   select(-name)

problemTemplate <- read_lines('day14/day14.txt', n_max = 1)

problemRules <- tibble(x = read_lines('day14/day14.txt', skip = 2)) %>% 
  separate(x, into = c('pair', 'insert')) %>% 
  mutate(pair1 = pair,
         start = str_sub(pair1, 1, 1),
         end = str_sub(pair1, 2, 2),
         pairing1 = paste0(start,insert),
         pairing2 = paste0(insert, end),
         notFinalPairing = paste0(start, insert),
         final = paste0(start,insert,end)) 

# uncomment below to try to get solve2 to work
# %>% 
#   select(pair, pairing1, pairing2) %>% 
#   pivot_longer(cols = c(pairing1, pairing2), values_to = 'newPairs') %>% 
#   select(-name)

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

solve2 <- function(template, rules, steps) {
  
  starting <- c()
  
  for(i in 1:nchar(template)-1) {
    starting[i] <- str_sub(template, i, i +1)
  }
  
  count <- rules %>% 
    select(pair) %>% 
    distinct() %>% 
    mutate(total = ifelse(pair %in% starting, 1, 0))  
  
  for(i in 2:steps) {
    stepCount <- count %>% select(pair) %>% mutate(increaseCount = 0)
    for(j in 1:NROW(count)) {
      if(count$total[j] > 0) {
        # get the pair total
        startingCount <- count$total[j]
        # decrease the pair total by 1
        count$total[j] <- 0
        # get the current pair
        currentPair <- count$pair[j]
        # figure out the pair in makes
        newPairs <- rules %>% filter(pair == currentPair) %>% pull(newPairs)
        
        # add the new pair and the current count
        stepCount <- stepCount %>% 
          mutate(increaseCount = ifelse(pair %in% newPairs, increaseCount + startingCount, increaseCount))
        
      }
    }
    # at the end of the step combine all the counts
    count <- count %>% 
      left_join(stepCount, by = c('pair' = 'pair')) %>% 
      mutate(total = increaseCount) %>% 
      select(-increaseCount)
    
  }
    
  totalTibble <- count %>% 
    separate(pair, into= c('d', 'f', 'l'), sep = "") %>% 
    select(l, total) %>% 
    group_by(l) %>% 
    summarise(sum = sum(total))
  
  
  
  # return(count)
  return(max(totalTibble$sum) - min(totalTibble$sum))
  
}

debugonce(solve2)

tictoc::tic()
part2 <- solve2(demoTemplate, rules, 10)
part2 <- solve2(problemTemplate, problemRules, 10)
tictoc::toc()


day4Tibble <- tibble(pair = starting) %>% 
  group_by(pair) %>% 
  summarize(count = n())

temp <- part2 %>% 
  left_join(day4Tibble)
