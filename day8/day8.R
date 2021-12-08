# day 8

library(tidyverse)

# ideal disply:
# 0: abcefg
# 1: cf
# 2: acdeg
# 3: acdfg
# 4: bcdf
# 5: abdfg
# 6: abdefg
# 7: acf
# 8: abcdefg
# 9: abcdfg

oneLiner <- c('acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf')

demo <-
  c(
    'be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe',
    'edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc',
    'fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg',
    'fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb',
    'aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea',
    'fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb',
    'dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe',
    'bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef',
    'egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb',
    'gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce'
  )

problem <- read_csv("day8/day8.csv", col_names = FALSE)

solve1 <- function(data) {
  finalNumber <- 0
  
  for (i in 1:length(data)) {
    line <- str_split(data[i], "\\|")
    
    digits <- str_split(str_trim(line[[1]][[2]]), " ")[[1]]
    
    for (j in 1:length(digits)) {
      if (nchar(digits[j]) %in% c(2, 3, 4, 7)) {
        finalNumber <- finalNumber + 1
      }
    }
  }
  
  return(finalNumber)
  
}


solve1(demo)
solve1(problem$X1)

solve2 <- function(data) {
  finalNumber <- 0
  
  for (a in 1:length(data)) {
    line <- str_split(data[1], "\\|")
    
    numbers <- str_split(str_trim(line[[1]][[1]]), " ")[[1]]
    
    # make a look up table based on what i know already
    numberTibble <-
      tibble(code = numbers,
             number = NA,
             char = NA) %>%
      group_by(code) %>%
      mutate(code = paste(sort(unlist(
        strsplit(code, "")
      )), collapse = "")) %>%
      ungroup() %>%
      mutate(
        char = nchar(code),
        number = ifelse(nchar(code) == 2, 1, number),
        number = ifelse(nchar(code) == 3, 7, number),
        number = ifelse(nchar(code) == 4, 4, number),
        number = ifelse(nchar(code) == 7, 8, number)
      )
    
    # NEED TO FIND THEM IN THIS ORDER
    # for the 5 letter strings
    
    fiveLetter <- numberTibble %>% filter(char == 5) %>% pull(code)
    sixLetter <- numberTibble %>% filter(char == 6) %>% pull(code)
    
    # number 3, a 5 letter string, would the only one containing all of 1
    
    num1 <- numberTibble %>% filter(number == 1) %>% pull(code)
    
    num1SeachString <- str_split(num1, "")[[1]]
    
    for (i in 1:length(fiveLetter)) {
      count <- 0
      for (j in 1:length(num1SeachString)) {
        if (str_detect(fiveLetter[i], num1SeachString[j])) {
          count <- count + 1
        }
      }
      if (count == nchar(num1)) {
        numberTibble <- numberTibble %>%
          mutate(number = ifelse(code == fiveLetter[i], 3, number))
        fiveLetter <- fiveLetter[-i]
        break
      }
    }
    
    # number 9 contains the number 3
    
    num3 <- numberTibble %>% filter(number == 3) %>% pull(code)
    
    num3SeachString <- str_split(num3, "")[[1]]
    
    for (i in 1:length(sixLetter)) {
      count <- 0
      for (j in 1:length(num3SeachString)) {
        if (str_detect(sixLetter[i], num3SeachString[j])) {
          count <- count + 1
        }
      }
      if (count == nchar(num3)) {
        numberTibble <- numberTibble %>%
          mutate(number = ifelse(code == sixLetter[i], 9, number))
        sixLetter <- sixLetter[-i]
        break
      }
    }
    
    # number 0 contains the 7
    
    num7 <- numberTibble %>% filter(number == 7) %>% pull(code)
    
    num7SeachString <- str_split(num7, "")[[1]]
    
    for (i in 1:length(sixLetter)) {
      count <- 0
      for (j in 1:length(num7SeachString)) {
        if (str_detect(sixLetter[i], num7SeachString[j])) {
          count <- count + 1
        }
      }
      if (count == nchar(num7)) {
        numberTibble <- numberTibble %>%
          mutate(number = ifelse(code == sixLetter[i], 0, number))
        sixLetter <- sixLetter[-i]
        break
      }
    }
    
    # number 6 is my last string
    numberTibble <- numberTibble %>%
      mutate(number = ifelse(code == sixLetter[1], 6, number))
    
    
    
    # number 5 is inside the number 9
    num9 <- numberTibble %>% filter(number == 9) %>% pull(code)
    
    num9SeachString <- str_split(num9, "")[[1]]
    
    for (i in 1:length(fiveLetter)) {
      count <- 0
      for (j in 1:length(num9SeachString)) {
        if (str_detect(fiveLetter[i], num9SeachString[j])) {
          count <- count + 1
        }
      }
      if (count == 5) {
        numberTibble <- numberTibble %>%
          mutate(number = ifelse(code == fiveLetter[i], 5, number))
        fiveLetter <- fiveLetter[-i]
        break
      }
    }
    
    
    
    # number 2
    numberTibble <- numberTibble %>%
      mutate(number = ifelse(code == fiveLetter[1], 2, number))
    
    # get the digits from the string now
    digits <- str_split(str_trim(line[[1]][[2]]), " ")[[1]]
    
    for (i in 1:length(digits)) {
      digits[i] <-
        paste(sort(unlist(strsplit(digits[i], ""))), collapse = "")
    }
    
    # assign numbers now
    
    digitTibble <- tibble(code = digits)
    
    tempNumber <- digitTibble %>% 
      left_join(numberTibble) %>% 
      pull(number) %>%
      paste(., collapse = "") %>%
      as.numeric()
    
    finalNumber <- finalNumber + tempNumber
    
  }
  
  return(finalNumber)
}

solve2(oneLiner)
solve2(demo)
