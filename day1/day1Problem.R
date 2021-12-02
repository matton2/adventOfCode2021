library(tidyverse)

demoPuzzle <- c(199,
                200,
                208,
                210,
                200,
                207,
                240,
                269,
                260,
                263)


increaseDemo <- 0

for(i in 1:length(demoPuzzle)) {
  if(i == 1) {
    
  } else {
    if(demoPuzzle[i] > demoPuzzle[i-1]) {
      increaseDemo = 1 + increaseDemo
    }
  }
}


problem1 <- read_csv("Week62/day1Problem1.csv", col_names = FALSE)

increaseProblem <- 0

for(i in 1:NROW(problem1)) {
  if (i == 1) {
    
  } else {
    if(problem1$X1[i] > problem1$X1[i-1]) {
      increaseProblem = increaseProblem + 1
    }
  }
}


# problem 2

demoProblem <- tibble(num = demoPuzzle)

demoProblem <- demoProblem %>% mutate(firstLag = lag(num),
                       secondLag = lag(num, n= 2),
                       math = num + firstLag + secondLag)


demoProblemList <- na.omit(demoProblem$math)

demoProblem2Increase <- 0

for(i in 1:length(demoProblemList)) {
  if (i == 1) {
    
  } else {
    if(demoProblemList[i] > demoProblemList[i-1]) {
      demoProblem2Increase <- demoProblem2Increase + 1
    }
  }
}

# solve puzzle 2

puzz2 <- problem1 %>% mutate(firstLag = lag(X1),
                                      secondLag = lag(X1, n = 2),
                                      math = X1 + firstLag + secondLag)

puzz2List <- na.omit(puzz2$math)

problem2Increase <- 0

for(i in 1:length(puzz2List)) {
  if (i == 1) {
    
  } else {
    if(puzz2List[i] > puzz2List[i-1]) {
      problem2Increase <- problem2Increase + 1
    }
  }
}
