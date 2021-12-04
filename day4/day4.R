# day4 

library(tidyverse)
library(R6)

calledNumbersDemo <- c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)

bingoCardsDataDemo <- c('22 13 17 11 0',
                '8 2 23 4 24',
                '21 9 14 16 7',
                '6 10 3 18 5',
                '1 12 20 15 19',
                '3 15 0 2 22',
                '9 18 13 17 5',
                '19 8 7 25 23',
                '20 11 10 24 4',
                '14 21 16 12 6',
                '14 21 17 24 4',
                '10 16 15 9 19',
                '18 8 23 26 20',
                '22 11 13 6 5',
                '2 0 12 3 7')



bingoCard <- R6Class('bingoCards',
  public = list(
    card = NULL,
    initialize = function(numbers) {
      self$card <- tibble(card = numbers) %>% separate(col = card, into = LETTERS[1:5], sep = " ")
    },
    calledNumber = function(calledNumber) {
      for(i in 1:NCOL(self$card)) {
        for(j in 1:NROW(self$card)) {
          if(self$card[[i]][[j]] == as.character(calledNumber)) {
            self$card[[i]][[j]] <- "X"
          }
        }
      }
    },
    checkBingoFunction = function(calledNumber) {
      bingo <- "XXXXX"
      sendBack <- FALSE
      # check if there is a bingo in the row
      temp <- self$card %>% unite('final', everything(), sep = "")
      for(i in 1:NROW(temp)) {
        if(temp$final[i] == bingo) {
          private$calcFinalAnswer(calledNumber)
          sendBack <-TRUE
        }  
      }
      # check for a bingo in a col
      for(i in 1:NCOL(self$card)) {
        if(paste0(self$card[[i]], collapse = "") == bingo) {
          private$calcFinalAnswer(calledNumber) 
          sendBack <- TRUE
        } 
      }
      #check for a bingo on the diagonal
      return(sendBack)
    }
  ),
  private = list(
    calcFinalAnswer = function(calledNumber) {
      weHaveABingo <- 0
      for(i in 1:NCOL(self$card)) {
        for(j in 1:NROW(self$card)) {
          if(self$card[[i]][[j]] != "X") {
            weHaveABingo <- weHaveABingo + as.numeric(self$card[[i]][[j]])
          }
        }
      }
      print(paste("BINGO!!! Final Answer=", weHaveABingo * calledNumber))
    }
  )
)

solve1 <- function(calledNumbers, cardData) {
  
  bingoCardList <- list()
  bingoCardNumber <- 0
  
  for(i in seq(from=1, to=length(cardData)-1, by=5)) {
    bingoCardNumber <- bingoCardNumber +1
    bingoCardList[[bingoCardNumber]] <- bingoCard$new(cardData[i:(i+4)])
  }
  
  
  for(a in 1:length(calledNumbers)) {
    bingo <- FALSE
    for(b in 1:length(bingoCardList)) {
      bingoCardList[[b]]$calledNumber(calledNumbers[a])
      bingo <- bingoCardList[[b]]$checkBingoFunction(calledNumbers[a])
      if(bingo) {
        break
      }
    }
    if(bingo) {
      break
    }
  }
  
}

solve1(calledNumbersDemo, bingoCardsDataDemo)

# bingoCardList <- list()
# bingoCardNumber <- 0
# 
# for(i in seq(from=1, to=length(bingoCardsData)-1, by=5)) {
#   bingoCardNumber <- bingoCardNumber +1
#   bingoCardList[[bingoCardNumber]] <- bingoCard$new(bingoCardsData[i:(i+4)])
# }
# 
# 
# for(a in 1:length(calledNumbers)) {
#   bingo <- FALSE
#   for(b in 1:length(bingoCardList)) {
#     bingoCardList[[b]]$calledNumber(calledNumbers[a])
#     bingo <- bingoCardList[[b]]$checkBingoFunction(calledNumbers[a])
#     if(bingo) {
#       break
#     }
#   }
#   if(bingo) {
#     break
#   }
# }

# onto problem 1

calledNumbers <- read_csv("day4/day4.csv", col_names = FALSE, n_max = 1) %>% 
  gather("all", 'somethingElse', everything()) %>% 
  pull(somethingElse)

bingoCardProbData <- read_csv('day4/day4.csv', col_names = FALSE, skip = 1) %>% 
  mutate(X1 = str_replace_all(X1, "  ", " ")) %>% 
  pull(X1)

solve1(calledNumbers, bingoCardProbData)

bingoCardList <- list()
bingoCardNumber <- 0

for(i in seq(from=1, to=length(bingoCardProbData)-4, by=5)) {
  bingoCardNumber <- bingoCardNumber +1
  bingoCardList[[bingoCardNumber]] <- bingoCard$new(bingoCardProbData[i:(i+4)])
}

for(a in 1:length(calledNumbers)) {
  bingo <- FALSE
  for(b in 1:length(bingoCardList)) {
    bingoCardList[[b]]$calledNumber(calledNumbers[a])
    bingo <- bingoCardList[[b]]$checkBingoFunction(calledNumbers[a])
    if(bingo) {
      break
    }
  }
  if(bingo) {
    break
  }
}

# problem 2 is find last winning board

solve2 <- function(calledNumbers, cardData) {
  
  prob2BingoCardList <- list()
  bingoCardNumber <- 0
  
  for(i in seq(from=1, to=length(cardData)-4, by=5)) {
    bingoCardNumber <- bingoCardNumber +1
    prob2BingoCardList[[bingoCardNumber]] <- bingoCard$new(cardData[i:(i+4)])
  }
  
  
  for(a in 1:length(calledNumbers)) {
    bingo <- FALSE
    boardsToRemove <- c()
    for(b in 1:length(prob2BingoCardList)) {
      prob2BingoCardList[[b]]$calledNumber(calledNumbers[a])
      bingo <- prob2BingoCardList[[b]]$checkBingoFunction(calledNumbers[a])
      if(bingo) {
        boardsToRemove <- c(boardsToRemove, b)
      }
    }
    if(length(boardsToRemove) > 0) {
      for(c in length(boardsToRemove):1) {
        prob2BingoCardList[boardsToRemove[[c]]] <- NULL
      }
      
      #print(paste("on called number =", a))
      #print(paste("number of boards left=", length(prob2BingoCardList)))
    }
    if(length(prob2BingoCardList) == 0) {
      break
    }
  }
  
  
  
}

solve2(calledNumbers, bingoCardProbData)
