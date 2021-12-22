# day 21

library(tidyverse)

##Player 1 starting position: 4
# Player 2 starting position: 3

solve1 <- function(p1, p2) {
  
  dice <- rep(c(1:100), 1000)
  p1Pos <- p1
  p1Score <- 0
  p2Score <- 0
  p2Pos <- p2
  dieRolls <- 1
  
  while(p1Score < 1000 & p2Score < 1000) {
    
    roll <- sum(dice[dieRolls:(dieRolls+2)])
    # play1 turn
    if(dieRolls %% 2 != 0) {
      p1Pos <- roll + p1Pos
      if(p1Pos > 10) {
        p1Pos <- p1Pos %% 10
        if(p1Pos == 0) {
          p1Pos <- 10
        }
      }
      p1Score <- p1Score + p1Pos

      dieRolls <- dieRolls + 3
      # player 2 rolls
    } else {
      p2Pos <- roll + p2Pos
      if(p2Pos > 10) {
        p2Pos <- p2Pos %% 10
        if(p2Pos == 0) {
          p2Pos <- 10
        }
      }
      p2Score <- p2Score + p2Pos
      
      dieRolls <- dieRolls + 3
      
      
    }
  }
  print(paste('dierolls: ', dieRolls, "p1score: ", p1Score, 'p2score:', p2Score))
  return((dieRolls-1) * (min(p1Score, p2Score)))
  
}

# debugonce(solve1)
solve1(4,8)
solve1(4,3)
