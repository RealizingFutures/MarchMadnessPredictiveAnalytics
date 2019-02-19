#-------------------------------------------------------
# March Madness Predictive Anaytics Functions
#-------------------------------------------------------





#-------------------------------------------------------
# Setup
#-------------------------------------------------------



#-------------------------------------------------------
# GetNCAARankings Function
#-------------------------------------------------------

GetNCAARankings <- function(df.RegularSeasonCompactResults, df.Teams, myYear, myScoreCap, myLocMod
                            , myQ1Mod, myQ2Mod, myQ3Mod, myQ4Mod){
  # this function calculates and returns NCAA rankings 
  # for a given regular season and set of parameters        
  
  # load libraries
  require(dplyr)
  require(reshape2)
  require(MASS)
  
  
  # filter, mutate, and arrange dataset
  df.quarter <- df.RegularSeasonCompactResults %>%
    filter(Season == myYear)
  
  q1 <- (max(df.quarter$DayNum) - min(df.quarter$DayNum)) / 4
  q2 <- (max(df.quarter$DayNum) - min(df.quarter$DayNum)) / 2
  q3 <- ((max(df.quarter$DayNum) - min(df.quarter$DayNum)) / 2) + 
    ((max(df.quarter$DayNum) - min(df.quarter$DayNum)) / 4)
  
  
  # filter, mutate, and arrange dataset
  df.wins <- df.RegularSeasonCompactResults %>%
    filter(Season == myYear) %>%
    mutate(counter = 1) %>%
    mutate(ScoreDiff = WScore - LScore) %>%
    mutate(ScoreDiff = ifelse(ScoreDiff >= myScoreCap, myScoreCap, ScoreDiff)) %>%
    mutate(ScoreDiff = ifelse(WLoc == 'A|N', ScoreDiff * myLocMod, ScoreDiff))
  
  df.wins <- df.wins %>%
    mutate(ScoreDiff = 
             ifelse(DayNum <= q1
                    , ScoreDiff * myQ1Mod
                    , ifelse(DayNum <= q2
                             , ScoreDiff * myQ2Mod
                             , ifelse(DayNum <= q3
                                      , ScoreDiff * myQ3Mod
                                      , ScoreDiff * myQ4Mod)))) %>%
    group_by(WTeamID) %>%
    summarise(ScoreDiff = sum(ScoreDiff), Games = sum(counter)) %>%
    arrange(desc(ScoreDiff))
  
  # filter, mutate, and arrange dataset
  df.losses <- df.RegularSeasonCompactResults %>%
    filter(Season == myYear) %>%
    mutate(counter = 1) %>%
    mutate(ScoreDiff = LScore - WScore) %>%
    mutate(ScoreDiff = ifelse(ScoreDiff <= myScoreCap * -1, myScoreCap * -1, ScoreDiff)) %>%
    mutate(ScoreDiff = ifelse(WLoc == 'A|N', ScoreDiff * myLocMod, ScoreDiff))
  
  df.losses <- df.losses %>%
    mutate(ScoreDiff = 
             ifelse(DayNum <= q1
                    , ScoreDiff * myQ1Mod
                    , ifelse(DayNum <= q2
                             , ScoreDiff * myQ2Mod
                             , ifelse(DayNum <= q3
                                      , ScoreDiff * myQ3Mod
                                      , ScoreDiff * myQ4Mod)))) %>%
    group_by(LTeamID) %>%
    summarise(ScoreDiff = sum(ScoreDiff), Games = sum(counter)) %>%
    arrange(desc(ScoreDiff))        
  
  # change the WTeamID and LTeamID field names to be TeamID
  names(df.wins)[1] <- 'TeamID' 
  names(df.losses)[1] <- 'TeamID'
  
  # bind the wins and losses and then summarise final scores differences
  df.sum <- rbind(df.wins, df.losses)
  df.sum <- df.sum %>%
    group_by(TeamID) %>%
    summarise(ScoreDiff = sum(ScoreDiff))
  
  # create a data frame with teams and all zeroes to use as a 
  # base to build the score difference vector
  df.sum.base <- data.frame(TeamID = as.character(df.Teams$TeamID)
                            , ScoreDiff = as.numeric(0))
  rownames(df.sum.base) <- df.sum.base$TeamID
  
  # load the data frame with the cumulative score differences for each team
  for(i in 1:nrow(df.sum)){
    Tid <- as.character(df.sum[i, 1])
    TScoreDiff <- df.sum[i, 2]
    df.sum.base[Tid, 2] <- TScoreDiff
  }
  
  # create a data frame with all zeroes 
  # to use for building a matrix of matchups
  df.mat.base <- data.frame(WTeamID = as.character(df.Teams$TeamID)
                            , LTeamID = as.character(df.Teams$TeamID)
                            , Games = as.numeric(0))
  df.mat.base <- dcast(df.mat.base, WTeamID ~ LTeamID, fill = 0)
  rownames(df.mat.base) <- df.mat.base$WTeamID
  drops <- c('WTeamID')
  df.mat.base <- df.mat.base[ , !(names(df.mat.base) %in% drops)]
  
  # create a data frame with the count of games played by team
  df.games <- rbind(df.wins, df.losses)
  df.games <- df.games %>%
    group_by(TeamID) %>%
    summarise(Games = sum(Games)) 
  
  # load the matrix with the number of games played by each team 
  # corresponding to cells where teams intersect themselves
  for(i in 1:nrow(df.games)){
    Tid <- as.character(df.games[i, 1])
    Tgames <- df.games[i, 2]
    df.mat.base[rownames(df.mat.base) == Tid
                , colnames(df.mat.base) == Tid] <- Tgames
  }
  
  # filter, mutate, and arrange dataset
  df.matchups <- df.RegularSeasonCompactResults %>%
    filter(Season == myYear) %>%
    mutate(counter = -1) %>%
    group_by(WTeamID, LTeamID) %>%
    summarise(Matchups = sum(counter))
  
  # load the matrix based on the number of matchups between each team
  for(i in 1:nrow(df.matchups)){
    Wtid <- as.character(df.matchups[i, 1])
    Ltid <- as.character(df.matchups[i, 2])
    Tmatch <- df.matchups[i, 3]
    df.mat.base[rownames(df.mat.base) == Wtid
                , colnames(df.mat.base) == Ltid] <- Tmatch        
    df.mat.base[rownames(df.mat.base) == Ltid
                , colnames(df.mat.base) == Wtid] <- Tmatch                   
  }
  
  # change data frame into a matrix
  df.mat <- as.matrix(df.mat.base)
  
  # create rankings by calculating the inverse of the matrix
  # then multiplying the inverted matrix by the vector of 
  # cumulative score differences
  rankings <- round(ginv(df.mat) %*% df.sum.base[, 2], 3)
  
  # create a dataset with rankings and sort it
  df.rank <- cbind(df.Teams, rankings)
  df.rank <- arrange(df.rank, desc(rankings))
  
  return(df.rank)
  
}


#-------------------------------------------------------
# SimulateMarchMadness Function
#-------------------------------------------------------

SimulateMarchMadness <- function(df.seeds, df.slots, myData, myYear, myUpsets, coinFlip = FALSE){
  # this function takes some NCAA rankings data and 
  # uses it to predict March Madness brackets         
  
  
  
  # filter datasets for selected year
  df.seeds.curr <- df.seeds %>%
    filter(Season == myYear)
  df.slots.curr <- df.slots %>%
    filter(Season == myYear)
  
  # pick random upset games - but exclude the first seed games
  gamesFilter <- c(1:32)[-c(1, 9, 17, 25)]
  upsetGames <- sample(gamesFilter, myUpsets, replace=F)
  
  # create a NULL variable to hold the tournament outcomes
  df.tournament.sim <- NULL
  
  # loop to simulate tournament rounds
  for(r in 1:7){
    if(r == 1){
      df.seeds.round <- df.seeds.curr[grepl('a|b'
                                            , df.seeds.curr$Seed), ]
      names(df.seeds.round)[3] <- 'TeamID'
      df.seeds.round <- merge(df.seeds.round
                              , myData, sort = FALSE)
    } else if(r == 2){
      df.seeds.round <- df.seeds.curr[!grepl('a|b'
                                             , df.seeds.curr$Seed), ]
      names(df.seeds.round)[3] <- 'TeamID'
      df.seeds.round <- merge(df.seeds.round
                              , myData, sort = FALSE)   
      df.seeds.round <- rbind(df.seeds.round
                              , df.tournament.sim)  
      df.tournament.sim <- df.seeds.round
    } else {
      df.seeds.round <- df.tournament.sim
    }
    
    if(r == 1){
      for(i in 1:nrow(df.seeds.round)){
        if(i %% 2 != 0){
          t1 <- df.seeds.round[i, ]
          t2 <- df.seeds.round[i+1, ]
          if(coinFlip == TRUE){
            coin <- sample(1:2, 1, FALSE)
            if(coin == 2){
              df.tournament.sim <- 
                rbind(df.tournament.sim, t2)   
            } else {
              df.tournament.sim <- 
                rbind(df.tournament.sim, t1)                                      
            }
          }else if(t1$rankings >= t2$rankings){
            df.tournament.sim <- 
              rbind(df.tournament.sim, t1)   
          } else {
            df.tournament.sim <- 
              rbind(df.tournament.sim, t2)                                      
          }
        }
      }
      
      # remove a and b designations from the seed names
      df.tournament.sim$Seed <- gsub('a|b'
                                     , ''
                                     , df.tournament.sim$Seed)
      
    } else {
      df.slots.round <- df.slots.curr[grepl(paste('R', r-1, sep = '')
                                            , df.slots.curr$Slot), ]
      
      for(i in 1:nrow(df.slots.round)){
        strong <- as.character(df.slots.round$StrongSeed[i])
        weak <- as.character(df.slots.round$WeakSeed[i])
        slot <- as.character(df.slots.round$Slot[i])
        t1 <- df.seeds.round[df.seeds.round$Seed == strong, ]
        t2 <- df.seeds.round[df.seeds.round$Seed == weak, ]
        if(coinFlip == TRUE){
          coin <- sample(1:2, 1, FALSE)
          if(coin == 2){
            t2$Seed <- slot
            df.tournament.sim <- 
              rbind(df.tournament.sim, t2)    
          } else {
            t1$Seed <- slot
            df.tournament.sim <- 
              rbind(df.tournament.sim, t1)                                      
          }
        } else if(r == 2 && i %in% upsetGames){
          if(t1$rankings >= t2$rankings){
            t2$Seed <- slot
            df.tournament.sim <- 
              rbind(df.tournament.sim, t2)    
          } else {
            t1$Seed <- slot
            df.tournament.sim <- 
              rbind(df.tournament.sim, t1)                                      
          }
          
        } else {
          if(t1$rankings >= t2$rankings){
            t1$Seed <- slot
            df.tournament.sim <- 
              rbind(df.tournament.sim, t1)   
          } else {
            t2$Seed <- slot
            df.tournament.sim <- 
              rbind(df.tournament.sim, t2)                                      
          }
        }
      }
    }     
  }   
  
  # write.csv(df.tournament.sim
  #           ,paste('C:/Users/Jared/Documents/R/MarchMadness/Bracket_'
  #                  , myYear
  #                  ,'.csv'
  #                  , sep = ''))
  
  return(df.tournament.sim)
  
}


#-------------------------------------------------------
# MarchMadnessBracket Function
#-------------------------------------------------------

MarchMadnessBracket <- function(mySim, myYear){
  
  
  # load libaries
  require(TeachingDemos)
  
  tourn_order.r2 <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
  
  tourn.base <- mySim[1:64,3:5]
  tourn.base <- tourn.base[order(tourn.base$Seed),]
  
  tourn.r2.p1 <- tourn.base[1:16, ]
  tourn.r2.p1 <- tourn.r2.p1[tourn_order.r2, ]
  
  tourn.r2.p2 <- tourn.base[17:32, ]
  tourn.r2.p2 <- tourn.r2.p2[tourn_order.r2, ]
  
  tourn.r2.p3 <- tourn.base[33:48, ]
  tourn.r2.p3 <- tourn.r2.p3[tourn_order.r2, ]
  
  tourn.r2.p4 <- tourn.base[49:64, ]
  tourn.r2.p4 <- tourn.r2.p4[tourn_order.r2, ]
  
  tourn.r2 <- rbind(tourn.r2.p1, tourn.r2.p2, tourn.r2.p3, tourn.r2.p4)
  
  
  
  tourn_order.r3 <- c(1, 8, 5, 4, 6, 3, 7, 2)
  
  tourn.base <- mySim[65:96,3:5]
  
  tourn.r3.p1 <- tourn.base[1:8, ]
  tourn.r3.p1 <- tourn.r3.p1[tourn_order.r3, ]
  
  tourn.r3.p2 <- tourn.base[9:16, ]
  tourn.r3.p2 <- tourn.r3.p2[tourn_order.r3, ]
  
  tourn.r3.p3 <- tourn.base[17:24, ]
  tourn.r3.p3 <- tourn.r3.p3[tourn_order.r3, ]
  
  tourn.r3.p4 <- tourn.base[25:32, ]
  tourn.r3.p4 <- tourn.r3.p4[tourn_order.r3, ]
  
  tourn.r3 <- rbind(tourn.r3.p1, tourn.r3.p2, tourn.r3.p3, tourn.r3.p4)
  
  
  
  tourn_order.r4 <- c(1, 4, 3, 2)
  
  tourn.base <- mySim[97:112,3:5]
  
  tourn.r4.p1 <- tourn.base[1:4, ]
  tourn.r4.p1 <- tourn.r4.p1[tourn_order.r4, ]
  
  tourn.r4.p2 <- tourn.base[5:8, ]
  tourn.r4.p2 <- tourn.r4.p2[tourn_order.r4, ]
  
  tourn.r4.p3 <- tourn.base[9:12, ]
  tourn.r4.p3 <- tourn.r4.p3[tourn_order.r4, ]
  
  tourn.r4.p4 <- tourn.base[13:16, ]
  tourn.r4.p4 <- tourn.r4.p4[tourn_order.r4, ]
  
  tourn.r4 <- rbind(tourn.r4.p1, tourn.r4.p2, tourn.r4.p3, tourn.r4.p4)
  
  tourn.r5 <- mySim[113:120,3:5]
  tourn.r6 <- mySim[121:124,3:5]
  tourn.r7 <- mySim[125:126,3:5]
  tourn.ch <- mySim[127,3:5]
  
  par(mar=c(0,0,0,0) , bg="white" )
  plot(1,1,col="white",xlim=c(-5,130) , ylim=c(0,330) ,xaxt="n", yaxt="n" , bty="n" , xlab="" , ylab="")
  
  c <- 0.6
  myColor <- 'darkorange'
  
  myRow <- 0
  mySlots <- tourn.r2
  slotNum <- nrow(tourn.r2)
  for(i in 1:32){
    
    x0 <- 0; y0 <- myRow; x1 <- 10; y1 <- myRow
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x0, myRow + 3, labels = label, pos = 4, offset = 0, cex = c)
    
    x0 <- 120; y0 <- myRow; x1 <- 130; y1 <- myRow
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum - 32, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x1, myRow + 3, labels = label, pos = 2, offset = 0, cex = c)
    
    myRow <- myRow + 10
    slotNum <- slotNum - 1
  }
  
  
  myRow <- 0
  mySlots <- tourn.r3
  slotNum <- nrow(tourn.r3)
  for(i in 1:16){
    
    x0 <- 10; y0 <- myRow + 5; x1 <- 20; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x0 + 0.2, y0 + 3, labels = label, pos = 4, offset = 0, cex = c)
    
    x0 <- 10; y0 <- myRow; x1 <- 10; y1 <- myRow + 10
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    
    
    x0 <- 110; y0 <- myRow + 5; x1 <- 120; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum - 16, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x1 - 0.2, y0 + 3, labels = label, pos = 2, offset = 0, cex = c)
    
    x0 <- 120; y0 <- myRow; x1 <- 120; y1 <- myRow + 10
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd")) 
    
    slotNum <- slotNum - 1
    myRow <- myRow + 20
  }
  
  myRow <- 10
  mySlots <- tourn.r4
  slotNum <- nrow(tourn.r4)
  for(i in 1:8){
    
    x0 <- 20; y0 <- myRow + 5; x1 <- 30; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd")) 
    label <- mySlots[slotNum, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x0 + 0.2, y0 + 3, labels = label, offset = 0, pos = 4, cex = c)
    
    x0 <- 20; y0 <- myRow - 5; x1 <- 20; y1 <- myRow + 15
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    
    x0 <- 100; y0 <- myRow + 5; x1 <- 110; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum - 8, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x1 - 0.2, y0 + 3, labels = label, pos = 2, offset = 0, cex = c)
    
    x0 <- 110; y0 <- myRow - 5; x1 <- 110; y1 <- myRow + 15
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    
    
    slotNum <- slotNum - 1
    myRow <- myRow + 40
  }
  
  
  myRow <- 30
  mySlots <- tourn.r5
  slotNum <- nrow(tourn.r5)
  for(i in 1:4){
    
    x0 <- 30; y0 <- myRow + 5; x1 <- 40; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x0 + 0.2, y0 + 3, labels = label, offset = 0, pos = 4, cex = c)
    
    x0 <- 30; y0 <- myRow - 15; x1 <- 30; y1 <- myRow + 25
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    
    x0 <- 90; y0 <- myRow + 5; x1 <- 100; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum - 4, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x1 - 0.2, y0 + 3, labels = label, pos = 2, offset = 0, cex = c)
    
    x0 <- 100; y0 <- myRow - 15; x1 <- 100; y1 <- myRow + 25
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    
    
    slotNum <- slotNum - 1
    myRow <- myRow + 80
  }
  
  
  myRow <- 70
  mySlots <- tourn.r6
  slotNum <- nrow(tourn.r6)
  for(i in 1:2){
    
    x0 <- 40; y0 <- myRow + 5; x1 <- 50; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x0 + 0.2, y0 + 3, labels = label, offset = 0, pos = 4, cex = c)
    
    x0 <- 40; y0 <- myRow - 35; x1 <- 40; y1 <- myRow + 45
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    
    x0 <- 80; y0 <- myRow + 5; x1 <- 90; y1 <- myRow + 5
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    label <- mySlots[slotNum - 2, 2]
    #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
    text(x1 - 0.2, y0 + 3, labels = label, pos = 2, offset = 0, cex = c)
    
    x0 <- 90; y0 <- myRow - 35; x1 <- 90; y1 <- myRow + 45
    segments(x0, y0, x1, y1,
             col = myColor, lty = par("lty"), lwd = par("lwd"))  
    
    
    slotNum <- slotNum - 1
    myRow <- myRow + 160
  }
  
  
  mySlots <- tourn.r7
  
  x0 <- 50; y0 <- 210; x1 <- 60; y1 <- 210
  segments(x0, y0, x1, y1,
           col = myColor, lty = par("lty"), lwd = par("lwd"))  
  label <- mySlots[2, 2]
  #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
  text(x0 + 0.2, y0 + 3, labels = label, offset = 0, pos = 4, cex = c)
  
  x0 <- 70; y0 <- 100; x1 <- 80; y1 <- 100
  segments(x0, y0, x1, y1,
           col = myColor, lty = par("lty"), lwd = par("lwd"))  
  label <- mySlots[1, 2]
  #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
  text(x1 - 0.2, y0 + 3, labels = label, offset = 0, pos = 2, cex = c)
  
  
  myRow <- 70
  
  x0 <- 50; y0 <- myRow + 5; x1 <- 50; y1 <- myRow + 165
  segments(x0, y0, x1, y1,
           col = myColor, lty = par("lty"), lwd = par("lwd"))  
  
  x0 <- 80; y0 <- myRow + 5; x1 <- 80; y1 <- myRow + 165
  segments(x0, y0, x1, y1,
           col = myColor, lty = par("lty"), lwd = par("lwd"))  
  
  
  mySlots <- tourn.ch
  rect(52, 135, 78, 195, col="transparent", border="darkorange", lwd = par("lwd"))
  x0 <- 60; y0 <- 165; x1 <- 70; y1 <- 165
  segments(x0, y0, x1, y1,
           col = myColor, lty = par("lty"), lwd = par("lwd"))  
  label <- mySlots[1, 2]
  #c <- ifelse(nchar(as.character(label)) >= 11, 0.45, 0.6)
  text(x0 + 5, y0 + 3, labels = label, offset = 0, pos = 3, cex = 0.6)
  
  
  
  label = paste('NCAA\nMARCH MADNESS\n', myYear, sep = '')
  #text(x0 + 5, 300, labels = label, offset = 0, pos = 1, cex = 1, col = 'darkorange')
  shadowtext(x0 + 5, y = 300, labels = label, col = "darkorange"
             , bg = "black", theta = seq(pi/4, 2 * pi, length.out = 20), r = 0.1, cex = 1.5)
  
  
}




