library(glmnet)
library(randomForest)
library(dplyr)
RidgeFunc <- function(year){
TrainDat <- data.matrix(DiffDat %>% filter(Season != year) %>%
                          select(-Season, -team, -team2, - Seed1, -Seed2, -Round, -DPts,
                                 -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
                                 -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DOutcome))
  #%>%
   # select(-DSeed))
  #select(Round, DSeed, DRecord, DOutcome)

TrainOutcome <- unlist(DiffDat %>% filter(Season != year) %>%
                         select(DOutcome))

###### The Outcome of the trained model

cvfit <- cv.glmnet(TrainDat, TrainOutcome, type.measure = "class", family = 'binomial',
                   alpha = 0)

coef(cvfit, s = cvfit$lambda.1se )

PredData <- DiffDat %>%filter(Season == year) %>%
  select(-Season, -team, -team2, - Seed1, -Seed2, -Round, -DPts,
         -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
         -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DOutcome)#, -DSeed)
### Predicted values
Pred <- predict(cvfit, data.matrix(PredData), s = cvfit$lambda.min, type="response")

Results <- DiffDat %>% filter(Season == year) %>% 
  select(DOutcome, Seed1,team, Seed2, team2) %>%
  mutate("Pred" = Pred, "PredOutcome" = ifelse(Pred > .5, 1, 0)) %>%
  mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
  left_join(Teams, by = c('team')) %>%
  left_join(Teams, by = c('team2' = 'team'))


PredRight <- Results %>% summarize(mean(RightWrong))

return(list("Pred" = PredRight, "Results" = Results))
}

Y2013 <- RidgeFunc(year = 2016)
Y2013$Pred
View(Y2013$Results)
cor(TrainDat)


#######################################################################
LogisticFunc <- function(year){

   TrainDat <- PrevYears$DiffDat %>% filter(Season != year) %>%
     filter(DSeedN != -15 | DSeedN != 15, DSeedN != -13 | DSeedN != 13,
            DSeedN != -11 | DSeedN != 11, DSeedN != 9 | DSeedN != -9) %>%
   # select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
    #       -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
     #      -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -Round, -DSeedN,
      #     -Dratio1, -Dratio2, -DAst, -DTO, -DSTL) 
     select(DOutcome, DLast10, DTPercent, DTGames, DAdjO, DAdjD, Dratio3)
     #mutate(ratio1 = DAdjO/(DOAdjD + .001), ratio2 = DAdjD/(DOAdjO+.001) )
   #colnames(TrainDat)

   #vars <- c( "DFG3", "DAst", "DTO", "DSTL", "DLast10", 
    #         "DTPercent", "DTGames", "DAdjO", "DAdjD", "DAdjT", "DOAdjO", 
     #        "DOAdjD")
    #filter(DSeed != -13 , DSeed != 13, DSeed != 12, DSeed != -12,
     #      DSeed != 11, DSeed != -11, DSeed != 10, DSeed != -10, 
      #     DSeed != 9, DSeed != -9)
  LogisticMod <- glm(DOutcome ~.  , family = 'binomial', data = TrainDat)
  
  #cor(TrainDat)
  #GLogisticMod <- glmer(paste('DOutcome ~', paste(vars, collapse="+"), '+(1 | Round)'),
   #                     family = 'binomial', data = TrainDat)
  #summary(GLogisticMod)
  summary(LogisticMod)
  #LogisticMod$coefficients
  PredData <- PrevYears$DiffDat %>%filter(Season == year) %>%
    #select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
     #      -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
      #     -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -DSeedN, -DOutcome) 
  select(DLast10, DTPercent, DTGames, DAdjO, DAdjD, Dratio3)
    #mutate(ratio1 = DAdjO/(DOAdjD + .001), ratio2 = DAdjD/(DOAdjO+.001) )
   
    
  ### Predicted values
  Pred <- predict(LogisticMod, PredData, type = 'response')
  #Pred2 <- predict(GLogisticMod, PredData, type = 'response', allow.new.levels=TRUE)
  
  Results <- PrevYears$DiffDat %>% filter(Season == year) %>% 
    select(DOutcome, Seed1,team, Seed2, team2) %>%
    mutate("Pred" = Pred, 
           "PredOutcome" = ifelse(Pred > 0.5, 1, 0)) %>%
    mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
    left_join(Teams, by = c('team')) %>%
    left_join(Teams, by = c('team2' = 'team'))
  
  PredRight <- Results %>% summarize(mean(RightWrong))
  
 
 
  ###### Getting Predictions
  DatPred <- DiffCombo %>% ungroup() %>% filter(Season == year) %>%
    #select(-Season, -team, -team2, -Seed1, -Seed2) 
    select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
           -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
           -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -DSeedN,
           -Dratio1, -Dratio2, -DAst, -DTO, -DSTL)
    #mutate(ratio1 = DAdjO/(DOAdjD + .001), ratio2 = DAdjD/(DOAdjO+.001) )
  Pred <- data.frame(pred = predict(LogisticMod, DatPred, type = 'response'))
  
  ###### Combining Predictions and data
  AttachDat <- DiffCombo %>% ungroup() %>% filter(Season == year)
  
  PredOutput <- bind_cols(AttachDat, Pred) %>% 
    unite("id", Season:team2, sep = '_') %>%
    select(id, pred)
  
  return(list("Pred" = PredRight,
              "Results" = Results, "PredOutput" = PredOutput))
}

ForestFunc <- function(year){

  TrainDat <- PrevYears$DiffDat %>% filter(Season != year) %>%
    filter(DSeedN != -15 | DSeedN != 15, DSeedN != -13 | DSeedN != 13,
           DSeedN != -11 | DSeedN != 11, DSeedN != 9 | DSeedN != -9) %>%
     select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
           -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
          -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -Round, -DSeedN,
         -Dratio1, -Dratio2, -DAst, -DTO, -DSTL) 
    #select(DOutcome, DLast10, DTPercent, DTGames, DAdjO, DAdjD, Dratio3)
  #mutate(ratio1 = DAdjO/(DOAdjD + .001), ratio2 = DAdjD/(DOAdjO+.001) )
  #colnames(TrainDat)
  
  #vars <- c( "DFG3", "DAst", "DTO", "DSTL", "DLast10", 
  #         "DTPercent", "DTGames", "DAdjO", "DAdjD", "DAdjT", "DOAdjO", 
  #        "DOAdjD")
  #filter(DSeed != -13 , DSeed != 13, DSeed != 12, DSeed != -12,
  #      DSeed != 11, DSeed != -11, DSeed != 10, DSeed != -10, 
  #     DSeed != 9, DSeed != -9)
  #LogisticMod <- glm(DOutcome ~.  , family = 'binomial', data = TrainDat)
  X <- (TrainDat %>% select(-DOutcome))
  Y <- as.factor(unlist(TrainDat %>% select(DOutcome)))
  
  RandFor <- randomForest(x = X ,y = Y, importance = TRUE, ntree = 500, mtry = 7)
  #RandFor$importance
  
  #cor(TrainDat)
  #GLogisticMod <- glmer(paste('DOutcome ~', paste(vars, collapse="+"), '+(1 | Round)'),
  #                     family = 'binomial', data = TrainDat)
  #summary(GLogisticMod)
  #summary(LogisticMod)
  #LogisticMod$coefficients
  PredData <- PrevYears$DiffDat %>%filter(Season == year) %>%
    select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
          -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
         -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -DSeedN, -DOutcome) 
    #select(DLast10, DTPercent, DTGames, DAdjO, DAdjD, Dratio3)
  #mutate(ratio1 = DAdjO/(DOAdjD + .001), ratio2 = DAdjD/(DOAdjO+.001) )
  
  
  ### Predicted values
  Pred <- predict(RandFor, PredData, type = 'response')
  #Pred2 <- predict(GLogisticMod, PredData, type = 'response', allow.new.levels=TRUE)
  
  Results <- PrevYears$DiffDat %>% filter(Season == year) %>% 
    select(DOutcome, Seed1,team, Seed2, team2) %>%
    mutate("PredOutcome" = Pred) %>%
   mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
    left_join(Teams, by = c('team')) %>%
    left_join(Teams, by = c('team2' = 'team'))
  
  PredRight <- Results %>% summarize(mean(RightWrong))
  
  

  return(list("Pred" = PredRight,
              "Results" = Results))
}

R2013 <- ForestFunc(year = 2016)
L2013 <- LogisticFunc(year = 2016)
R2013$Pred
L2013$Pred
View(L2013$Results)
library(pROC)
foo <- roc(L2013$Results$DOutcome, L2013$Results$Pred)
plot(foo)
N2013 <- LogisticFunc(year = 2013)$PredOutput
N2014 <- LogisticFunc(year = 2014)$PredOutput
N2015 <- LogisticFunc(year = 2015)$PredOutput
N2016 <- LogisticFunc(year = 2016)$PredOutput

Output <- rbind(N2013, N2014, N2015, N2016)
View(Output %>% filter(is.na(pred)))
write.csv(Output, "~/Downloads/TourneyAttempt11", row.names = FALSE)




###################### Predicting 2017 #######################
##### Combining tournament data with summary season data
### Adding Pomeroy stats from Pomeroy Tab
OverallDat2 <- SumSeasonDat %>% filter(Season == 2017) %>% 
  inner_join(TourneySeeds, by = c('Season', 'team')) %>%
  mutate(SeedLoc = gsub("[0,1,2,3,4,5,6,7,8,9, a, b, c, d]", "", Seed)) %>%
  left_join(PomeroyStats, by = c('Season','team' = 'team.x', 'Seed2')) %>%
  select(-Team_Name)


LogisticFunc2017 <- function(year){

  TrainDat <- PrevYears$DiffDat %>% filter(Season != year) %>%
    filter(DSeedN != -15 | DSeedN != 15, DSeedN != -13 | DSeedN != 13,
           DSeedN != -11 | DSeedN != 11, DSeedN != 9 | DSeedN != -9) %>%
    select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
          -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
           -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -Round, -DSeedN,
           -Dratio1, -Dratio2, -DAst, -DTO, -DSTL) 
  #select(DOutcome, DLast10, DTPercent, DTGames, DAdjO, DAdjD, Dratio3)
  #mutate(ratio1 = DAdjO/(DOAdjD + .001), ratio2 = DAdjD/(DOAdjO+.001) )
  #colnames(TrainDat)
  
  #vars <- c( "DFG3", "DAst", "DTO", "DSTL", "DLast10", 
  #         "DTPercent", "DTGames", "DAdjO", "DAdjD", "DAdjT", "DOAdjO", 
  #        "DOAdjD")
  #filter(DSeed != -13 , DSeed != 13, DSeed != 12, DSeed != -12,
  #      DSeed != 11, DSeed != -11, DSeed != 10, DSeed != -10, 
  #     DSeed != 9, DSeed != -9)
  LogisticMod <- glm(DOutcome ~.  , family = 'binomial', data = TrainDat)
  
  #cor(TrainDat)
  #GLogisticMod <- glmer(paste('DOutcome ~', paste(vars, collapse="+"), '+(1 | Round)'),
  #                     family = 'binomial', data = TrainDat)
  #summary(GLogisticMod)
  #summary(LogisticMod)
  
  ###### Getting Predictions
  #View(Year2017 %>% filter(Season == 2017, team == 1276))
  DatPred <- Year2017 %>% ungroup() %>% filter(Season == year) %>%
    #select(-Season, -team, -team2, -Seed1, -Seed2) 
    select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
           -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
           -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -DSeedN,
           -Dratio1, -Dratio2, -DAst, -DTO, -DSTL)
  #select(DOutcome, DLast10, DTPercent, DTGames, DAdjO, DAdjD, Dratio3)
  #mutate(ratio1 = DAdjO/(DOAdjD + .001), ratio2 = DAdjD/(DOAdjO+.001) )
  Pred <- data.frame(pred = predict(LogisticMod, DatPred, type = 'response'))
  
  ###### Combining Predictions and data
  AttachDat <- Year2017 %>% ungroup() %>% filter(Season == year)
  
  PredDat <- bind_cols(AttachDat, Pred) %>%
    select(team, team2, Round, Seed1, Seed2, pred) %>%
    left_join(Teams, by = c('team')) %>%
    left_join(Teams, by = c('team2' = 'team')) %>%
    #select(-First.x, -First.y, -Last.x, -Last.y, -team, -team2) %>%
    arrange(Team_Name.x,Team_Name.y, Round)
  
  PredOutput <- bind_cols(AttachDat, Pred) %>% 
    unite("id", Season:team2, sep = '_') %>%
    select(id, pred)
  
  return(list("Pred" = PredRight,
              "Results" = Results, "PredOutput" = PredOutput, 'PredDat' = PredDat))
}


Attempt1 <- LogisticFunc2017(year = 2017)
View(Attempt1$PredDat %>% filter(Team_Name.x == "Providence" | Team_Name.y == "Providence"))
View(Attempt1$PredOutput)
write.csv(Attempt1$PredOutput, '~/Downloads/Tourney1.csv', row.names = FALSE)

View(Y2017 %>% filter(Season == 2017))
View(OverallDat2 %>% filter(Season == 2017, team == 1388))
View(OverallDat2 %>% filter(Season == 2017, team == 1433))
