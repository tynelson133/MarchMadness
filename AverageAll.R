library(lme4)

MultiLevelMod <- function(year){
TrainDat <- DiffDat %>% filter(Season != year) %>%
  select(-Season, -team, -team2, - Seed1, -Seed2) 
  cor(TrainDat)
#GLogisticMod <- glmer(DOutcome ~ DSeed + DPts + DsdPts + DFG  + DFG3 +
 #                       DFT + DOR + DDR + DAst + DTO  + 
  #                      DLast10 + DRecord + DOppRec + DPtDiff +
   #                     DTPercent*DTGames + 
    #                    (1 | Round), family = 'binomial', data = TrainDat)

GLogisticMod <- glmer(DOutcome ~ Round + DOR + DLast10 + DFG + DDR + DAst + DBLK +
                        DRecord + DOppRec + DPtDiff +
                        DTPercent*DTGames +
                        (1 | DSeed), family = 'binomial', data = TrainDat)
#summary(GLogisticMod)
#LogisticMod$coefficients
PredData <- DiffDat %>%filter(Season == year) %>%
  select(-Season, -DOutcome, -team, -team2, -Seed1, - Seed2)
#select(-Season, -team, -team2, - Seed1, -Seed2, -DPts, -DRecord,
#      -DPF, -DSTL, -DFG3, -DAst, -DFT, -DOR, -DOutcome)

### Predicted values
Pred <- predict(GLogisticMod, PredData, type = 'response', allow.new.levels=TRUE)

Results <- DiffDat %>% filter(Season == year) %>% 
  select(DOutcome, Seed1,team, Seed2, team2) %>%
  mutate("Pred" = Pred, "PredOutcome" = ifelse(Pred > .5, 1, 0)) %>%
  mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
  left_join(Teams, by = c('team')) %>%
  left_join(Teams, by = c('team2' = 'team'))

PredRight <- Results %>% summarize(mean(RightWrong))

###### Getting Predictions
DatPred <- DiffCombo %>% ungroup() %>% filter(Season == year) %>%
  select(-Season, -team, -team2, -Seed1, -Seed2)
Pred <- data.frame(pred = predict(GLogisticMod, DatPred, type = 'response',allow.new.levels=TRUE))

###### Combining Predictions and data
AttachDat <- DiffCombo %>% ungroup() %>% filter(Season == year)

PredOutput <- bind_cols(AttachDat, Pred) %>% 
  unite("id", Season:team2, sep = '_') %>%
  select(id, pred)

return(list("Pred" = PredRight, "Results" = Results, "PredOutput" = PredOutput))

}

Go <- MultiLevelMod(year = 2011)
Go$Pred
View(Go$Results)


################### Ridge Func ######################
RidgeFunc <- function(year){
  TrainDat <- data.matrix(DiffDat %>% filter(Season != year) %>%
                            #select(-Season, -team, -team2, - Seed1, -Seed2, -DOutcome,
                             #      -DSeed)
                          select(Round, DOR, DLast10, DRecord, DOppRec, 
                                 DPtDiff, DTPercent, DTGames, DFG, DDR, DAst, DBLK))
  
  TrainOutcome <- unlist(DiffDat %>% filter(Season != year) %>%
                           select(DOutcome))
  
  ###### The Outcome of the trained model
  
  cvfit <- cv.glmnet(TrainDat, TrainOutcome, type.measure = "class", family = 'binomial',
                     alpha = 0)
  
  coef(cvfit, s = cvfit$lambda.1se )
  
  PredData <- DiffDat %>%filter(Season == year) %>%
    #select(-Season, -DOutcome, -team, -team2, -Seed1, - Seed2, -DSeed)
    select(Round, DOR, DLast10, DRecord, DOppRec, 
           DPtDiff, DTPercent, DTGames, DFG, DDR, DAst, DBLK)
  ### Predicted values
  Pred <- predict(cvfit, data.matrix(PredData), s = cvfit$lambda.min, type="response")
  
  Results <- DiffDat %>% filter(Season == year) %>% 
    select(DOutcome, Seed1,team, Seed2, team2) %>%
    mutate("Pred" = Pred, "PredOutcome" = ifelse(Pred > .5, 1, 0)) %>%
    mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
    left_join(Teams, by = c('team')) %>%
    left_join(Teams, by = c('team2' = 'team'))
  
  
  PredRight <- Results %>% summarize(mean(RightWrong))
  
  
  ###### Getting Predictions
  DatPred <- DiffCombo %>% ungroup() %>% filter(Season == year) %>%
    #select(-Season, -team, -team2, -Seed1, -Seed2)
    select(Round, DOR, DLast10, DRecord, DOppRec, 
           DPtDiff, DTPercent, DTGames, DFG, DDR, DAst, DBLK)
  
  Pred <- data.frame(pred = predict(cvfit, data.matrix(DatPred), type = 'response'))
  
  ###### Combining Predictions and data
  AttachDat <- DiffCombo %>% ungroup() %>% filter(Season == year)
  
  PredOutput <- bind_cols(AttachDat, Pred) %>% 
    unite("id", Season:team2, sep = '_') %>%
    select(id, X1)
  
  
  return(list("Pred" = PredRight, "Results" = Results, 'PredOutput' = PredOutput))
}

Y2013 <- RidgeFunc(year = 2014)
Y2013$Pred
View(Y2013$Results)
cor(TrainDat)


#######################################################################
LogisticFunc <- function(year){
  
  TrainDat <- DiffDat %>% filter(Season != year) %>%
    #select(-Season, -team, -team2, - Seed1, -Seed2, -DSeed) 
    select(Round, DOR, DLast10, DRecord, DOppRec, 
           DPtDiff, DTPercent, DTGames, DOutcome, DFG, DDR, DAst, DBLK)
  
  #%>% 
  #filter(DSeed != -13 , DSeed != 13, DSeed != 12, DSeed != -12,
  #      DSeed != 11, DSeed != -11, DSeed != 10, DSeed != -10, 
  #     DSeed != 9, DSeed != -9)
  LogisticMod <- glm(DOutcome ~., family = 'binomial', data = TrainDat)
  #summary(LogisticMod)
  #LogisticMod$coefficients
  PredData <- DiffDat %>%filter(Season == year) %>%
    #select(-Season, -DOutcome, -team, -team2, -Seed1, - Seed2, -DSeed)
    select(Round, DOR, DLast10, DRecord, DOppRec, 
           DPtDiff, DTPercent, DTGames, DFG, DDR, DAst, DBLK)
  #select(-Season, -team, -team2, - Seed1, -Seed2, -DPts, -DRecord,
  #      -DPF, -DSTL, -DFG3, -DAst, -DFT, -DOR, -DOutcome)
  
  ### Predicted values
  Pred <- predict(LogisticMod, PredData, type = 'response')
  
  Results <- DiffDat %>% filter(Season == year) %>% 
    select(DOutcome, Seed1,team, Seed2, team2) %>%
    mutate("Pred" = Pred, "PredOutcome" = ifelse(Pred > .5, 1, 0)) %>%
    mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
    left_join(Teams, by = c('team')) %>%
    left_join(Teams, by = c('team2' = 'team'))
  
  PredRight <- Results %>% summarize(mean(RightWrong))
  
  
  
  ###### Getting Predictions
  DatPred <- DiffCombo %>% ungroup() %>% filter(Season == year) %>%
    select(-Season, -team, -team2, -Seed1, -Seed2, -DSeed)
  Pred <- data.frame(pred = predict(LogisticMod, DatPred, type = 'response'))
  
  ###### Combining Predictions and data
  AttachDat <- DiffCombo %>% ungroup() %>% filter(Season == year)
  
  PredOutput <- bind_cols(AttachDat, Pred) %>% 
    unite("id", Season:team2, sep = '_') %>%
    select(id, pred)
  
  return(list("Pred" = PredRight, "Results" = Results, "PredOutput" = PredOutput))
}

AllFunc1 <- function(year){
R2013 <- RidgeFunc(year = year)
L2013 <- LogisticFunc(year = year)
Go <- MultiLevelMod(year = year)

DatR <- R2013$Results
DatL <- L2013$Results
DatM <- Go$Results

BringTogether <- DatR %>% left_join(DatL, by = c("team", 'team2', 'Seed1', 'Seed2',
                                                 'Team_Name.x', 'Team_Name.y')) %>%
  left_join(DatM, by = c("team", 'team2', 'Seed1', 'Seed2',
                         'Team_Name.x', 'Team_Name.y'))


BringTogether2 <- BringTogether %>% mutate('AvgPred' = (Pred.x + Pred.y + Pred)/3) %>%
  mutate(AvgOutcome = ifelse(AvgPred > .5, 1, 0), 
         AvgRight = ifelse(DOutcome.x == AvgOutcome, 1, 0))
View(BringTogether2)
PR <- mean(BringTogether2$AvgRight)
PL <- mean(BringTogether$RightWrong.x)
PM <- mean(BringTogether2$RightWrong.y)
PA <- mean(BringTogether2$RightWrong)

return(c(PR,PL, PM, PA))

}
AllFunc1(year = 2014)

Looking <- BringTogether2 %>% select(DOutcome.x, AvgOutcome, Seed1, Team_Name.x, Seed2,
                                     Team_Name.y, Pred.x, Pred.y, Pred, AvgPred)

View(Looking)


######################### Putting together #############################
AllFunc <- function(year){

  Ridge <- RidgeFunc(year = year)$PredOutput
  Logit <- LogisticFunc(year = year)$PredOutput
  Multi <- MultiLevelMod(year = year)$PredOutput
  

SubmitTogether <- Ridge %>% left_join(Logit, by = 'id') %>%
  left_join(Multi, by = 'id' )

SubmitTogether2 <- SubmitTogether %>% mutate('pred' = (X1 + pred.x + pred.y)/3) %>%
  select(id, pred)
 
return(SubmitTogether2)
}

N2013 <- AllFunc(year = 2013)
N2014 <- AllFunc(year = 2014)
N2015 <- AllFunc(year = 2015)
N2016 <- AllFunc(year = 2016)

Output <- rbind(N2013, N2014, N2015, N2016)
View(Output %>% filter(is.na(pred)))
write.csv(Output, "~/Downloads/TourneyAttempt6", row.names = FALSE)

