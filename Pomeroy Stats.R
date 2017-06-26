stats2017 <- read.csv('~/Downloads/Pom2017.csv')
stats2016 <- read.csv('~/Downloads/Pom2016.csv')
stats2015 <- read.csv('~/Downloads/Pom2015.csv')
stats2014 <- read.csv('~/Downloads/Pom2014.csv')
stats2013 <- read.csv('~/Downloads/Pom2013.csv')
stats2012 <- read.csv('~/Downloads/Pom2012.csv')
stats2011 <- read.csv('~/Downloads/Pom2011.csv')
stats2010 <- read.csv('~/Downloads/Pom2010.csv')
stats2009 <- read.csv('~/Downloads/Pom2009.csv')
stats2008 <- read.csv('~/Downloads/Pom2008.csv')
stats2007 <- read.csv('~/Downloads/Pom2007.csv')
stats2006 <- read.csv('~/Downloads/Pom2006.csv')
stats2005 <- read.csv('~/Downloads/Pom2005.csv')
stats2004 <- read.csv('~/Downloads/Pom2004.csv')
stats2003 <- read.csv('~/Downloads/Pom2003.csv')


changeDat <- function(Dat, year){
  
  Dat$team <- gsub('[0,1,2,3,4,5,6,7,8,9]', "", Dat$team)
  Dat$team <- gsub('[.]', "", Dat$team)
#View(Dat)
  Dat <- Dat %>%
    mutate(team = ifelse(team == 'VCU', 'VA Commonwealth', team)) %>%
    mutate(team = ifelse(team == 'Green Bay', 'WI Green Bay', team)) %>%
    mutate(team = ifelse(team == "Saint Joseph's", "St Joseph's PA", team)) %>%
    mutate(team = ifelse(team == 'Southern', 'Southern Univ', team)) %>%
    mutate(team = ifelse(team == 'Middle Tennessee', 'MTSU', team)) %>%
    mutate(team = ifelse(team == 'North Carolina St', 'NC State', team)) %>%
    mutate(team = ifelse(team == 'Albany', 'Albany NY', team)) %>%
    mutate(team = ifelse(team == 'Coastal Carolina', 'Coastal Car', team)) %>%
    mutate(team = ifelse(team == 'Milwaukee', 'WI Milwaukee', team)) %>%
    mutate(team = ifelse(team == 'Cal Poly', 'Cal Poly SLO', team)) %>%
    mutate(team = ifelse(team == 'Louisiana Lafayette', 'ULL', team)) %>%
    mutate(team = ifelse(team == 'NorthCarolinaSt', 'NC State', team)) %>%
    mutate(team = ifelse(team == 'LIUBrooklyn', 'Long Island', team)) %>%
    mutate(team = ifelse(team == 'NorthwesternSt', 'Northwestern LA', team)) %>%
    mutate(team = ifelse(team == 'WesternKentucky', 'WKU', team)) %>%
    mutate(team = ifelse(team == 'MiddleTennessee', 'MTSU', team)) %>%
    mutate(team = ifelse(team == "SaintMary's", "St Mary's CA", team)) %>%
    mutate(team = ifelse(team == "UTSA", "UT San Antonio", team)) %>%
    mutate(team = ifelse(team == "UCSantaBarbara", "Santa Barbara", team)) %>%
    mutate(team = ifelse(team == "BostonUniversity", "Boston Univ", team)) %>%
    mutate(team = ifelse(team == "EastTennesseeSt", "ETSU", team)) %>%
    mutate(team = ifelse(team == "American", "American Univ", team)) %>%
    mutate(team = ifelse(team == "SaintJoseph's", "St Joseph's PA", team)) %>%
    mutate(team = ifelse(team == "CentralConnecticut", "Central Conn", team)) %>%
    mutate(team = ifelse(team == "TexasA&MCorpusChris", "TAM C. Christi", team)) %>%
    mutate(team = ifelse(team == "Monmouth", "Monmouth NJ", team)) %>%
    mutate(team = ifelse(team == "LouisianaLafayette", "ULL", team)) %>%
    mutate(team = ifelse(team == "TroySt", "Troy", team))
  
  
  Dat$First <- substring(Dat$team, 1,1)
  Dat$Last <- str_sub(Dat$team,-1,-1)
  
  Teams$First <- substring(Teams$Team_Name, 1,1)
  Teams$Last <- str_sub(Teams$Team_Name,-1,-1)
  
  TourneyInfo <- TourneySeeds %>% filter(Season == year) %>% 
    left_join(Teams, by = c('team' = 'team')) %>%
    left_join(Dat, by = c('Seed2' = 'Seed', 'First', 'Last')) %>%
    select(-First, -Last, -team.y, -Seed)
}

############################ Data ##################################
Y2017 <- changeDat(Dat = stats2017, year = 2017) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2016 <- changeDat(Dat = stats2016, year = 2016) %>% 
 distinct(Team_Name, .keep_all = TRUE)
Y2015 <- changeDat(Dat = stats2015, year = 2015) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2014 <- changeDat(Dat = stats2014, year = 2014) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2013 <- changeDat(Dat = stats2013, year = 2013) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2012 <- changeDat(Dat = stats2012, year = 2012) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2011 <- changeDat(Dat = stats2011, year = 2011) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2010 <- changeDat(Dat = stats2010, year = 2010) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2009 <- changeDat(Dat = stats2009, year = 2009) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2008 <- changeDat(Dat = stats2008, year = 2008) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2007 <- changeDat(Dat = stats2007, year = 2007) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2006 <- changeDat(Dat = stats2006, year = 2006) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2005 <- changeDat(Dat = stats2005, year = 2005) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2004 <- changeDat(Dat = stats2004, year = 2004) %>% 
  distinct(Team_Name, .keep_all = TRUE)
Y2003 <- changeDat(Dat = stats2003, year = 2003) %>% 
  distinct(Team_Name, .keep_all = TRUE)
#############################################################
PomeroyStats <- rbind(Y2003, Y2004, Y2005,Y2006, Y2007, Y2008, Y2009,
                      Y2010, Y2011, Y2012,Y2013, Y2014, Y2015, Y2016, Y2017)

View(PomeroyStats)
