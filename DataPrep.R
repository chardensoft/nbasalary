##Data Prep
library(readxl)
library(sqldf)
library(dplyr)
library(stringr)
rm(list = ls())

##ELO PREP##

full_elo <- read.csv(file = "elo.csv")
full_elo <- full_elo[c(5, 9:10, 13)]
names(full_elo)[1] <- "year_end"

full_elo$fran_id <- ifelse(full_elo$team_id=="CHH", "Hornets", full_elo$fran_id)
full_elo$team_id <- ifelse(full_elo$fran_id=="Hornets", "CHA", full_elo$team_id)
full_elo$team_id <- ifelse(full_elo$fran_id=="Pelicans", "NOP", full_elo$team_id)
full_elo$team_id <- ifelse(full_elo$fran_id=="Nets", "BKN", full_elo$team_id)
full_elo$team_id <- ifelse(full_elo$fran_id=="Wizards", "WAS", full_elo$team_id)
full_elo$team_id <- ifelse(full_elo$fran_id=="Thunder", "OKC", full_elo$team_id)
full_elo$team_id <- ifelse(full_elo$fran_id=="Grizzlies", "MEM", full_elo$team_id)

write.csv(full_elo, "elo_partialprep.csv", row.names = FALSE)

elo <- sqldf('SELECT *, MAX(elo_n) AS MaxElo 
             FROM full_elo 
             GROUP BY year_end, team_id')

elo <- elo[c(1:3, 5)]

write.csv(elo, "elo_prepped.csv", row.names = FALSE)
rm(list = ls())

##Salary Prep##

full_salary <- read.csv(file = "Past_salaries.csv")
full_salary <- full_salary[c(2:3, 5:7)]
names(full_salary) <- c("player", "salary", "year_end", "team_id", "fran_id")

full_salary <- full_salary %>% 
  mutate(fran_id = str_replace(fran_id, ".*\\s", ""))
  
full_salary$salary <- full_salary$salary %>% 
  str_replace_all(c("[$]" = "", "," = ""))

full_salary$salary <- as.numeric(full_salary$salary)

full_salary$fran_id <- ifelse(full_salary$fran_id=="Caveliers", "Cavaliers", full_salary$fran_id)
full_salary$fran_id <- ifelse(full_salary$fran_id=="Blazers", "Trailblazers", full_salary$fran_id)
full_salary$fran_id <- ifelse(full_salary$fran_id=="76ers", "Sixers", full_salary$fran_id)
full_salary$fran_id <- ifelse(full_salary$fran_id=="Celtic", "Celtics", full_salary$fran_id)
full_salary$team_id <- ifelse(full_salary$team_id=="NOH", "NOP", full_salary$team_id)
full_salary$team_id <- ifelse(full_salary$team_id=="NJN", "BKN", full_salary$team_id)
full_salary$fran_id <- ifelse(full_salary$team_id=="TOR", "Raptors", full_salary$fran_id)

cap <- read.csv("cap.csv")

cap$sal <- cap$sal %>% 
  str_replace_all(c("[$]" = "", "," = ""))

cap$sal <- as.numeric(cap$sal)

for (i in 1:length(full_salary$player)) {
  full_salary$cap[i] <- cap$sal[which(cap$year_end == full_salary$year_end[i])]
}

full_salary$per_cap <- full_salary$salary / full_salary$cap
full_salary$sal_2020 <- full_salary$per_cap * cap$sal[36]

write.csv(full_salary, "salary_partialprep.csv", row.names = FALSE)

salary <- sqldf('SELECT year_end, team_id, fran_id, SUM(salary) AS tot_sal, AVG(salary) AS avg_sal, 
                      MAX(salary) AS max_sal, MIN(salary) AS min_sal, STDEV(salary) AS std_sal
                FROM full_salary 
                GROUP BY year_end, team_id')

write.csv(salary, "salary_prepped.csv", row.names = FALSE)
rm(list = ls())

### Player Prep ###

full_players <- read.csv(file = "Player_stats.csv")
full_players <- full_players[c(2:21, 23:26, 28:53)]
names(full_players)[c(1:5)] <- c("year_end", "player", "pos", "age", "team_id")

full_players$team_id <- ifelse(full_players$team_id=="WSB", "WAS", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="CHH", "CHA", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="SEA", "OKC", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="NJN", "BKN", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="NOH", "NOP", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="NOK", "NOP", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="BRK", "BKN", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="CHO", "CHA", full_players$team_id)
full_players$team_id <- ifelse(full_players$team_id=="VAN", "MEM", full_players$team_id)

rows_to_inspect <- which(full_players$team_id == "TOT")
rows_to_remove <- c()

for (i in 1:length(rows_to_inspect)) {
  year <- full_players$year_end[rows_to_inspect[i]]
  player <- full_players$player[rows_to_inspect[i]]
  for (j in rows_to_inspect[i]:length(full_players$year_end)) {
    if (full_players$year_end[j] != year) {
      change_row <- j
      break
    }
    if (full_players$player[j] != player) {
      change_row <- j
      break
    }
    
  }
  new_team_id <- full_players$team_id[(change_row-1)]
  full_players$team_id[rows_to_inspect[i]] <- new_team_id
  rows_to_remove <- append(rows_to_remove, c((rows_to_inspect[i]+1):(change_row-1)))
}

full_players <- full_players[-rows_to_remove,]

full_players$player <- full_players$player %>% 
  str_replace_all(c("\\*" = ""))

full_players$GB <- full_players$G - full_players$GS
full_players$Bench <- ifelse((full_players$GB > (full_players$G / 2)), "Bench", "Starter")

full_players_list <- split(full_players, full_players$year_end)

for (i in 1:length(full_players_list)) {
  full_players_list[[i]]$AdjPER <- cume_dist(full_players_list[[i]]$PER)*100
  full_players_list[[i]]$AdjWS <- cume_dist(full_players_list[[i]]$WS)*100
  full_players_list[[i]]$AdjBPM <- cume_dist(full_players_list[[i]]$BPM)*100
  full_players_list[[i]]$AdjVORP <- cume_dist(full_players_list[[i]]$VORP)*100
  full_players_list[[i]]$AdjPTS <- cume_dist(full_players_list[[i]]$PTS)*100
}

full_players <- bind_rows(full_players_list)

write.csv(full_players, "player_stats_prepped.csv", row.names = FALSE)
rm(list = ls())

## Combine player stats & salary ##

player_salary <- read.csv(file = "salary_partialprep.csv")
player_stats <- read.csv(file = "player_stats_prepped.csv")

player_salary$id <- paste0(player_salary$player, "-", player_salary$year_end, "-", player_salary$team_id)
player_stats$id <- paste0(player_stats$player, "-", player_stats$year_end, "-", player_stats$team_id)

player_info_na <- sqldf('SELECT player_stats.*, player_salary.salary, 
                          player_salary.per_cap, player_salary.sal_2020
                     FROM player_stats
                     LEFT JOIN player_salary
                     ON player_stats.id = player_salary.id')

player_info <- player_info_na[-c(which(is.na(player_info_na$salary))),]

write.csv(player_info, "player_info_prepped.csv", row.names = FALSE)
rm(list=ls())

## Further changes to player_info ##

impact_stats <- read.csv("player_info_prepped.csv")
impact_stats <- impact_stats[c(1:8,20,50,52:61)]

impact_stats <- impact_stats[-c(which(impact_stats$MP == 0)),]
impact_stats <- impact_stats[-c(which(impact_stats$USG. == 0)),]

impact_stats$sal_per_min <- (impact_stats$per_cap * 100) / impact_stats$MP
impact_stats$sal_per_WS <- (impact_stats$per_cap * 100) / impact_stats$AdjWS
impact_stats$sal_per_BPM <- (impact_stats$per_cap * 100) / impact_stats$AdjBPM
impact_stats$sal_per_PTS <- (impact_stats$per_cap * 100) / impact_stats$AdjPTS
impact_stats$sal_per_PER <- (impact_stats$per_cap * 100) / impact_stats$AdjPER
impact_stats$sal_per_VORP <- (impact_stats$per_cap * 100) / impact_stats$AdjVORP
impact_stats$sal_per_USG <- (impact_stats$per_cap * 100) / impact_stats$USG.

write.csv(impact_stats, "impact_stats.csv", row.names = FALSE)
rm(list = ls())

impact_stats <- read.csv("player_info_prepped.csv")
impact_stats <- impact_stats[c(1:8,20,50,52:61)]

impact_stats <- impact_stats[-c(which(impact_stats$MP == 0)),]
impact_stats <- impact_stats[-c(which(impact_stats$USG. == 0)),]

impact_stats$sal_per_min <- impact_stats$sal_2020 / impact_stats$MP
impact_stats$sal_per_WS <- impact_stats$sal_2020 / impact_stats$AdjWS
impact_stats$sal_per_BPM <- impact_stats$sal_2020 / impact_stats$AdjBPM
impact_stats$sal_per_PTS <- impact_stats$sal_2020 / impact_stats$AdjPTS
impact_stats$sal_per_PER <- impact_stats$sal_2020 / impact_stats$AdjPER
impact_stats$sal_per_VORP <- impact_stats$sal_2020 / impact_stats$AdjVORP
impact_stats$sal_per_USG <- impact_stats$sal_2020 / impact_stats$USG.

write.csv(impact_stats, "impact_stats_adj.csv", row.names = FALSE)
rm(list=ls())

##Combine player data into team data ##

elo <- read.csv("elo_prepped.csv")
team_salary <- read.csv("salary_prepped.csv")
impact_stats <- read.csv("impact_stats.csv")

elo$id <- paste0(elo$team_id, "-", elo$year_end)
team_salary$id <- paste0(team_salary$team_id, "-", team_salary$year_end)
impact_stats$teamID <- paste0(impact_stats$team_id, "-", impact_stats$year_end)
impact_stats$posID <- paste0(impact_stats$teamID, "-", impact_stats$pos)
impact_stats$benchID <- paste0(impact_stats$teamID, "-", impact_stats$Bench)

team_sal_elo <- sqldf('SELECT team_salary.*, elo.MaxElo
                     FROM team_salary
                     LEFT JOIN elo
                     ON team_salary.id = elo.id')

building_master_bench <- sqldf('SELECT impact_stats.teamID, impact_stats.benchID,
                                  COUNT(impact_stats.per_cap) AS ct_bench,
                                  SUM(impact_stats.per_cap) AS tot_sal
                               FROM impact_stats
                               GROUP BY impact_stats.benchID')

building_master_test <- sqldf('SELECT impact_stats.teamID, impact_stats.posID,
                                COUNT(impact_stats.per_cap) AS ct_pos,
                                SUM(impact_stats.per_cap) AS tot_sal, 
                                SUM(impact_stats.sal_per_min) AS tot_spMP, 
                                SUM(impact_stats.sal_per_WS) AS tot_spWS, 
                                SUM(impact_stats.sal_per_BPM) AS tot_spBPM, 
                                SUM(impact_stats.sal_per_PTS) AS tot_spPTS,
                                SUM(impact_stats.sal_per_PER) AS tot_spPER, 
                                SUM(impact_stats.sal_per_VORP) AS tot_spVORP, 
                                SUM(impact_stats.sal_per_USG) AS tot_spUSG,
                                AVG(impact_stats.per_cap) AS avg_sal, 
                                AVG(impact_stats.sal_per_min) AS avg_spMP, 
                                AVG(impact_stats.sal_per_WS) AS avg_spWS, 
                                AVG(impact_stats.sal_per_BPM) AS avg_spBPM, 
                                AVG(impact_stats.sal_per_PTS) AS avg_spPTS,
                                AVG(impact_stats.sal_per_PER) AS avg_spPER, 
                                AVG(impact_stats.sal_per_VORP) AS avg_spVORP, 
                                AVG(impact_stats.sal_per_USG) AS avg_spUSG,
                                MAX(impact_stats.per_cap) AS max_sal, 
                                MAX(impact_stats.sal_per_min) AS max_spMP, 
                                MAX(impact_stats.sal_per_WS) AS max_spWS, 
                                MAX(impact_stats.sal_per_BPM) AS max_spBPM, 
                                MAX(impact_stats.sal_per_PTS) AS max_spPTS,
                                MAX(impact_stats.sal_per_PER) AS max_spPER, 
                                MAX(impact_stats.sal_per_VORP) AS max_spVORP, 
                                MAX(impact_stats.sal_per_USG) AS max_spUSG,
                                MIN(impact_stats.per_cap) AS min_sal, 
                                MIN(impact_stats.sal_per_min) AS min_spMP, 
                                MIN(impact_stats.sal_per_WS) AS min_spWS, 
                                MIN(impact_stats.sal_per_BPM) AS min_spBPM, 
                                MIN(impact_stats.sal_per_PTS) AS min_spPTS,
                                MIN(impact_stats.sal_per_PER) AS min_spPER, 
                                MIN(impact_stats.sal_per_VORP) AS min_spVORP, 
                                MIN(impact_stats.sal_per_USG) AS min_spUSG,
                                STDEV(impact_stats.per_cap) AS std_sal, 
                                STDEV(impact_stats.sal_per_min) AS std_spMP, 
                                STDEV(impact_stats.sal_per_WS) AS std_spWS, 
                                STDEV(impact_stats.sal_per_BPM) AS std_spBPM, 
                                STDEV(impact_stats.sal_per_PTS) AS std_spPTS,
                                STDEV(impact_stats.sal_per_PER) AS std_spPER, 
                                STDEV(impact_stats.sal_per_VORP) AS std_spVORP, 
                                STDEV(impact_stats.sal_per_USG) AS std_spUSG
                             FROM impact_stats
                             GROUP BY impact_stats.posID')

building_master <- sqldf('SELECT impact_stats.teamID,
                            SUM(impact_stats.per_cap) AS tot_sal, 
                            SUM(impact_stats.sal_per_min) AS tot_spMP, 
                            SUM(impact_stats.sal_per_WS) AS tot_spWS, 
                            SUM(impact_stats.sal_per_BPM) AS tot_spBPM, 
                            SUM(impact_stats.sal_per_PTS) AS tot_spPTS,
                            SUM(impact_stats.sal_per_PER) AS tot_spPER, 
                            SUM(impact_stats.sal_per_VORP) AS tot_spVORP, 
                            SUM(impact_stats.sal_per_USG) AS tot_spUSG,
                            AVG(impact_stats.per_cap) AS avg_sal, 
                            AVG(impact_stats.sal_per_min) AS avg_spMP, 
                            AVG(impact_stats.sal_per_WS) AS avg_spWS, 
                            AVG(impact_stats.sal_per_BPM) AS avg_spBPM, 
                            AVG(impact_stats.sal_per_PTS) AS avg_spPTS,
                            AVG(impact_stats.sal_per_PER) AS avg_spPER, 
                            AVG(impact_stats.sal_per_VORP) AS avg_spVORP, 
                            AVG(impact_stats.sal_per_USG) AS avg_spUSG,
                            MAX(impact_stats.per_cap) AS max_sal, 
                            MAX(impact_stats.sal_per_min) AS max_spMP, 
                            MAX(impact_stats.sal_per_WS) AS max_spWS, 
                            MAX(impact_stats.sal_per_BPM) AS max_spBPM, 
                            MAX(impact_stats.sal_per_PTS) AS max_spPTS,
                            MAX(impact_stats.sal_per_PER) AS max_spPER, 
                            MAX(impact_stats.sal_per_VORP) AS max_spVORP, 
                            MAX(impact_stats.sal_per_USG) AS max_spUSG,
                            MIN(impact_stats.per_cap) AS min_sal, 
                            MIN(impact_stats.sal_per_min) AS min_spMP, 
                            MIN(impact_stats.sal_per_WS) AS min_spWS, 
                            MIN(impact_stats.sal_per_BPM) AS min_spBPM, 
                            MIN(impact_stats.sal_per_PTS) AS min_spPTS,
                            MIN(impact_stats.sal_per_PER) AS min_spPER, 
                            MIN(impact_stats.sal_per_VORP) AS min_spVORP, 
                            MIN(impact_stats.sal_per_USG) AS min_spUSG,
                            STDEV(impact_stats.per_cap) AS std_sal, 
                            STDEV(impact_stats.sal_per_min) AS std_spMP, 
                            STDEV(impact_stats.sal_per_WS) AS std_spWS, 
                            STDEV(impact_stats.sal_per_BPM) AS std_spBPM, 
                            STDEV(impact_stats.sal_per_PTS) AS std_spPTS,
                            STDEV(impact_stats.sal_per_PER) AS std_spPER, 
                            STDEV(impact_stats.sal_per_VORP) AS std_spVORP, 
                            STDEV(impact_stats.sal_per_USG) AS std_spUSG
                        FROM impact_stats
                        GROUP BY impact_stats.teamID')

for (i in 1:length(building_master$teamID)) {
  building_master$sal_C[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-C"))) == 0, "NA", building_master_test$tot_sal[c(which(
    building_master_test$posID == paste0(building_master$teamID[i], "-C")))])
  building_master$sal_PF[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-PF"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-PF")))])
  building_master$sal_PG[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-PG"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-PG")))])
  building_master$sal_SF[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-SF"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-SF")))])
  building_master$sal_SG[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-SG"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-SG")))])
  building_master$sal_Bench[i] <- ifelse(length(which(building_master_bench$benchID == paste0(
    building_master$teamID[i], "-Bench"))) == 0, "NA", building_master_bench$tot_sal[c(which(
      building_master_bench$benchID == paste0(building_master$teamID[i], "-Bench")))])
  building_master$sal_Start[i] <- ifelse(length(which(building_master_bench$benchID == paste0(
    building_master$teamID[i], "-Starter"))) == 0, "NA", building_master_bench$tot_sal[c(which(
      building_master_bench$benchID == paste0(building_master$teamID[i], "-Starter")))])
}



closer_to_master <- sqldf('SELECT team_sal_elo.year_end, team_sal_elo.team_id, team_sal_elo.fran_id, 
                            team_sal_elo.id, team_sal_elo.MaxElo, building_master.*
                          FROM team_sal_elo
                          LEFT JOIN building_master
                          ON team_sal_elo.id = building_master.teamID')

master <- closer_to_master[-c(1:3, 6)]
master <- master[-c(which(is.na(master$MaxElo))),]

write.csv(master, "master.csv", row.names = FALSE)
rm(list=ls())

elo <- read.csv("elo_prepped.csv")
team_salary <- read.csv("salary_prepped.csv")
impact_stats_adj <- read.csv("impact_stats_adj.csv")

elo$id <- paste0(elo$team_id, "-", elo$year_end)
team_salary$id <- paste0(team_salary$team_id, "-", team_salary$year_end)
impact_stats_adj$teamID <- paste0(impact_stats_adj$team_id, "-", impact_stats_adj$year_end)
impact_stats_adj$posID <- paste0(impact_stats_adj$teamID, "-", impact_stats_adj$pos)
impact_stats_adj$benchID <- paste0(impact_stats_adj$teamID, "-", impact_stats_adj$Bench)

team_sal_elo <- sqldf('SELECT team_salary.*, elo.MaxElo
                     FROM team_salary
                     LEFT JOIN elo
                     ON team_salary.id = elo.id')

building_master_bench <- sqldf('SELECT impact_stats_adj.teamID, impact_stats_adj.benchID,
                                  COUNT(impact_stats_adj.sal_2020) AS ct_bench,
                                  SUM(impact_stats_adj.sal_2020) AS tot_sal
                               FROM impact_stats_adj
                               GROUP BY impact_stats_adj.benchID')

building_master_test <- sqldf('SELECT impact_stats_adj.teamID, impact_stats_adj.posID,
                                COUNT(impact_stats_adj.sal_2020) AS ct_pos,
                                SUM(impact_stats_adj.sal_2020) AS tot_sal, 
                                SUM(impact_stats_adj.sal_per_min) AS tot_spMP, 
                                SUM(impact_stats_adj.sal_per_WS) AS tot_spWS, 
                                SUM(impact_stats_adj.sal_per_BPM) AS tot_spBPM, 
                                SUM(impact_stats_adj.sal_per_PTS) AS tot_spPTS,
                                SUM(impact_stats_adj.sal_per_PER) AS tot_spPER, 
                                SUM(impact_stats_adj.sal_per_VORP) AS tot_spVORP, 
                                SUM(impact_stats_adj.sal_per_USG) AS tot_spUSG,
                                AVG(impact_stats_adj.sal_2020) AS avg_sal, 
                                AVG(impact_stats_adj.sal_per_min) AS avg_spMP, 
                                AVG(impact_stats_adj.sal_per_WS) AS avg_spWS, 
                                AVG(impact_stats_adj.sal_per_BPM) AS avg_spBPM, 
                                AVG(impact_stats_adj.sal_per_PTS) AS avg_spPTS,
                                AVG(impact_stats_adj.sal_per_PER) AS avg_spPER, 
                                AVG(impact_stats_adj.sal_per_VORP) AS avg_spVORP, 
                                AVG(impact_stats_adj.sal_per_USG) AS avg_spUSG,
                                MAX(impact_stats_adj.sal_2020) AS max_sal, 
                                MAX(impact_stats_adj.sal_per_min) AS max_spMP, 
                                MAX(impact_stats_adj.sal_per_WS) AS max_spWS, 
                                MAX(impact_stats_adj.sal_per_BPM) AS max_spBPM, 
                                MAX(impact_stats_adj.sal_per_PTS) AS max_spPTS,
                                MAX(impact_stats_adj.sal_per_PER) AS max_spPER, 
                                MAX(impact_stats_adj.sal_per_VORP) AS max_spVORP, 
                                MAX(impact_stats_adj.sal_per_USG) AS max_spUSG,
                                MIN(impact_stats_adj.sal_2020) AS min_sal, 
                                MIN(impact_stats_adj.sal_per_min) AS min_spMP, 
                                MIN(impact_stats_adj.sal_per_WS) AS min_spWS, 
                                MIN(impact_stats_adj.sal_per_BPM) AS min_spBPM, 
                                MIN(impact_stats_adj.sal_per_PTS) AS min_spPTS,
                                MIN(impact_stats_adj.sal_per_PER) AS min_spPER, 
                                MIN(impact_stats_adj.sal_per_VORP) AS min_spVORP, 
                                MIN(impact_stats_adj.sal_per_USG) AS min_spUSG,
                                STDEV(impact_stats_adj.sal_2020) AS std_sal, 
                                STDEV(impact_stats_adj.sal_per_min) AS std_spMP, 
                                STDEV(impact_stats_adj.sal_per_WS) AS std_spWS, 
                                STDEV(impact_stats_adj.sal_per_BPM) AS std_spBPM, 
                                STDEV(impact_stats_adj.sal_per_PTS) AS std_spPTS,
                                STDEV(impact_stats_adj.sal_per_PER) AS std_spPER, 
                                STDEV(impact_stats_adj.sal_per_VORP) AS std_spVORP, 
                                STDEV(impact_stats_adj.sal_per_USG) AS std_spUSG
                             FROM impact_stats_adj
                             GROUP BY impact_stats_adj.posID')

building_master <- sqldf('SELECT impact_stats_adj.teamID,
                            SUM(impact_stats_adj.sal_2020) AS tot_sal, 
                            SUM(impact_stats_adj.sal_per_min) AS tot_spMP, 
                            SUM(impact_stats_adj.sal_per_WS) AS tot_spWS, 
                            SUM(impact_stats_adj.sal_per_BPM) AS tot_spBPM, 
                            SUM(impact_stats_adj.sal_per_PTS) AS tot_spPTS,
                            SUM(impact_stats_adj.sal_per_PER) AS tot_spPER, 
                            SUM(impact_stats_adj.sal_per_VORP) AS tot_spVORP, 
                            SUM(impact_stats_adj.sal_per_USG) AS tot_spUSG,
                            AVG(impact_stats_adj.sal_2020) AS avg_sal, 
                            AVG(impact_stats_adj.sal_per_min) AS avg_spMP, 
                            AVG(impact_stats_adj.sal_per_WS) AS avg_spWS, 
                            AVG(impact_stats_adj.sal_per_BPM) AS avg_spBPM, 
                            AVG(impact_stats_adj.sal_per_PTS) AS avg_spPTS,
                            AVG(impact_stats_adj.sal_per_PER) AS avg_spPER, 
                            AVG(impact_stats_adj.sal_per_VORP) AS avg_spVORP, 
                            AVG(impact_stats_adj.sal_per_USG) AS avg_spUSG,
                            MAX(impact_stats_adj.sal_2020) AS max_sal, 
                            MAX(impact_stats_adj.sal_per_min) AS max_spMP, 
                            MAX(impact_stats_adj.sal_per_WS) AS max_spWS, 
                            MAX(impact_stats_adj.sal_per_BPM) AS max_spBPM, 
                            MAX(impact_stats_adj.sal_per_PTS) AS max_spPTS,
                            MAX(impact_stats_adj.sal_per_PER) AS max_spPER, 
                            MAX(impact_stats_adj.sal_per_VORP) AS max_spVORP, 
                            MAX(impact_stats_adj.sal_per_USG) AS max_spUSG,
                            MIN(impact_stats_adj.sal_2020) AS min_sal, 
                            MIN(impact_stats_adj.sal_per_min) AS min_spMP, 
                            MIN(impact_stats_adj.sal_per_WS) AS min_spWS, 
                            MIN(impact_stats_adj.sal_per_BPM) AS min_spBPM, 
                            MIN(impact_stats_adj.sal_per_PTS) AS min_spPTS,
                            MIN(impact_stats_adj.sal_per_PER) AS min_spPER, 
                            MIN(impact_stats_adj.sal_per_VORP) AS min_spVORP, 
                            MIN(impact_stats_adj.sal_per_USG) AS min_spUSG,
                            STDEV(impact_stats_adj.sal_2020) AS std_sal, 
                            STDEV(impact_stats_adj.sal_per_min) AS std_spMP, 
                            STDEV(impact_stats_adj.sal_per_WS) AS std_spWS, 
                            STDEV(impact_stats_adj.sal_per_BPM) AS std_spBPM, 
                            STDEV(impact_stats_adj.sal_per_PTS) AS std_spPTS,
                            STDEV(impact_stats_adj.sal_per_PER) AS std_spPER, 
                            STDEV(impact_stats_adj.sal_per_VORP) AS std_spVORP, 
                            STDEV(impact_stats_adj.sal_per_USG) AS std_spUSG
                        FROM impact_stats_adj
                        GROUP BY impact_stats_adj.teamID')

for (i in 1:length(building_master$teamID)) {
  building_master$sal_C[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-C"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-C")))])
  building_master$sal_PF[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-PF"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-PF")))])
  building_master$sal_PG[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-PG"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-PG")))])
  building_master$sal_SF[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-SF"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-SF")))])
  building_master$sal_SG[i] <- ifelse(length(which(building_master_test$posID == paste0(
    building_master$teamID[i], "-SG"))) == 0, "NA", building_master_test$tot_sal[c(which(
      building_master_test$posID == paste0(building_master$teamID[i], "-SG")))])
  building_master$sal_Bench[i] <- ifelse(length(which(building_master_bench$benchID == paste0(
    building_master$teamID[i], "-Bench"))) == 0, "NA", building_master_bench$tot_sal[c(which(
      building_master_bench$benchID == paste0(building_master$teamID[i], "-Bench")))])
  building_master$sal_Start[i] <- ifelse(length(which(building_master_bench$benchID == paste0(
    building_master$teamID[i], "-Starter"))) == 0, "NA", building_master_bench$tot_sal[c(which(
      building_master_bench$benchID == paste0(building_master$teamID[i], "-Starter")))])
}



closer_to_master <- sqldf('SELECT team_sal_elo.year_end, team_sal_elo.team_id, team_sal_elo.fran_id, 
                            team_sal_elo.id, team_sal_elo.MaxElo, building_master.*
                          FROM team_sal_elo
                          LEFT JOIN building_master
                          ON team_sal_elo.id = building_master.teamID')

master <- closer_to_master[-c(1:3, 6)]
master <- master[-c(which(is.na(master$MaxElo))),]

write.csv(master, "master_adj.csv", row.names = FALSE)
rm(list=ls())






