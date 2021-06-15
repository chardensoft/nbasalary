##R Shiny App
library(shiny)
library(tidyverse)
library(ggrepel)
library(rsconnect)
library(randomForest)
library(DT)
library(lubridate)
library(scales)

rmod <- readRDS("rf_model.rds")
lmod <- readRDS("lin_model.rds")
master <- read.csv("master_clean.csv")
elo <- read.csv("elo_partialprep.csv")

master$prediction <-  round(predict(rmod, master), 3)
master$diff <- master$MaxElo - master$prediction
for (i in 1:length(master$prediction)) {
  if (abs(round(master$diff[i], 0)) <= 100) {
    master$in_range[i] <- "yes"
  } else {
    master$in_range[i] <- "no"
  }
  if (master$MaxElo[i] >= 1775) {
    master$category[i] <- "Historically Great"
  } else if (master$MaxElo[i] >= 1675) {
    master$category[i] <- "Title Contender"
  } else if (master$MaxElo[i] >= 1550) {
    master$category[i] <- "Playoff Bound"
  } else if (master$MaxElo[i] >= 1450) {
    master$category[i] <- "Average Team"
  } else if (master$MaxElo[i] >= 1350) {
    master$category[i] <- "Lottery Bound"
  } else if (master$MaxElo[i] >= 1250) {
    master$category[i] <- "Really Terrible"
  } else {
    master$category[i] <- "Hisorically Awful"
  }
  
  if (master$prediction[i] >= 1775) {
    master$cat_predict[i] <- "Historically Great"
  } else if (master$prediction[i] >= 1675) {
    master$cat_predict[i] <- "Title Contender"
  } else if (master$prediction[i] >= 1550) {
    master$cat_predict[i] <- "Playoff Bound"
  } else if (master$prediction[i] >= 1450) {
    master$cat_predict[i] <- "Average Team"
  } else if (master$prediction[i] >= 1350) {
    master$cat_predict[i] <- "Lottery Bound"
  } else if (master$prediction[i] >= 1250) {
    master$cat_predict[i] <- "Really Terrible"
  } else {
    master$cat_predict[i] <- "Hisorically Awful"
  }
  
}

master <- master %>% map_df(rev)

server <- shinyServer(function(input, output, session) {
  
  output$rTeam <- renderTable({
    statsTable <- data.frame(cbind(Name=master$id, "Total_Salary_Cap"=paste0(round(100*master$tot_sal, 2), "%"),
                                   "Total_Cap_per_VORP"=paste0(round(100*master$tot_spVORP, 2), "%"), 
                                   "Predicted_Max_Elo_Rating"=round(master$prediction, 1),
                                   "Actual_Max_Elo_Rating"=round(master$MaxElo, 0),
                                   "In_Range"=toupper(master$in_range),
                                   "Predicted_Category"=master$cat_predict,
                                   "Actual_Category"=master$category
    ))
    statsTable1 <- statsTable %>% 
      filter(Name == input$teamID)
    # predTable <- data.frame(cbind(InNBA=ifelse(mod_res$reality==1,"no","yes"),
    #                               ChanceOfNBA=mod_res$yes_nba,
    #                               player=mod_res$player))
    # if (!(input$playerR %in% mod_res$player)) {
    #   NULL
    # } else {
    #   statsTable1 <- statsTable %>% 
    #     filter(Name == input$playerR)
    #   predTable1 <- predTable %>% 
    #     filter(player==input$playerR) %>% 
    #     select(InNBA, ChanceOfNBA)
    #   predTable1$ChanceOfNBA <- paste0(as.numeric(predTable1$ChanceOfNBA)*100, "%")
    #   data.frame(cbind(statsTable1, predTable1))
    # }
  })
  
  output$plot <- renderPlot({
    data <- elo %>% 
      filter(team_id == substr(input$teamID, 1, 3)) %>% 
      filter(year_id == substr(input$teamID, 5, 8))
    max_point <- data$game_id[which(data$elo_n == max(data$elo_n))]
    yr_minus <- as.character(as.numeric(substr(input$teamID, 7, 8)) - 1)
    
    ggplot(data, mapping = aes(x = as.Date(date_game, format="%m/%d/%y"), y = elo_n)) +
      geom_point(color = "blue") +
      geom_line(size = 1, color = "blue") +
      geom_point(data = data[which(data$game_id == max_point),], pch = 21, fill = NA, size = 4,
                 color = "red", stroke = 1) +
      # geom_point(shape = 21, colour = "blue", size = 1, fill = NA,
      #            alpha = .7, stroke = .5) + 
      # geom_point(data = earnxstat.valid[earnxstat.valid$name == test, ], 
      #            shape = 21, fill = NA, size = 1,
      #            color = "red", stroke = .5) +
      # geom_label_repel(data = earnxstat.valid[earnxstat.valid$name == test, ], 
      #                  aes(label = name), 
      #                  box.padding   = 0.3, 
      #                  point.padding = 0.5,
      #                  col = "red",
      #                  segment.color = 'red') + 
    # geom_vline(xintercept = mean(earnxstat.valid$fullbox), linetype = "dashed", col = "purple") + 
    # scale_x_date(date_breaks = "1 month", 
    #              labels = date_format("%m/%d/%y"),
    #              limits = as.Date(c(min_date, max_date))) +
    # scale_y_continuous(limits = c(-100, 1200),                  
    #                    breaks = seq(-100, 1200, by = 100)) + 
    theme_bw() +
      labs(title = paste0("Elo Rating Over the ", substr(input$teamID, 5, 6), yr_minus, "-", 
                          substr(input$teamID, 7, 8), " Season for ", substr(input$teamID, 1, 3)), 
           subtitle = "Max Elo Circled in Red") +
      xlab("Date") +
      ylab("Elo Rating")
  })
  
  output$hist1 <- renderPlot({
    data <- master %>% 
      filter(substr(id, 5, 8) == substr(input$teamID, 5, 8))
    team <- data %>% 
      filter(id == input$teamID)
    
    ggplot(data, aes(x = tot_sal)) +
      geom_histogram(color = "black", fill = "lightblue", bins = input$bins) + 
      geom_vline(xintercept = team$tot_sal, color = "red") + 
      theme_bw() +
      labs(title = paste0("Histogram of Total Salaries in ", substr(input$teamID, 5, 8), " across the League"), 
           subtitle = paste0(substr(input$teamID, 1, 3), " Shown in Red")) +
      xlab("Total Salary Caps by Team") +
      ylab("Teams in Bucket")
    
  })
  
  output$hist2 <- renderPlot({
    data <- master %>% 
      filter(substr(id, 5, 8) == substr(input$teamID, 5, 8))
    team <- data %>% 
      filter(id == input$teamID)
    
    ggplot(data, aes(x = tot_spVORP)) +
      geom_histogram(color = "black", fill = "lightblue", bins = input$bins) + 
      geom_vline(xintercept = team$tot_spVORP, color = "red") + 
      theme_bw() +
      labs(title = paste0("Histogram of Cap Spent per VORP in ", substr(input$teamID, 5, 8), " across the League"), 
           subtitle = paste0(substr(input$teamID, 1, 3), " Shown in Red")) +
      xlab("Cap per VORP by Team") +
      ylab("Teams in Bucket")
    
  })
  
})
