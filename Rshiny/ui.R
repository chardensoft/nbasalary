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


ui <- shinyUI(
  fluidPage(
    
    titlePanel("Predicting Max Elo Rating using NBA Salaries"),
    
    # "Select a Player",
    fluidRow(selectizeInput(inputId = "teamID",
                            label = "Select Team",
                            choices = master$id)),
    tableOutput("rTeam"),
    plotOutput("plot"),
    sliderInput(inputId = "bins",
                label = "Number of bins:",
                min = 1,
                max = 30,
                value = 8),
    plotOutput("hist1"),
    plotOutput("hist2")
  ))