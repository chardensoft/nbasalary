# NBA stats explanation

## Authors: Bradley Brown, Andrew Cannon, Isaac Corcoran, Jordan Khamvongsouk, Chris Harden, Felicia Seng

## Player_stats.ipynb, Nba_salary.ipynb, Scraping_ESPN.ipynb
These files scrape public website in order to compile most of the original data we have in this folder. Past_salaries.csv, Player_stats.csv are the main two files created from scraping. In addition, PracticeData.xlsx, elo_rating.xlsx and cap.csv were created by hand, while elo.csv was obtained from the 538 github for Elo rating (see Resources). Currently if you run these files, it will recreate the data contained in Past_salaries.csv and Player_stats.csv.

## FirstVersion.R, V2.R, NBA_Salary Assumptions.R
These files contain the code for the first few versions of our model when we were just practicing with basic data and working on getting out the kinks. They also contain some code for testing assumptions and slightly modifying the data set we were working with. Currently if you run these, they won't do much. 

## DataPrep.R
This file is essentially where all the data prep is done in order to create master.csv. Along the way, elo_partialprep.csv, elo_prepped.csv, impact_stats.csv, impact_stats_adj.csv, master_adj.csv, player_info_prepped.csv, player_stats_prepped.csv, salary_partialprep.csv and salary_prepped.csv are all created and saved. Finally master.csv is created with the full database of max Elo ratings and all the explanatory variables we compiled. Currently, if you run this file start to finish, it will duplicate and write over all the above listed csv files.

## traintest.R
This file contains our final code for the models we created, some Exploratory Data Analysis code and then the test for our final models to see how they performed. This code also produces the files rf_model.rds and lin_model.rds for later use. You can read through some of the commented code for insights on how we went about working with the data, but these are mostly noted in our final report. If you run this code from start to finish as is, it will create the linear and random forest models for you with their test results. You can also run this file on any file with similar data as it is adaptive.